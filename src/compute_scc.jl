page_years = [2020, 2030, 2040, 2050, 2075, 2100, 2150, 2200, 2250, 2300]
page_year_0 = 2015

function getpageindexfromyear(year)
    i = findfirst(isequal(year), page_years)
    if i == 0
        error("Invalid PAGE year: $year.")
    end
    return i
end

function getperiodlength(year)      # same calculations made for yagg_periodspan in the model
    i = getpageindexfromyear(year)

    if year == page_years[1]
        start_year = page_year_0
    else
        start_year = page_years[i - 1]
    end

    if year == page_years[end]
        last_year = page_years[end]
    else
        last_year = page_years[i + 1]
    end

    return (last_year - start_year) / 2
end

"""
Applies undiscounting factor to get the SCC, discounted to the emissions year instead of the base year.
"""
function undiscount_scc(m::Model, year::Int, cc_focus::Int)
    df = m[:EquityWeighting, :df_utilitydiscountfactor]
    consfocus0 = m[:GDP, :cons_percap_consumption_0][cc_focus]
    consfocus = m[:GDP, :cons_percap_consumption][:, cc_focus]
    emuc = m[:EquityWeighting, :emuc_utilityconvexity]
    sccii = getpageindexfromyear(year)

    return df[sccii] * ((consfocus[sccii] / consfocus0)^-emuc)
end

@defcomp ExtraEmissions begin
    e_globalCO2emissions = Parameter(index=[time], unit="Mtonne/year")
    pulse_size = Parameter(unit="Mtonne CO2")
    pulse_year = Parameter()
    e_globalCO2emissions_adjusted = Variable(index=[time], unit="Mtonne/year")

    function run_timestep(p, v, d, t)
        if gettime(t) == p.pulse_year
            # pulse is applied to the years around this year, as a triangular distribution
            v.e_globalCO2emissions_adjusted[t] = p.e_globalCO2emissions[t] + p.pulse_size / getperiodlength(p.pulse_year)
        else
            v.e_globalCO2emissions_adjusted[t] = p.e_globalCO2emissions[t]
        end
    end
end

"""
    compute_scc(
        m::Model = get_model();
        year::Union{Int, Nothing} = nothing,
        eta::Union{Float64, Nothing} = nothing,
        prtp::Union{Float64, Nothing} = nothing,
        equity_weighting::Bool = true,
        pulse_size = 100_000.,
        n::Union{Int,Nothing}=nothing,
        trials_output_filename::Union{String, Nothing} = nothing,
        seed::Union{Int, Nothing} = nothing)

Computes the social cost of CO2 for an emissions pulse in `year` for the provided the model `m`.
Returns a NamedTuple (scc, scc_disaggregated, mm) of the social cost of carbon and the MarginalModel used to compute it.
If no model is provided, the default model from get_model() is used.
Units of the returned value are \$ per metric tonne of CO2.

The discounting scheme can be specified by the `eta` and `prtp` parameters, which will update the values of emuc_utilitiyconvexity
and ptp_timepreference in the model. If no values are provided, the discount factors will be computed using the default
PAGE values of emuc_utilitiyconvexity=1.1666666667 and ptp_timepreference=1.0333333333.

The size of the marginal emission defaults to 100_000 metric megatonnes of CO2 (Mtonne CO2), and this
pulse can be modified with the `pulse_size` keyword argument, in metric megatonnes of CO2 (Mtonne CO2)
(this does not change the units of the returned value, which is always normalized by the
`pulse_size` used). A pulse in `year` is actually a gradual increase throughout the timestep preceeding `year`,
followed by a gradual decrease in emissions in the timestep superseeding `year`. Emissions are linearly interpolated
between the points given by the years.

By default, `n = nothing`, and a single value for the "best guess" social cost of CO2 is returned. If a positive
value for keyword `n` is specified, then a Monte Carlo simulation with sample size `n` will run, sampling from
all of PAGE's random variables, and a vector of `n` social cost values will be returned.
Optionally providing a CSV file path to `trials_output_filename` will save all of the sampled trial data as a CSV file.
Optionally providing a `seed` value will set the random seed before running the simulation, allowing the
results to be replicated.
"""
function compute_scc(
        m::Model=get_model();
        year::Union{Int,Nothing}=nothing,
        prefrange::Bool=true,
        equity_weighting::Bool=true,
        pulse_size=75000.,
        n::Union{Int,Nothing}=nothing,
        trials_output_filename::Union{String,Nothing}=nothing,
        seed::Union{Int,Nothing}=nothing
        )

    year === nothing ? error("Must specify an emission year. Try `compute_scc(m, year=2020)`.") : nothing
    !(year in page_years) ? error("Cannot compute the scc for year $year, year must be within the model's time index $page_years.") : nothing

    if !equity_weighting
        try
            set_param!(m, :equity_proportion, 0)
        catch e
            update_param!(m, :equity_proportion, 0)
        end
    end

    run(m)
    cc_focus = argmin(abs.(m[:GDP, :cons_percap_consumption_0] .- median(m[:GDP, :cons_percap_consumption_0])))

    # note here that we use `pulse_size` as the `delta` keyword argument for
    # the marginal model so we can normalize to $ per ton
    mm = get_marginal_model(m, year=year, pulse_size=pulse_size)   # Returns a marginal model that has already been run

    if n === nothing
        # Run the "best guess" social cost calculation
        run(mm)
        scc = mm[:EquityWeighting, :td_totaldiscountedimpacts] / undiscount_scc(mm.base, year, cc_focus)
        tds = getdataframe(mm, :CountryLevelNPV, :td_totaldiscountedimpacts)
        tds[!, :scc] = tds[!, :td_totaldiscountedimpacts] / undiscount_scc(mm.base, year, cc_focus)
        scc_disaggregated = tds
    elseif n < 1
        error("Invalid `n` value, only values >=1 allowed.")
    else
        # Run a Monte Carlo simulation

        simdef = getsim(m)   # get the default simulation, need to remove :emuc_utilityconvexity and :ptp_timepreference RVs if user specified values for these
        if !prefrange
            Mimi.delete_transform!(simdef, :pref_draw)
        end

        seed !== nothing ? Random.seed!(seed) : nothing

        # Setup of location of final results
        scc_results = zeros(n)
        scc_disaggregated_results = []

        function mc_scc_calculation(sim_inst::SimulationInstance, trialnum::Int, ntimesteps::Int, ignore::Nothing)
            marginal = sim_inst.models[1]
            marg_damages = marginal[:EquityWeighting, :td_totaldiscountedimpacts] / undiscount_scc(mm.base, year, cc_focus)
            scc_results[trialnum] = marg_damages
            tds = getdataframe(marginal, :CountryLevelNPV, :td_totaldiscountedimpacts)
            tds[!, :scc] = tds[!, :td_totaldiscountedimpacts] / undiscount_scc(mm.base, year, cc_focus)
            push!(scc_disaggregated_results, tds)
        end

        si = run(simdef, mm, n, trials_output_filename=trials_output_filename, post_trial_func=mc_scc_calculation)
        scc = scc_results
        scc_disaggregated = vcat(scc_disaggregated_results...)
    end

    return (scc=scc, scc_disaggregated=scc_disaggregated, mm=mm)
end

# # Helper function for removing a random variable by its parameter name in the model.
# #   Their random variable names have been appended with a unique number by Mimi,
# #   so need to look them up this way to find the RV name in the simulation definition
# #   in order to use the Mimi.delete_RV! function.
# #   If it is an array variable, it will be represented by multiple RVs; this deletes them all.
# function _remove_RV!(simdef, _name)
#     all_rv_names = collect(keys(simdef.rvdict))
#     rv_names = all_rv_names[findall(startswith(String(_name)), map(String, all_rv_names))]
#     [Mimi.delete_RV!(simdef, rv_name) for rv_name in rv_names]
# end


"""
    get_marginal_model(m::Model = get_model(); year::Union{Int, Nothing} = nothing, pulse_size = 100000.)

Returns a Mimi MarginalModel where the provided m is the base model, and the
marginal model has additional emissions of CO2 in year `year`. If no Model m is
provided, the default model from MimiPAGE2020.get_model() is used as the base model.
Note that the returned MarginalModel has already been run. The `pulse_size` defaults
to 100_000 metric megatonnes of CO2 (Mtonne CO2), and is spread over all years within the
period following `year`.
"""
function get_marginal_model(m::Model=get_model(); year::Union{Int,Nothing}=nothing, pulse_size=75000.)
    year === nothing ? error("Must specify an emission year. Try `get_marginal_model(m, year=2020)`.") : nothing
    !(year in page_years) ? error("Cannot add marginal emissions in $year, year must be within the model's time index $page_years.") : nothing

    # note here that we use `pulse_size` as the `delta` keyword argument for
    # the marginal model so we can normalize to $ per ton
    mm = create_marginal_model(m, pulse_size)

    add_comp!(mm.modified, ExtraEmissions, :extra_emissions; after=:co2emissions)
    connect_param!(mm.modified, :extra_emissions => :e_globalCO2emissions, :co2emissions => :e_globalCO2emissions)
    set_param!(mm.modified, :extra_emissions, :pulse_size, pulse_size)
    set_param!(mm.modified, :extra_emissions, :pulse_year, year)

    connect_param!(mm.modified, :CO2Cycle => :e_globalCO2emissions, :extra_emissions => :e_globalCO2emissions_adjusted)

    run(mm)
    return mm
end
