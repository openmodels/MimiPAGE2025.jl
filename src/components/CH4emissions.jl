using Statistics

include("../utils/country_tools.jl")

@defcomp ch4emissions begin
    region = Index()
    country = Index()

    model = Parameter{Model}()
    gains_match = Parameter{Bool}()
    e_regionalCH4emissions_gains = Parameter(index=[time, country], unit="kt/person/year")

    e0_baselineCH4emissions = Parameter(index=[country], unit="Mtonne/year")
    e0_baselineCH4emissions_region = Variable(index=[region], unit="Mtonne/year")

    er_CH4emissionsgrowth_region = Parameter(index=[time, region], unit="%")

    e_regionalCH4emissions_region = Variable(index=[time, region], unit="Mtonne/year")
    e_regionalCH4emissions = Variable(index=[time, country], unit="Mtonne/year")

    e_globalCH4emissions = Variable(index=[time], unit="Mtonne/year")

    # read in counterfactual GDP in absence of growth effects (gdp_baseline) and actual GDP
    gdppc = Parameter(index=[time, country], unit="\$/person")
    pop_population = Parameter(index=[time, country], unit="million person")
    gdp_baseline = Parameter(index=[time, country], unit="\$M")
    emfeed_emissionfeedback = Parameter{Bool}(unit="none", default=true)

    function init(pp, vv, dd)
        vv.e0_baselineCH4emissions_region[:] = countrytoregion(pp.model, sum, pp.e0_baselineCH4emissions)
    end

    function run_timestep(p, v, d, t)
        if p.gains_match
            for cc in d.country
                v.e_regionalCH4emissions[t, cc] = p.e_regionalCH4emissions_gains[t, cc] * p.pop_population[t, cc] * 1e6 / 1e3
            end
            v.e_regionalCH4emissions_region[t, :] = countrytoregion(p.model, sum, v.e_regionalCH4emissions[t, :])
        else
            # eq.4 in Hope (2006) - regional CH4 emissions as % change from baseline
            for r in d.region
                v.e_regionalCH4emissions_region[t,r] = p.er_CH4emissionsgrowth_region[t,r] * v.e0_baselineCH4emissions_region[r] / 100
            end

            er_CH4emissionsgrowth = regiontocountry(p.model, p.er_CH4emissionsgrowth_region[t, :])

            for cc in d.country
                v.e_regionalCH4emissions[t, cc] = er_CH4emissionsgrowth[cc] * p.e0_baselineCH4emissions[cc] / 100

                # rescale emissions based on GDP deviation from original scenario pathway
                if !is_first(t) && p.emfeed_emissionfeedback
                    v.e_regionalCH4emissions[t, cc] = v.e_regionalCH4emissions[t, cc] * (p.gdppc[t-1, cc] * p.pop_population[t-1, cc] / p.gdp_baseline[t-1, cc])
                end
            end
        end

        # eq. 5 in Hope (2006) - global CH4 emissions are sum of regional emissions
        v.e_globalCH4emissions[t] = sum(v.e_regionalCH4emissions[t, :])
    end
end

function addch4emissions(model::Model, use_gains_ch4::Bool, gains_scenario::Symbol)
    ch4emit = add_comp!(model, ch4emissions)

    ch4emit[:model] = model
    baselineemits = CSV.read(pagedata("climate/ch4emit.csv"), DataFrame)
    baselineemits2 = DataFrame(iso=baselineemits[!, 1], value=vec(mean(Matrix(baselineemits[!, 2:12]), dims=2)) / 1000) # Convert kt to Mt
    e0_baselineCH4emissions = readcountrydata_i_const(model, baselineemits2, :iso, :value, sum; allowmissing=true)
    e0_baselineCH4emissions[ismissing.(e0_baselineCH4emissions)] .= 0
    ch4emit[:e0_baselineCH4emissions] = convert(Vector{Float64}, e0_baselineCH4emissions)

    gains_ch4 = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))

    if use_gains_ch4
        baseline = CSV.read(pagedata("pollution/baseline.csv"), DataFrame)
        baseline.EMIS_CH4_KT_percap = baseline.EMIS_CH4_KT ./ baseline.POPULATION
        baseline2 = leftjoin(gains_mapping, baseline, on=:REGION_4LETTER)
        baseline2 = baseline2[.!ismissing.(baseline2.IDYEARS), :]

        baseline_after2050 = baseline2[(baseline2.IDYEARS .== 2050) .& (baseline2.IDSCENARIOS .== String(gains_scenario)), :]
        baseline_after2050_page = leftjoin(DataFrame(ISO3=dim_keys(model, :country)), baseline_after2050, on=:ISO3)
        for tt in 1:dim_count(model, :time)
            if dim_keys(model, :time)[tt] >= 2050
                baseline_page = baseline_after2050_page
            else
                if dim_keys(model, :time)[tt] < 2030
                    baseline_period = baseline2[(baseline2.IDYEARS .== dim_keys(model, :time)[tt]) .& (baseline2.IDSCENARIOS .== replace(String(gains_scenario), "APC" => "CLE")), :]
                else
                    baseline_period = baseline2[(baseline2.IDYEARS .== dim_keys(model, :time)[tt]) .& (baseline2.IDSCENARIOS .== String(gains_scenario)), :]
                end
                baseline_page = leftjoin(DataFrame(ISO3=dim_keys(model, :country)), baseline_period, on=:ISO3)
            end

            baseline_page.EMIS_CH4_KT_percap[ismissing.(baseline_page.EMIS_CH4_KT_percap)] .= mean(skipmissing(baseline_page.EMIS_CH4_KT_percap))
            gains_ch4[tt, :] = baseline_page.EMIS_CH4_KT_percap
        end
    end

    ch4emit[:gains_match] = use_gains_ch4
    ch4emit[:e_regionalCH4emissions_gains] = gains_ch4

    return ch4emit
end
