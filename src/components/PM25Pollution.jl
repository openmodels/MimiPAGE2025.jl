# ==============================================================================
# PM2.5 Pollution Component
#
# This component estimates anthropogenic PM2.5 concentrations resulting from
# greenhouse gas emissions and socioeconomic factors.
#
#  SELF Contribution:
#   - PM2.5 concentration within a region attributable to its own emissions.
#
#  EXPORT Contribution:
#   - Total PM2.5 concentration observed in other regions attributable
#     to exported (transboundary) emissions.
#
# Coefficients come from 1000 Monte Carlo draws.
# ==============================================================================

using DataFrames
using CSV
using Interpolations

include("../utils/interpol.jl")
include("../utils/gains.jl")

gains_mapping = CSV.read(pagedata("pollution/GAINS_4letter_regions_mapping.csv"), DataFrame)

@defcomp PM25Pollution begin
    country = Index()
    time = Index()

    y_year_0 = Parameter(unit="year")
    y_year = Parameter(index=[time], unit="year")

    # === Input Predictors ===
    e_countryCO2emissions = Parameter(index=[time, country], unit="Mtonne/year")
    e_countryCH4emissions = Parameter(index=[time, country], unit="Mtonne/year")

    gdp = Parameter(index=[time, country], unit="million US\$2005/yr")
    gdp0_initgdp = Parameter(index=[country], unit="\$M")

    pop_population = Parameter(index=[time, country], unit="million person")
    pop0_initpopulation = Parameter(index=[country], unit="million person")

    control_factor = Parameter(default=1.) # > 1 to increase control action

    # === Draw Selector ===
    pm25_draw = Parameter{Int}()  # 0 = use mean of draws, 1–1000 = use a specific draw
    scenario_suffix = Parameter{String}()
    use_econ = Parameter{Bool}()
    use_extrap = Parameter{Bool}()
    use_co2 = Parameter{Bool}()
    gainsmatch = Parameter{Bool}()

    yearfe_self = Variable(index=[time])
    trendfe_self = Variable(index=[country])
    yearfe_export = Variable(index=[time])
    trendfe_export = Variable(index=[country])

    baseline_year = Parameter(index=[time])
    baseline_co2 = Parameter(index=[time], unit="Mtonne/year")
    baseline_ch4 = Parameter(index=[time], unit="Mtonne/year")
    baseline_gdp = Parameter(index=[time, country], unit="MEUR2015")
    baseline_gdppc = Parameter(index=[time, country], unit="\$/person")
    # baseline_costs = Parameter(index=[time, country], unit="million EUR")
    baseline_pm25_total = Parameter(index=[time, country]) # Only used under gainsmatch
    baseline_pm25_self = Parameter(index=[time, country])
    baseline_pm25_export = Parameter(index=[time, country])

    export_pattern = Parameter(index=[country, country]) # sink x source

    ekc_loggdppc_coeff = Parameter(default=0)
    ekc_loggdppc2_coeff = Parameter(default=0)

    # === Regression Coefficients (SELF) ===
    β_self_co2         = Variable()
    β_self_ch4         = Variable()
    β_self_co2xyear    = Variable()
    β_self_ch4xyear    = Variable()
    β_self_loggdp0     = Variable()
    β_self_loggdppc    = Variable()
    β_self_laglogpm0   = Variable()
    β_self_logcost     = Variable()
    β_self_logcostxloggdppc = Variable()

    # === Regression Coefficients (EXPORT) ===
    β_export_co2       = Variable()
    β_export_ch4       = Variable()
    β_export_co2xyear  = Variable()
    β_export_ch4xyear  = Variable()
    β_export_loggdp0   = Variable()
    β_export_loggdppc  = Variable()
    β_export_laglogpm0 = Variable()
    β_export_logcost   = Variable()
    β_export_logcostxloggdppc = Variable()

    # === Output Variables ===
    logpm_self   = Variable(index=[time, country], unit="log(μg/m^3)")  # PM2.5 from own emissions
    logpm_export = Variable(index=[time, country], unit="log(μg/m^3)")  # PM2.5 from exported emissions
    pm_total = Variable(index=[time, country], unit="μg/m^3")  # PM2.5 from exported emissions

    function init(p, v, d)
        pm25_self_params = CSV.read(pagedata("pollution/mvrnorm_SELF_Contribution$(p.scenario_suffix).csv"), DataFrame)
        pm25_export_params = CSV.read(pagedata("pollution/mvrnorm_EXPORT_Contribution$(p.scenario_suffix).csv"), DataFrame)

        if p.pm25_draw == 0
            values_self = mean.(eachcol(pm25_self_params))
            values_export = mean.(eachcol(pm25_export_params))
        else
            values_self = pm25_self_params[p.pm25_draw, :]
            values_export = pm25_export_params[p.pm25_draw, :]
        end

        if p.use_extrap
            idx1 = findall(col -> startswith(col, "year0:factor(REGION_4LETTER)"), names(pm25_self_params))
            pm25_self_fixeds_regional = values_self[idx1]
            pm25_self_fixeds_regional2 = DataFrame(idx=replace.(names(pm25_self_params)[idx1], "year0:factor(REGION_4LETTER)" => ""), effect=collect(pm25_self_fixeds_regional))
            pm25_self_fixeds = leftjoin(gains_mapping, pm25_self_fixeds_regional2, on=:REGION_4LETTER => :idx)

            idx2 = findall(col -> startswith(col, "factor(IDYEARS)"), names(pm25_self_params))
            pm25_self_fixeds_year = values_self[idx2]
            lininterp_self = LinearInterpolation(parse.(Int64, replace.(names(pm25_self_params)[idx2], "factor(IDYEARS)" => "")),
                                                 collect(pm25_self_fixeds_year), extrapolation_bc=Flat())
            v.yearfe_self[:] = lininterp_self(dim_keys(model, :time))
            v.trendfe_self[:] = readcountrydata_i_const(model, pm25_self_fixeds, :ISO3, :effect, vv -> mean(skipmissing(vv)))

            idx1 = findall(col -> startswith(col, "year0:factor(REGION_4LETTER)"), names(pm25_export_params))
            pm25_export_fixeds_regional = values_export[idx1]
            pm25_export_fixeds_regional2 = DataFrame(idx=replace.(names(pm25_export_params)[idx1], "year0:factor(REGION_4LETTER)" => ""), effect=collect(pm25_export_fixeds_regional))
            pm25_export_fixeds = leftjoin(gains_mapping, pm25_export_fixeds_regional2, on=:REGION_4LETTER => :idx)

            idx2 = findall(col -> startswith(col, "factor(IDYEARS)"), names(pm25_export_params))
            pm25_export_fixeds_year = values_export[idx2]
            lininterp_export = LinearInterpolation(parse.(Int64, replace.(names(pm25_export_params)[idx2], "factor(IDYEARS)" => "")),
                                                   collect(pm25_export_fixeds_year), extrapolation_bc=Flat())
            v.yearfe_export[:] = lininterp_export(dim_keys(model, :time))
            v.trendfe_export[:] = readcountrydata_i_const(model, pm25_export_fixeds, :ISO3, :effect, vv -> mean(skipmissing(vv)))
        else
            v.yearfe_self[:] .= 0.
            v.trendfe_self[:] .= 0.

            v.yearfe_export[:] .= 0.
            v.trendfe_export[:] .= 0.
        end

        if p.use_econ
            v.β_self_co2      = p.use_co2 ? values_self[findfirst(names(pm25_self_params) .== "logco20")] : 0.
            v.β_self_ch4      = 0.
            v.β_self_co2xyear = 0.
            v.β_self_ch4xyear = 0.
            v.β_self_loggdp0 = 0.
            v.β_self_logcost = 0.
            v.β_self_loggdppc = values_self[findfirst(names(pm25_self_params) .== "loggdppc0")]
            v.β_self_logcostxloggdppc = 0.
            v.β_self_laglogpm0 = values_self[findfirst(names(pm25_self_params) .== "laglogpm0")]

            v.β_export_co2      = p.use_co2 ? values_export[findfirst(names(pm25_export_params) .== "logco20")] : 0.
            v.β_export_ch4      = 0.
            v.β_export_co2xyear = 0.
            v.β_export_ch4xyear = 0.
            v.β_export_loggdp0 = 0.
            v.β_export_logcost = 0.
            v.β_export_loggdppc = values_export[findfirst(names(pm25_export_params) .== "loggdppc0")]
            v.β_export_logcostxloggdppc = 0.
            v.β_export_laglogpm0 = values_export[findfirst(names(pm25_export_params) .== "laglogpm0")]
        else
            v.β_self_co2      = 0.
            v.β_self_ch4      = 0.
            v.β_self_co2xyear = 0.
            v.β_self_ch4xyear = 0.
            v.β_self_loggdp0 = 0.
            v.β_self_logcost = 0.
            v.β_self_logcostxloggdppc = 0.
            v.β_self_laglogpm0 = 0.

            v.β_export_co2      = 0.
            v.β_export_ch4      = 0.
            v.β_export_co2xyear = 0.
            v.β_export_ch4xyear = 0.
            v.β_export_loggdp0 = 0.
            v.β_export_logcost = 0.
            v.β_export_logcostxloggdppc = 0.
            v.β_export_laglogpm0 = 0.
        end
    end

    function run_timestep(pp, vv, dd, tt)
        if is_first(tt)
            vv.logpm_self[tt, :] .= 0.
            vv.logpm_export[tt, :] .= 0.
        else
            baseidx = TimestepIndex(findfirst(pp.y_year[:] .== pp.baseline_year[tt]))

            # Bias-correct so that difference is 0 in 2020
            if pp.use_co2
                logco20 = log(max(1, sum(pp.e_countryCO2emissions[tt, :]))) - log(max(1, pp.baseline_co2[tt])) - (log(sum(pp.e_countryCO2emissions[TimestepIndex(1), :])) - log(pp.baseline_co2[TimestepIndex(1)]))
            else
                logco20 = 0
            end
            logch40 = log(sum(pp.e_countryCH4emissions[tt, :])) - log(pp.baseline_ch4[tt]) - (log(sum(pp.e_countryCH4emissions[TimestepIndex(1), :])) - log(pp.baseline_ch4[TimestepIndex(1)]))

            loggdp0 = (log.(pp.gdp[tt, :]) - log.(pp.gdp[TimestepIndex(1), :])) - (log.(pp.baseline_gdp[tt, :]) - log.(pp.baseline_gdp[TimestepIndex(1), :]))
            # logpop0 = log.(pp.pop_population[tt, :]) - log.(pp.pop_population[TimestepIndex(1), :])
            loggdppc = log.(pp.gdp[tt, :] ./ pp.pop_population[tt, :])
            loggdppc0 = loggdppc - log.(pp.baseline_gdppc[tt, :]) - (log.(pp.gdp[TimestepIndex(1), :] ./ pp.pop_population[TimestepIndex(1), :]) - log.(pp.baseline_gdppc[TimestepIndex(1), :]))
            loggdppc02 = loggdppc.^2 - log.(pp.baseline_gdppc[tt, :]).^2 - (log.(pp.gdp[TimestepIndex(1), :] ./ pp.pop_population[TimestepIndex(1), :]).^2 - log.(pp.baseline_gdppc[TimestepIndex(1), :]).^2)

            ekc_effect = pp.ekc_loggdppc_coeff * loggdppc0 + pp.ekc_loggdppc2_coeff * loggdppc02

            ## logcost * β - logcost_baseline * β
            logcosteffect_self = log(pp.control_factor) * max.(0., vv.β_self_logcost .+ vv.β_self_logcostxloggdppc .* loggdppc)
            logcosteffect_export = log(pp.control_factor) * max.(0., vv.β_export_logcost .+ vv.β_export_logcostxloggdppc .* loggdppc)

            vv.logpm_self[tt, :] = vv.β_self_co2 * logco20 +
                vv.β_self_ch4 * logch40 +
                vv.β_self_co2xyear * logco20 * (pp.baseline_year[tt] - 2020) +
                vv.β_self_ch4xyear * logch40 * (pp.baseline_year[tt] - 2020) .+
                vv.β_self_loggdp0 * loggdp0 #+
                # vv.β_self_loggdppc * loggdppc +
                # logcosteffect_self +
                # # vv.β_self_laglogpm0 * vv.logpm_self[tt - 1, :] + <-- DROP because can't do diff
                # ekc_effect .+
                # (vv.yearfe_self[tt] - vv.yearfe_self[baseidx])
                # # vv.trendfe_self .* (pp.y_year[tt] - pp.baseline_year[tt]) <-- Don't allow extrapolation

            vv.logpm_export[tt, :] = vv.β_export_co2 * logco20 .+
                # vv.β_export_ch4 * logch40 +
                # vv.β_export_co2xyear * logco20 * (pp.baseline_year[tt] - 2020) +
                # vv.β_export_ch4xyear * logch40 * (pp.baseline_year[tt] - 2020) .+
                # vv.β_export_loggdp0 * loggdp0 +
                vv.β_export_loggdppc * loggdppc +
                logcosteffect_export +
                # vv.β_export_laglogpm0 * vv.logpm_export[tt - 1, :] + <-- DROP because can't do diff
                ekc_effect .+
                (vv.yearfe_export[tt] - vv.yearfe_export[baseidx]) # 0 under flat interpolation
                # vv.trendfe_export .* (pp.y_year[tt] - pp.baseline_year[tt]) <-- Don't allow extrapolation

            # Fill in missing values
            mean_self = mean(filter(x -> !ismissing(x) && !isnan(x), vv.logpm_self[tt, :]))
            vv.logpm_self[tt, ismissing.(vv.logpm_self[tt, :])] .= mean_self
            vv.logpm_self[tt, isnan.(vv.logpm_self[tt, :])] .= mean_self
            mean_export = mean(filter(x -> !ismissing(x) && !isnan(x), vv.logpm_export[tt, :]))
            vv.logpm_export[tt, ismissing.(vv.logpm_export[tt, :])] .= mean_export
            vv.logpm_export[tt, isnan.(vv.logpm_export[tt, :])] .= mean_export
        end

        if pp.gainsmatch && pp.y_year[tt] <= 2100
            vv.pm_total[tt, :] = pp.baseline_pm25_total[tt, :]
            mean_total = mean(filter(x -> !ismissing(x) && !isnan(x), vv.pm_total[tt, :]))
            vv.pm_total[tt, ismissing.(vv.pm_total[tt, :])] .= mean_total
            vv.pm_total[tt, isnan.(vv.pm_total[tt, :])] .= mean_total
        else
            exports = pp.baseline_pm25_export[tt, :] .* exp.(vv.logpm_export[tt, :])
            vv.pm_total[tt, :] = pp.baseline_pm25_self[tt, :] .* exp.(vv.logpm_self[tt, :]) + pp.export_pattern * exports
        end
    end
end

function load_pm25pollution_baseline(model::Model, scenario::String)
    baseline2 = load_pm25pollution_basedata(model, scenario)

    baseline_year = zeros(dim_count(model, :time))
    baseline_co2 = zeros(dim_count(model, :time))
    baseline_ch4 = zeros(dim_count(model, :time))
    baseline_gdp = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    baseline_gdppc = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    # baseline_costs = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    baseline_pm25_total = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    baseline_pm25_self = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    baseline_pm25_export = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))

    for tt in 1:dim_count(model, :time)
        baseline_page, baseline_page_year = get_pm25pollution_baserows(model, scenario, baseline2, dim_keys(model, :time)[tt])
        baseline_year[tt] = baseline_page_year

        baseline_co2[tt] = baseline_page."CO2 Mt CO2/yr"[1]
        baseline_ch4[tt] = baseline_page."CH4 kt/yr"[1] / 1000
        baseline_gdp[tt, :] = baseline_page.GDP_GUSD2017_PPP * 1e3
        baseline_gdppc[tt, :] = 1e9 * baseline_page.GDP_GUSD2017_PPP ./ baseline_page.POPULATION
        # baseline_costs[tt, :] = baseline_page.AP_CONTROL_COSTS_MEUR2015
        baseline_pm25_total[tt, :] = baseline_page.PM25_TOTAL
        baseline_pm25_self[tt, :] = baseline_page.PM25_SELF
        baseline_pm25_export[tt, :] = baseline_page.PM25_EXPORT
    end

    return baseline_year, baseline_co2, baseline_ch4, baseline_gdp, baseline_gdppc, baseline_pm25_total, baseline_pm25_self, baseline_pm25_export
end

function add_pm25pollution(model::Model, useekc::Bool, useecon::Bool, useextrap::Bool, useco2::Bool, gainsmatch::Bool, scenario::String)
    pm25pollution = add_comp!(model, PM25Pollution)
    pm25pollution[:pm25_draw] = 0
    pm25pollution[:use_econ] = useecon
    pm25pollution[:use_extrap] = useextrap
    pm25pollution[:use_co2] = useco2
    pm25pollution[:gainsmatch] = gainsmatch
    pm25pollution[:e_countryCO2emissions] = zeros(dim_count(model, :time), dim_count(model, :country))

    export_pattern = CSV.read(pagedata("pollution/export_pattern.csv"), DataFrame)

    pattern_matrix = zeros(dim_count(model, :country), dim_count(model, :country)) # sink x source
    for ii in 1:nrow(export_pattern)
        iis = [iso ∈ gains_mapping.ISO3[gains_mapping.REGION_4LETTER .== export_pattern.REGION_4LETTER[ii]] for iso in dim_keys(model, :country)]
        jjs = [iso ∈ gains_mapping.ISO3[gains_mapping.REGION_4LETTER .== export_pattern.Source[ii]] for iso in dim_keys(model, :country)]
        pattern_matrix[iis, jjs] .= export_pattern.PM25_Contribution[ii]
    end

    pm25pollution[:export_pattern] = pattern_matrix

    baseline_year, baseline_co2, baseline_ch4, baseline_gdp, baseline_gdppc, baseline_pm25_total, baseline_pm25_self, baseline_pm25_export =
        load_pm25pollution_baseline(model, scenario)

    pm25pollution[:baseline_year] = baseline_year
    pm25pollution[:baseline_co2] = baseline_co2
    pm25pollution[:baseline_ch4] = baseline_ch4
    pm25pollution[:baseline_gdp] = baseline_gdp
    pm25pollution[:baseline_gdppc] = baseline_gdppc
    # pm25pollution[:baseline_costs] = baseline_costs
    pm25pollution[:baseline_pm25_total] = baseline_pm25_total
    pm25pollution[:baseline_pm25_self] = baseline_pm25_self
    pm25pollution[:baseline_pm25_export] = baseline_pm25_export

    if useekc
        pm25pollution[:ekc_loggdppc_coeff] = 0.2727793
        pm25pollution[:ekc_loggdppc2_coeff] = -0.0139551
    end

    if scenario in ["Delayed Action", "Integrated Action"]
        pm25pollution[:scenario_suffix] = "_decarb"
    else
        pm25pollution[:scenario_suffix] = "_baseline"
    end

    return pm25pollution
end

