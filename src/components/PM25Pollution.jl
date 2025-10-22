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

gains_mapping = CSV.read(pagedata("pollution/GAINS_4letter_regions_mapping.csv"), DataFrame)

@defcomp PM25Pollution begin
    country = Index()
    time = Index()

    y_year_0 = Parameter(unit="year")
    y_year = Parameter(index=[time], unit="year")

    # === Input Predictors ===
    e_countryCO2emissions = Parameter(index=[time, country], unit="Mtonne/year")
    e_countryCH4emissions = Parameter(index=[time, country], unit="Mtonne/year")

    gdp = Parameter(index=[time, country], unit="\$M")
    gdp0_initgdp = Parameter(index=[country], unit="\$M")

    pop_population = Parameter(index=[time, country], unit="million person")
    pop0_initpopulation = Parameter(index=[country], unit="million person")

    control_factor = Parameter(default=1.) # > 1 to increase control action

    # === Draw Selector ===
    pm25_draw = Parameter{Int}()  # 0 = use mean of draws, 1–1000 = use a specific draw
    scenario_suffix = Parameter{String}()

    yearfe_self = Parameter(index=[time])
    trendfe_self = Parameter(index=[country])
    yearfe_export = Parameter(index=[time])
    trendfe_export = Parameter(index=[country])

    baseline_year = Parameter(index=[time])
    baseline_co2 = Parameter(index=[time, country], unit="Mtonne/year")
    baseline_ch4 = Parameter(index=[time, country], unit="Mtonne/year")
    baseline_gdppc = Parameter(index=[time, country], unit="\$/person")
    # baseline_costs = Parameter(index=[time, country], unit="million EUR")
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
    # β_self_costs       = Variable()

    # === Regression Coefficients (EXPORT) ===
    β_export_co2       = Variable()
    β_export_ch4       = Variable()
    β_export_co2xyear  = Variable()
    β_export_ch4xyear  = Variable()
    # β_export_costs     = Variable()

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

        pm25_self_fixeds_regional = values_self[filter(col -> startswith(col, "year0:factor(REGION_4LETTER)"), names(values_self))]
        pm25_self_fixeds_regional2 = DataFrame(idx=replace.(names(pm25_self_fixeds_regional), "year0:factor(REGION_4LETTER)" => ""), effect=collect(pm25_self_fixeds_regional))
        pm25_self_fixeds = leftjoin(gains_mapping, pm25_self_fixeds_regional2, on=:REGION_4LETTER => :idx)

        pm25_self_fixeds_year = values_self[filter(col -> startswith(col, "factor(IDYEARS)"), names(values_self))]
        lininterp_self = LinearInterpolation(parse.(Int64, replace.(names(pm25_self_fixeds_year), "factor(IDYEARS)" => "")),
                                             collect(pm25_self_fixeds_year), extrapolation_bc=Flat())
        v.yearfe_self = lininterp_self(dim_keys(model, :time))
        v.trendfe_self = readcountrydata_i_const(model, pm25_self_fixeds, :ISO3, :effect, vv -> mean(skipmissing(vv)))

        pm25_export_fixeds_regional = values_export[filter(col -> startswith(col, "year0:factor(REGION_4LETTER)"), names(values_export))]
        pm25_export_fixeds_regional2 = DataFrame(idx=replace.(names(pm25_export_fixeds_regional), "year0:factor(REGION_4LETTER)" => ""), effect=collect(pm25_export_fixeds_regional))
        pm25_export_fixeds = leftjoin(gains_mapping, pm25_export_fixeds_regional2, on=:REGION_4LETTER => :idx)

        pm25_export_fixeds_year = values_export[filter(col -> startswith(col, "factor(IDYEARS)"), names(values_export))]
        lininterp_export = LinearInterpolation(parse.(Int64, replace.(names(pm25_export_fixeds_year), "factor(IDYEARS)" => "")),
                                             collect(pm25_export_fixeds_year), extrapolation_bc=Flat())
        v.yearfe_export = lininterp_export(dim_keys(model, :time))
        v.trendfe_export = readcountrydata_i_const(model, pm25_export_fixeds, :ISO3, :effect, vv -> mean(skipmissing(vv)))

        v.β_self_co2      = values_self.logco20
        v.β_self_ch4      = 0.
        v.β_self_co2xyear = values_self.logco20xyear0
        v.β_self_ch4xyear = 0.
        v.β_self_loggdp0 = values_self.loggdp0
        # v.β_self_logcost = ...
        # v.β_self_logcostxloggdppc = ...
        v.β_self_laglogpm0 = values_self[Symbol(:laglogpm0.self)]

        v.β_export_co2      = values_export.logco20
        v.β_export_ch4      = 0.
        v.β_export_co2xyear = values_export.logco20xyear0
        v.β_export_ch4xyear = 0.
        v.β_export_loggdp0 = values_export.loggdp0
        # v.β_export_logcost = ...
        # v.β_export_logcostxloggdppc = ...
        v.β_export_laglogpm0 = values_export[Symbol(:laglogpm0.export)]
    end

    function run_timestep(pp, vv, dd, tt)
        if is_first(tt)
            vv.logpm_self[tt, :] .= 0.
            vv.logpm_export[tt, :] .= 0.
        else
            # Bias-correct so that difference is 0 in 2020
            logco20 = log.(pp.e_countryCO2emissions[tt, :]) - log.(pp.baseline_co2[tt, :]) - (log.(pp.e_countryCO2emissions[TimestepIndex(1), :]) - log.(pp.baseline_co2[TimestepIndex(1), :]))
            logch40 = log.(pp.e_countryCH4emissions[tt, :]) - log.(pp.baseline_ch4[tt, :]) - (log.(pp.e_countryCH4emissions[TimestepIndex(1), :]) - log.(pp.baseline_ch4[TimestepIndex(1), :]))

            loggdp0 = log.(pp.gdp[tt, :]) - log.(pp.gdp[TimestepIndex(1), :])
            # logpop0 = log.(pp.pop_population[tt, :]) - log.(pp.pop_population[TimestepIndex(1), :])
            # loggdppc0 = log.(pp.gdp[tt, :] ./ pp.pop_population[tt, :]) - log.(pp.baseline_gdppc[tt, :]) - (log.(pp.gdp[TimestepIndex(1), :] ./ pp.pop_population[TimestepIndex(1), :]) - log.(pp.baseline_gdppc[TimestepIndex(1), :]))
            # loggdppc02 = log.(pp.gdp[tt, :] ./ pp.pop_population[tt, :]).^2 - log.(pp.baseline_gdppc[tt, :]).^2 - (log.(pp.gdp[TimestepIndex(1), :] ./ pp.pop_population[TimestepIndex(1), :]).^2 - log.(pp.baseline_gdppc[TimestepIndex(1), :]).^2)

            # logcost0 = pp.control_factor * (log.(pp.baseline_costs[tt, :]) - log.(pp.baseline_costs[TimestepIndex(1), :]))

            ekc_effect = pp.ekc_loggdppc_coeff * loggdppc0 + pp.ekc_loggdppc2_coeff * loggdppc02

            vv.logpm_self[tt, :] = vv.β_self_co2 * logco20 +
                vv.β_self_ch4 * logch40 +
                vv.β_self_co2xyear * logco20 * (pp.y_year[tt] - 2020) +
                vv.β_self_ch4xyear * logch40 * (pp.y_year[tt] - 2020) .+
                vv.β_self_loggdp0 * loggdp0 +
                # vv.β_self_costs * logcost0 +
                v.β_self_laglogpm0 * vv.logpm_self[tt - 1, :] +
                ekc_effect .+
                # pp.yearfe_self[tt] .+ <-- don't include: already in baseline
                vv.trendfe_self .* (pp.y_year[tt] - pp.baseline_year[tt])

            vv.logpm_export[tt, :] = vv.β_export_co2 * logco20 +
                vv.β_export_ch4 * logch40 +
                vv.β_export_co2xyear * logco20 * (pp.y_year[tt] - 2020) +
                vv.β_export_ch4xyear * logch40 * (pp.y_year[tt] - 2020) .+
                vv.β_export_loggdp0 * loggdp0 +
                # vv.β_export_costs * logcost0 +
                v.β_export_laglogpm0 * vv.logpm_export[tt - 1, :] +
                ekc_effect .+
                # pp.yearfe_export[tt] .+ <-- don't include: already in baseline
                vv.trendfe_export .* (pp.y_year[tt] - pp.baseline_year[tt])

            # Fill in missing values
            mean_self = mean(filter(x -> !ismissing(x) && !isnan(x), vv.logpm_self[tt, :]))
            vv.logpm_self[tt, ismissing.(vv.logpm_self[tt, :])] .= mean_self
            vv.logpm_self[tt, isnan.(vv.logpm_self[tt, :])] .= mean_self
            mean_export = mean(filter(x -> !ismissing(x) && !isnan(x), vv.logpm_export[tt, :]))
            vv.logpm_export[tt, ismissing.(vv.logpm_export[tt, :])] .= mean_export
            vv.logpm_export[tt, isnan.(vv.logpm_export[tt, :])] .= mean_export
        end

        exports = pp.baseline_pm25_export[tt, :] .* exp.(vv.logpm_export[tt, :])
        vv.pm_total[tt, :] = pp.baseline_pm25_self[tt, :] .* exp.(vv.logpm_self[tt, :]) + pp.export_pattern * exports
    end
end

function add_pm25pollution(model::Model, useekc::Bool, scenario::Symbol)
    pm25pollution = add_comp!(model, PM25Pollution)
    pm25pollution[:pm25_draw] = 0


    export_pattern = CSV.read(pagedata("pollution/export_pattern.csv"), DataFrame)

    pattern_matrix = zeros(dim_count(model, :country), dim_count(model, :country)) # sink x source
    for ii in 1:nrow(export_pattern)
        iis = [iso ∈ gains_mapping.ISO3[gains_mapping.REGION_4LETTER .== export_pattern.REGION_4LETTER[ii]] for iso in dim_keys(model, :country)]
        jjs = [iso ∈ gains_mapping.ISO3[gains_mapping.REGION_4LETTER .== export_pattern.Source[ii]] for iso in dim_keys(model, :country)]
        pattern_matrix[iis, jjs] .= export_pattern.PM25_Contribution[ii]
    end

    pm25pollution[:export_pattern] = pattern_matrix

    baseline = CSV.read(pagedata("pollution/baseline.csv"), DataFrame)
    baseline2 = leftjoin(gains_mapping, baseline, on=:REGION_4LETTER)
    baseline2 = baseline2[.!ismissing.(baseline2.IDYEARS), :]

    baseline_year = zeros(dim_count(model, :time))
    baseline_co2 = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    baseline_ch4 = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    baseline_gdppc = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    # baseline_costs = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    baseline_pm25_self = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))
    baseline_pm25_export = zeros(Union{Missing, Float64}, dim_count(model, :time), dim_count(model, :country))

    baseline_after2050 = baseline2[(baseline2.IDYEARS .== 2050) .& (baseline2.IDSCENARIOS .== String(scenario)), :]
    baseline_after2050_page = leftjoin(DataFrame(ISO3=dim_keys(model, :country)), baseline_after2050, on=:ISO3)
    baseline_after2050_page[ismissing.(baseline_after2050_page[!, :PM25_SELF]), :PM25_SELF] .= mean(skipmissing(baseline_after2050_page[!, :PM25_SELF]))
    baseline_after2050_page[ismissing.(baseline_after2050_page[!, :PM25_EXPORT]), :PM25_EXPORT] .= mean(skipmissing(baseline_after2050_page[!, :PM25_EXPORT]))
    for tt in 1:dim_count(model, :time)
        if dim_keys(model, :time)[tt] >= 2050
            baseline_page = baseline_after2050_page
            baseline_year[tt] = 2050
        else
            if dim_keys(model, :time)[tt] < 2030
                baseline_period = baseline2[baseline2.IDYEARS .== dim_keys(model, :time)[tt], :]
            else
                baseline_period = baseline2[(baseline2.IDYEARS .== dim_keys(model, :time)[tt]) .& (baseline2.IDSCENARIOS .== String(scenario)), :]
            end
            baseline_page = leftjoin(DataFrame(ISO3=dim_keys(model, :country)), baseline_period, on=:ISO3)
            baseline_page[ismissing.(baseline_page[!, :PM25_SELF]), :PM25_SELF] .= mean(skipmissing(baseline_page[!, :PM25_SELF]))
            baseline_page[ismissing.(baseline_page[!, :PM25_EXPORT]), :PM25_EXPORT] .= mean(skipmissing(baseline_page[!, :PM25_EXPORT]))

            baseline_year[tt] = dim_keys(model, :time)[tt]
        end

        baseline_co2[tt, :] = baseline_page.EMIS_CO2_KT / 1000
        baseline_ch4[tt, :] = baseline_page.EMIS_CH4_KT / 1000
        baseline_gdppc[tt, :] = 1e9 * baseline_page.GDP_PPP ./ baseline_page.POP
        # baseline_costs[tt, :] = baseline_page.AP_CONTROL_COSTS_MEUR2015
        baseline_pm25_self[tt, :] = baseline_page.PM25_SELF
        baseline_pm25_export[tt, :] = baseline_page.PM25_EXPORT
    end

    pm25pollution[:baseline_year] = baseline_year
    pm25pollution[:baseline_co2] = baseline_co2
    pm25pollution[:baseline_ch4] = baseline_ch4
    pm25pollution[:baseline_gdppc] = baseline_gdppc
    # pm25pollution[:baseline_costs] = baseline_costs
    pm25pollution[:baseline_pm25_self] = baseline_pm25_self
    pm25pollution[:baseline_pm25_export] = baseline_pm25_export

    if useekc
        pm25pollution[:ekc_loggdppc_coeff] = 0.2727793
        pm25pollution[:ekc_loggdppc2_coeff] = -0.0139551
    end

    if scenario == :Decarb
        pm25pollution[:scenario_suffix] = "_decarb"
    else
        pm25pollution[:scenario_suffix] = "_baseline"
    end

    return pm25pollution
end

