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

include("../utils/interpol.jl")

# Load Monte Carlo draws
if !isdefined(Main, :pm25_self_params)
    global const pm25_self_params = CSV.read("../data/pollution/mvrnorm_SELF_Contribution.csv", DataFrame)
end

if !isdefined(Main, :pm25_export_params)
    global const pm25_export_params = CSV.read("../data/pollution/mvrnorm_EXPORT_Contribution.csv", DataFrame)
end

@defcomp pm25_pollution begin
    country = Index()
    time = Index()

    y_year_0 = Parameter(unit="year")
    y_year = Parameter(index=[time], unit="year")

    # === Input Predictors ===
    e_countryCO2emissions = Parameter(index=[time, country], unit="MtCO2/year")
    e0_countryCO2emissions = Parameter(index=[country], unit="MtCO2/year")

    e_countryCH4emissions = Parameter(index=[time, country], unit="MtCH4/year")
    e0_countryCH4emissions = Parameter(index=[country], unit="MtCH4/year")

    gdp = Parameter(index=[time, country], unit="\$M")
    gdp0_initgdp = Parameter(index=[country], unit="\$M")

    pop = Parameter(index=[time, country], unit="million person")
    pop0_initpopulation = Parameter(index=[country], unit="million person")

    # === Draw Selector ===
    pm25_draw = Parameter{Int}()  # 0 = use mean of draws, 1–1000 = use a specific draw

    # === Regression Coefficients (SELF) ===
    β_self_co2         = Variable()
    β_self_ch4         = Variable()
    β_self_co2xyear    = Variable()
    β_self_ch4xyear    = Variable()
    β_self_pop         = Variable()
    β_self_gdppc       = Variable()
    β_self_gdppc2      = Variable()
    β_self_lag1        = Variable()
    β_self_lag2        = Variable()

    # === Regression Coefficients (EXPORT) ===
    β_export_co2       = Variable()
    β_export_ch4       = Variable()
    β_export_co2xyear  = Variable()
    β_export_ch4xyear  = Variable()
    β_export_pop       = Variable()
    β_export_gdppc     = Variable()
    β_export_gdppc2    = Variable()
    β_export_lag1      = Variable()
    β_export_lag2      = Variable()

    # === Output Variables ===
    logpm_self   = Variable(index=[time, country], unit="log(μg/m^3)")  # PM2.5 from own emissions
    logpm_export = Variable(index=[time, country], unit="log(μg/m^3)")  # PM2.5 from exported emissions

    function init(p, v, d)
        if p.pm25_draw == 0
            values = mean.(eachcol(pm25_self_params))
            v.β_self_co2      = values[1]
            v.β_self_ch4      = values[2]
            v.β_self_co2xyear = values[3]
            v.β_self_ch4xyear = values[4]
            v.β_self_pop      = values[5]
            v.β_self_gdppc    = values[6]
            v.β_self_gdppc2   = values[7]
            v.β_self_lag1     = values[8]
            v.β_self_lag2     = values[9]

            values2 = mean.(eachcol(pm25_export_params))
            v.β_export_co2      = values2[1]
            v.β_export_ch4      = values2[2]
            v.β_export_co2xyear = values2[3]
            v.β_export_ch4xyear = values2[4]
            v.β_export_pop      = values2[5]
            v.β_export_gdppc    = values2[6]
            v.β_export_gdppc2   = values2[7]
            v.β_export_lag1     = values2[8]
            v.β_export_lag2     = values2[9]
        else
            v.β_self_co2      = pm25_self_params[p.pm25_draw, 1]
            v.β_self_ch4      = pm25_self_params[p.pm25_draw, 2]
            v.β_self_co2xyear = pm25_self_params[p.pm25_draw, 3]
            v.β_self_ch4xyear = pm25_self_params[p.pm25_draw, 4]
            v.β_self_pop      = pm25_self_params[p.pm25_draw, 5]
            v.β_self_gdppc    = pm25_self_params[p.pm25_draw, 6]
            v.β_self_gdppc2   = pm25_self_params[p.pm25_draw, 7]
            v.β_self_lag1     = pm25_self_params[p.pm25_draw, 8]
            v.β_self_lag2     = pm25_self_params[p.pm25_draw, 9]

            v.β_export_co2      = pm25_export_params[p.pm25_draw, 1]
            v.β_export_ch4      = pm25_export_params[p.pm25_draw, 2]
            v.β_export_co2xyear = pm25_export_params[p.pm25_draw, 3]
            v.β_export_ch4xyear = pm25_export_params[p.pm25_draw, 4]
            v.β_export_pop      = pm25_export_params[p.pm25_draw, 5]
            v.β_export_gdppc    = pm25_export_params[p.pm25_draw, 6]
            v.β_export_gdppc2   = pm25_export_params[p.pm25_draw, 7]
            v.β_export_lag1     = pm25_export_params[p.pm25_draw, 8]
            v.β_export_lag2     = pm25_export_params[p.pm25_draw, 9]
        end
    end

    function run_timestep(pp, vv, dd, tt)
        logco20 = log.(pp.e_countryCO2emissions[tt, :]) - log.(pp.e0_countryCO2emissions)
        logch40 = log.(pp.e_countryCH4emissions[tt, :]) - log.(pp.e0_countryCH4emissions)

        logpop0 = log(pp.pop_population[tt, :]) - log(pp.pop0_initpopulation[tt, :])
        loggdppc0 = log(pp.gdp[tt, :] ./ pp.pop_population[tt, :]) - log(pp.gdp0_initgdp[tt, :] ./ pp.pop0_initpopulation[tt, :])
        loggdppc02 = log(pp.gdp[tt, :] ./ pp.pop_population[tt, :]).^2 - log(pp.gdp0_initgdp[tt, :] ./ pp.pop0_initpopulation[tt, :]).^2


        vv.logpm_self = v.β_self_co2 * logco20 +
            v.β_self_ch4 * logch40 +
            v.β_self_co2xyear * logco20 * (pp.y_year[tt] - pp.y_year_0) +
            v.β_self_ch4xyear * logch40 * (pp.y_year[tt] - pp.y_year_0) +
            v.β_self_pop * logpop0 +
            v.β_self_gdppc * loggdppc0 +
            v.β_self_gdppc2 * loggdppc02 +
            v.β_self_lag1 * [interpolate(gettime(tt) - 5, pp.y_year_0, 0, pp.y_year, vv.logpm_self[:, cc]) for cc in 1:num_countries] +
            v.β_self_lag2 * [interpolate(gettime(tt) - 10, pp.y_year_0, 0, pp.y_year, vv.logpm_self[:, cc]) for cc in 1:num_countries]

        v.logpm_export[t.t, :] = v.β_export_co2 * logco20 +
            v.β_export_ch4 * logch40 +
            v.β_export_co2xyear * logco20 * (pp.y_year[tt] - pp.y_year_0) +
            v.β_export_ch4xyear * logch40 * (pp.y_year[tt] - pp.y_year_0) +
            v.β_export_pop * logpop0 +
            v.β_export_gdppc * loggdppc0 +
            v.β_export_gdppc2 * loggdppc02 +
            v.β_export_lag1 * [interpolate(gettime(tt) - 5, pp.y_year_0, 0, pp.y_year, vv.logpm_export[:, cc]) for cc in 1:num_countries] +
            v.β_export_lag2 * [interpolate(gettime(tt) - 10, pp.y_year_0, 0, pp.y_year, vv.logpm_export[:, cc]) for cc in 1:num_countries]
    end
end

function add_pm25_pollution(model::Model)
    pm25pollution = add_comp!(model, pm25_pollution)
    pm25pollution[:pm25_draw] = 0
    return pm25pollution
end

