# ==============================================================================
# PM2.5 Pollution Component
#
# This component estimates anthropogenic PM2.5 concentrations resulting from
# greenhouse gas emissions and socioeconomic factors.
#
#  SELF Contribution:
#   - Portion of PM2.5 concentration within a region attributable to its own
#     emissions.
#
#  EXPORT Contribution:
#   - Portion of PM2.5 concentration observed in other regions attributable
#     to exported (transboundary) emissions.
#
# Coefficients come from 1000 Monte Carlo draws.
# ==============================================================================

using DataFrames
using CSV

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

    # === Input Predictors ===
    logco20         = Parameter(index=[time, country], unit="log(MtCO2/year)")            # CO2 emissions (log)
    logch40         = Parameter(index=[time, country], unit="log(Mtonne/year)")           # CH4 emissions (log)
    logco20xyear0   = Parameter(index=[time, country], unit="log(MtCO2/year) * year")     # CO2 x year interaction
    logch40xyear0   = Parameter(index=[time, country], unit="log(Mtonne/year) * year")    # CH4 x year interaction
    logpop0         = Parameter(index=[time, country], unit="log(million person)")        # Population (log)
    loggdppc0       = Parameter(index=[time, country], unit="log(\$/person)")              # GDP per capita (log)
    loggdppc02      = Parameter(index=[time, country], unit="(log(\$/person))^2")          # GDP per capita squared (log-squared)
    laglogpm0       = Parameter(index=[time, country], unit="log(μg/m^3)")                # 1-year lagged PM2.5
    lag2logpm0      = Parameter(index=[time, country], unit="log(μg/m^3)")                # 2-year lagged PM2.5

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

    function run_timestep(p, v, d, t)
    v.logpm_self[t.t, :] = v.β_self_co2      * p.logco20[t.t, :] +
                           v.β_self_ch4      * p.logch40[t.t, :] +
                           v.β_self_co2xyear * p.logco20xyear0[t.t, :] +
                           v.β_self_ch4xyear * p.logch40xyear0[t.t, :] +
                           v.β_self_pop      * p.logpop0[t.t, :] +
                           v.β_self_gdppc    * p.loggdppc0[t.t, :] +
                           v.β_self_gdppc2   * p.loggdppc02[t.t, :] +
                           v.β_self_lag1     * p.laglogpm0[t.t, :] +
                           v.β_self_lag2     * p.lag2logpm0[t.t, :]

    v.logpm_export[t.t, :] = v.β_export_co2      * p.logco20[t.t, :] +
                             v.β_export_ch4      * p.logch40[t.t, :] +
                             v.β_export_co2xyear * p.logco20xyear0[t.t, :] +
                             v.β_export_ch4xyear * p.logch40xyear0[t.t, :] +
                             v.β_export_pop      * p.logpop0[t.t, :] +
                             v.β_export_gdppc    * p.loggdppc0[t.t, :] +
                             v.β_export_gdppc2   * p.loggdppc02[t.t, :] +
                             v.β_export_lag1     * p.laglogpm0[t.t, :] +
                             v.β_export_lag2     * p.lag2logpm0[t.t, :]
    end
end

function add_pm25_pollution(model::Model)
    pm25pollution = add_comp!(model, pm25_pollution)
    pm25pollution[:pm25_draw] = 0
    return pm25pollution
end