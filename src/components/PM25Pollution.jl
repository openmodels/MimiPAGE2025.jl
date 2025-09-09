# ==============================================================================
# PM2.5 Pollution Component
#
# Estimates anthropogenic PM2.5 (SELF and EXPORT logs) from emissions and
# socioeconomic drivers using MC-drawn coefficients. Also constructs a physical
# total PM concentration (μg/m^3) per country & time by combining:
#   - baseline SELF × exp(logpm_self), plus
#   - export_pattern × (baseline EXPORT × exp(logpm_export))
#
# Coefficients come from 1000 Monte Carlo draws.
# ==============================================================================

using DataFrames
using CSV

# ---- Load Monte Carlo draws (once) ----
if !isdefined(Main, :pm25_self_params)
    # global const pm25_self_params = CSV.read("../data/pollution/mvrnorm_SELF_Contribution.csv", DataFrame)
    global const pm25_self_params = CSV.read(joinpath(@__DIR__, "../../data/pollution/mvrnorm_SELF_Contribution.csv"), DataFrame)
end

if !isdefined(Main, :pm25_export_params)
    # global const pm25_export_params = CSV.read("../data/pollution/mvrnorm_EXPORT_Contribution.csv", DataFrame)
    global const pm25_export_params = CSV.read(joinpath(@__DIR__, "../../data/pollution/mvrnorm_EXPORT_Contribution.csv"), DataFrame)
end

@defcomp pm25_pollution begin
    country = Index()
    time    = Index()

    # === Input Predictors (already prepared upstream) ===
    logco20         = Parameter(index=[time, country], unit="log(MtCO2/year)")
    logch40         = Parameter(index=[time, country], unit="log(Mtonne/year)")
    logco20xyear0   = Parameter(index=[time, country], unit="log(MtCO2/year) * year")
    logch40xyear0   = Parameter(index=[time, country], unit="log(Mtonne/year) * year")
    logpop0         = Parameter(index=[time, country], unit="log(million person)")
    loggdppc0       = Parameter(index=[time, country], unit="log(\$/person)")
    loggdppc02      = Parameter(index=[time, country], unit="(log(\$/person))^2")
    laglogpm0       = Parameter(index=[time, country], unit="log(μg/m^3)")
    lag2logpm0      = Parameter(index=[time, country], unit="log(μg/m^3)")

    # === Draw selector ===
    pm25_draw = Parameter{Int}()  # 0 = mean of draws, 1..1000 = specific draw

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

    # === Outputs (logs) ===
    logpm_self   = Variable(index=[time, country], unit="log(μg/m^3)")
    logpm_export = Variable(index=[time, country], unit="log(μg/m^3)")

    # === NEW: mixing + baselines to build physical totals ===
    export_pattern        = Parameter(index=[country, country])     # sink × source
    baseline_pm25_self    = Parameter(index=[time, country])        # μg/m^3
    baseline_pm25_export  = Parameter(index=[time, country])        # μg/m^3

    # === NEW: physical total PM (μg/m^3) received by each sink ===
    pm_total = Variable(index=[time, country], unit="μg/m^3")

    # ---- init: pick MC draw (or mean) and load coefficients ----
    function init(p, v, d)
        if p.pm25_draw == 0
            values  = mean.(eachcol(pm25_self_params))
            values2 = mean.(eachcol(pm25_export_params))
            v.β_self_co2      = values[1]
            v.β_self_ch4      = values[2]
            v.β_self_co2xyear = values[3]
            v.β_self_ch4xyear = values[4]
            v.β_self_pop      = values[5]
            v.β_self_gdppc    = values[6]
            v.β_self_gdppc2   = values[7]
            v.β_self_lag1     = values[8]
            v.β_self_lag2     = values[9]

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

    # ---- timestep: compute logs; then build physical totals using baselines/mixing ----
    function run_timestep(p, v, d, t)
        # SELF log
        v.logpm_self[t, :] =
              v.β_self_co2      .* p.logco20[t, :]
            .+ v.β_self_ch4      .* p.logch40[t, :]
            .+ v.β_self_co2xyear .* p.logco20xyear0[t, :]
            .+ v.β_self_ch4xyear .* p.logch40xyear0[t, :]
            .+ v.β_self_pop      .* p.logpop0[t, :]
            .+ v.β_self_gdppc    .* p.loggdppc0[t, :]
            .+ v.β_self_gdppc2   .* p.loggdppc02[t, :]
            .+ v.β_self_lag1     .* p.laglogpm0[t, :]
            .+ v.β_self_lag2     .* p.lag2logpm0[t, :]

        # EXPORT log
        v.logpm_export[t, :] =
              v.β_export_co2      .* p.logco20[t, :]
            .+ v.β_export_ch4      .* p.logch40[t, :]
            .+ v.β_export_co2xyear .* p.logco20xyear0[t, :]
            .+ v.β_export_ch4xyear .* p.logch40xyear0[t, :]
            .+ v.β_export_pop      .* p.logpop0[t, :]
            .+ v.β_export_gdppc    .* p.loggdppc0[t, :]
            .+ v.β_export_gdppc2   .* p.loggdppc02[t, :]
            .+ v.β_export_lag1     .* p.laglogpm0[t, :]
            .+ v.β_export_lag2     .* p.lag2logpm0[t, :]

        # --- NEW: construct physical PM totals from log pieces using baselines + mixing ---
        # total exports produced by each source j at time t (still on source index):
        exports = p.baseline_pm25_export[t, :] .* exp.(v.logpm_export[t, :])

        # each sink i receives: self_i + Σ_j pattern[i,j] * exports_j
        v.pm_total[t, :] =
            p.baseline_pm25_self[t, :] .* exp.(v.logpm_self[t, :]) .+
            p.export_pattern * exports
    end
end

# Helper to add this component (kept as-is; parameters are wired in main_model_def.jl)
function add_pm25_pollution(model::Model)
    pm25pollution = add_comp!(model, pm25_pollution)
    pm25pollution[:pm25_draw] = 0
    return pm25pollution
end



#=






# OLD CODE

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
    #global const pm25_self_params = CSV.read("../data/pollution/mvrnorm_SELF_Contribution.csv", DataFrame)
    global const pm25_self_params = CSV.read(joinpath(@__DIR__, "../../data/pollution/mvrnorm_SELF_Contribution.csv"), DataFrame)

end

if !isdefined(Main, :pm25_export_params)
    #global const pm25_export_params = CSV.read("../data/pollution/mvrnorm_EXPORT_Contribution.csv", DataFrame)
    global const pm25_export_params = CSV.read(joinpath(@__DIR__, "../../data/pollution/mvrnorm_EXPORT_Contribution.csv"), DataFrame)
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
    idx = Mimi.TimestepIndex(t.t)  # required index type for Mimi TimestepArray

    v.logpm_self[idx, :] =
          v.β_self_co2      .* p.logco20[idx, :]
        .+ v.β_self_ch4      .* p.logch40[idx, :]
        .+ v.β_self_co2xyear .* p.logco20xyear0[idx, :]
        .+ v.β_self_ch4xyear .* p.logch40xyear0[idx, :]
        .+ v.β_self_pop      .* p.logpop0[idx, :]
        .+ v.β_self_gdppc    .* p.loggdppc0[idx, :]
        .+ v.β_self_gdppc2   .* p.loggdppc02[idx, :]
        .+ v.β_self_lag1     .* p.laglogpm0[idx, :]
        .+ v.β_self_lag2     .* p.lag2logpm0[idx, :]

    v.logpm_export[idx, :] =
          v.β_export_co2      .* p.logco20[idx, :]
        .+ v.β_export_ch4      .* p.logch40[idx, :]
        .+ v.β_export_co2xyear .* p.logco20xyear0[idx, :]
        .+ v.β_export_ch4xyear .* p.logch40xyear0[idx, :]
        .+ v.β_export_pop      .* p.logpop0[idx, :]
        .+ v.β_export_gdppc    .* p.loggdppc0[idx, :]
        .+ v.β_export_gdppc2   .* p.loggdppc02[idx, :]
        .+ v.β_export_lag1     .* p.laglogpm0[idx, :]
        .+ v.β_export_lag2     .* p.lag2logpm0[idx, :]
    end
end





#=
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

=#

function add_pm25_pollution(model::Model)
    pm25pollution = add_comp!(model, pm25_pollution)
    pm25pollution[:pm25_draw] = 0
    return pm25pollution
end



=#