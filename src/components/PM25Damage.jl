
# ==============================================================================
# PM2.5 Damages Component
#
# Computes three cost components (MUSD/yr) per country & year using your
# chosen R Model 3 specification (estimated in logs):
#
#   ln(C_k) = α_k
#             + (β_pm_k + β_pm_year_k * YEAR[t]) * ln(PM25_TOTAL[t,c])
#             + θ_pop_k * ln(POP[t,c])
#             + θ_gdp_k * ln(GDP[t,c])
#
# with log-normal bias correction:
#   Ĉ_k = exp( ln(C_k) + 0.5 * Var(residuals_k) )
#
# Inputs:
#   PM25_TOTAL[t,c] comes directly from PM25Pollution.pm_total (μg/m^3),
#   already assembled via self + export mixing and baselines.
#
# Coefficients come from Monte Carlo draws stored in CSVs.
# ==============================================================================

using Mimi
using DataFrames
using CSV

@defcomp PM25Damage begin
    country = Index()
    time    = Index()

    # === Inputs ===
    y_year   = Parameter(index=[time], unit="year")                 # calendar year (initpage fills this)
    pm_total = Parameter(index=[time, country], unit="μg/m^3")      # from pm25_pollution :pm_total
    pop      = Parameter(index=[time, country])                     # population level (as in R fit)
    gdp      = Parameter(index=[time, country])                     # GDP level (as in R fit)

    # === Draw selector ===
    pm25_dmg_draw = Parameter{Int}()   # 0 = mean of draws, 1..N = specific row
    filesuffix = Parameter{String}()

    # === Coefficients (scalars; filled in init) ===
    fe = Parameter(index=[country])
    β_pm       = Variable()
    β_year     = Variable()
    β_pmyear   = Variable()
    θ_pop      = Variable()
    θ_gdp      = Variable()

    # === Residual variances from R models (for log-normal correction) ===
    # provided by user (var(resid(model3_*)))
    residvar   = Parameter()

    # === Outputs (MUSD/yr) ===
    cost   = Variable(index=[time, country], unit="MUSD/yr")

    # ---- init: pick MC draw (or mean) and load coefficients ----
    function init(p, v, d)
        # expected column order: beta_pm, beta_year, theta_pop, theta_gdp, beta_pm_year
        pm25_params = CSV.read(joinpath(@__DIR__, "../../data/pm25_damages/damage_mvrnorm_$(p.filesuffix).csv"), DataFrame)

        # expected column order in CSVs: beta_pm, beta_pm_year, theta_pop, theta_gdp
        if p.pm25_dmg_draw == 0
            params = mean.(eachcol(pm25_params))
        else
            params = Vector(pm25_params[p.pm25_dmg_draw, :])
        end
        v.β_pm     = hc[1]; v.β_year   = hc[2];
        v.θ_pop    = hc[3]; v.θ_gdp    = hc[4]; v.β_pmyear = hc[5]
    end

    # ---- timestep: vectorized over countries at current time ----
    function run_timestep(p, v, d, t)
        # year scalar
        yr = p.y_year[t]

        # total PM from pollution component (μg/m^3)
        pm = p.pm_total[t, :]

        # floors at 1.0 (guard logs)
        pp = max.(p.pop[t, :], 1.0)
        gd = max.(p.gdp[t, :], 1.0)

        # time-adjusted PM slopes
        β = v.β_pm .+ v.β_pmyear * yr

        # log-costs
        lnC = β .* log.(pm) .+ v.θ_pop .* log.(pp) .+ v.θ_gdp .* log.(gd) .+ v.β_year * yr .+ p.fe_h[:]

        # log-normal bias correction: yhat = exp(logyhat + 0.5 * Var(resid))
        v.cost[t, :]   = exp.(lnC .+ 0.5 * p.residvar)
    end
end

# Helper to add this component and default to "mean of draws"
function add_pm25_damages(model::Model, filesuffix::String, residvar::Float64, compname::Symbol)
    comp = add_comp!(model, PM25Damage, compname)
    comp[:pm25_dmg_draw] = 0

    mapping = CSV.read(pagedata("pollution/GAINS_4letter_regions_mapping.csv"), DataFrame)

    fe_regional = CSV.read(pagedata("pm25_damages/damage_fe_$filesuffix.csv"), DataFrame)
    fe = leftjoin(mapping, fe_regional, on=:REGION_4LETTER => :idx)
    comp[:fe] = readcountrydata_i_const(model, fe, :ISO3, :effect, vv -> mean(skipmissing(vv)))

    comp[:filesuffix] = filesuffix
    comp[:residvar] = residvar

    return comp
end

