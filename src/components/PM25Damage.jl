
# ==============================================================================
# PM2.5 Damages Component
#
# Computes three cost components (MUSD/yr) per country & year using your
# chosen R Model 3 specification (estimated in logs):
#
#   ln(C_k) = α_k
#             + (β_pm_k + β_pm_year_k * YEAR[t]) * PM25_TOTAL[t,c]
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

# ---- Load Monte Carlo draws once (healthcare, productivity, disutility) ----
if !isdefined(Main, :pm25_hc_params)
    # expected column order: beta_pm, beta_year, theta_pop, theta_gdp, beta_pm_year
    global const pm25_hc_params = CSV.read(joinpath(@__DIR__, "../../data/pm25_damages/damage_mvrnorm_healthcare.csv"), DataFrame)
end
if !isdefined(Main, :pm25_prod_params)
    global const pm25_prod_params = CSV.read(joinpath(@__DIR__, "../../data/pm25_damages/damage_mvrnorm_productivity.csv"), DataFrame)
end
if !isdefined(Main, :pm25_dis_params)
    global const pm25_dis_params = CSV.read(joinpath(@__DIR__, "../../data/pm25_damages/damage_mvrnorm_disutility.csv"), DataFrame)
end

@defcomp pm25_damages begin
    country = Index()
    time    = Index()

    # === Inputs ===
    y_year   = Parameter(index=[time], unit="year")                 # calendar year (initpage fills this)
    pm_total = Parameter(index=[time, country], unit="μg/m^3")      # from pm25_pollution :pm_total
    pop      = Parameter(index=[time, country])                     # population level (as in R fit)
    gdp      = Parameter(index=[time, country])                     # GDP level (as in R fit)

    # === Draw selector ===
    pm25_dmg_draw = Parameter{Int}()   # 0 = mean of draws, 1..N = specific row

    # === Coefficients (scalars; filled in init) ===
    # Healthcare
    fe_h = Parameter(index=[country])
    β_h_pm       = Variable()
    β_h_year     = Variable()
    β_h_pmyear   = Variable()
    θ_h_pop      = Variable()
    θ_h_gdp      = Variable()
    # Productivity
    fe_p = Parameter(index=[country])
    β_p_pm       = Variable()
    β_p_year     = Variable()
    β_p_pmyear   = Variable()
    θ_p_pop      = Variable()
    θ_p_gdp      = Variable()
    # Disutility
    fe_d = Parameter(index=[country])
    β_d_pm       = Variable()
    β_d_year     = Variable()
    β_d_pmyear   = Variable()
    θ_d_pop      = Variable()
    θ_d_gdp      = Variable()

    # === Residual variances from R models (for log-normal correction) ===
    # provided by user (var(resid(model3_*)))
    residvar_healthcare   = Parameter(default = 0.190420543976005)
    residvar_productivity = Parameter(default = 0.190723668649814)
    residvar_disutility   = Parameter(default = 0.191502604324298)

    # === Outputs (MUSD/yr) ===
    cost_healthcare   = Variable(index=[time, country], unit="MUSD/yr")
    cost_productivity = Variable(index=[time, country], unit="MUSD/yr")
    cost_disutility   = Variable(index=[time, country], unit="MUSD/yr")

    # ---- init: pick MC draw (or mean) and load coefficients ----
    function init(p, v, d)
        # expected column order in CSVs: beta_pm, beta_pm_year, theta_pop, theta_gdp
        if p.pm25_dmg_draw == 0
            hc = mean.(eachcol(pm25_hc_params))
            pr = mean.(eachcol(pm25_prod_params))
            di = mean.(eachcol(pm25_dis_params))
        else
            hc = Vector(pm25_hc_params[p.pm25_dmg_draw, :])
            pr = Vector(pm25_prod_params[p.pm25_dmg_draw, :])
            di = Vector(pm25_dis_params[p.pm25_dmg_draw, :])
        end
        # Healthcare
        v.β_h_pm     = hc[1]; v.β_h_year   = hc[2];
        v.θ_h_pop    = hc[3]; v.θ_h_gdp    = hc[4]; v.β_h_pmyear = hc[5]
        # Productivity
        v.β_p_pm     = hc[1]; v.β_p_year   = hc[2];
        v.θ_p_pop    = hc[3]; v.θ_p_gdp    = hc[4]; v.β_p_pmyear = hc[5]
        # Disutility
        v.β_d_pm     = hc[1]; v.β_d_year   = hc[2];
        v.θ_d_pop    = hc[3]; v.θ_d_gdp    = hc[4]; v.β_d_pmyear = hc[5]
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
        βh = v.β_h_pm .+ v.β_h_pmyear * yr
        βp = v.β_p_pm .+ v.β_p_pmyear * yr
        βd = v.β_d_pm .+ v.β_d_pmyear * yr

        # log-costs
        lnCh = βh .* pm .+ v.θ_h_pop .* log.(pp) .+ v.θ_h_gdp .* log.(gd) .+ v.β_h_year * yr .+ p.fe_h[:]
        lnCp = βp .* pm .+ v.θ_p_pop .* log.(pp) .+ v.θ_p_gdp .* log.(gd) .+ v.β_p_year * yr .+ p.fe_p[:]
        lnCd = βd .* pm .+ v.θ_d_pop .* log.(pp) .+ v.θ_d_gdp .* log.(gd) .+ v.β_d_year * yr .+ p.fe_d[:]

        # log-normal bias correction: yhat = exp(logyhat + 0.5 * Var(resid))
        v.cost_healthcare[t, :]   = exp.(lnCh .+ 0.5 * p.residvar_healthcare)
        v.cost_productivity[t, :] = exp.(lnCp .+ 0.5 * p.residvar_productivity)
        v.cost_disutility[t, :]   = exp.(lnCd .+ 0.5 * p.residvar_disutility)
    end
end

# Helper to add this component and default to "mean of draws"
function add_pm25_damages(model::Model)
    comp = add_comp!(model, pm25_damages)
    comp[:pm25_dmg_draw] = 0

    mapping = CSV.read(pagedata("pollution/GAINS_4letter_regions_mapping.csv"), DataFrame)

    fe_h_regional = CSV.read(pagedata("pm25_damages/damage_fe_healthcare.csv"), DataFrame)
    fe_h = leftjoin(mapping, fe_h_regional, on=:REGION_4LETTER => :idx)
    comp[:fe_h] = readcountrydata_i_const(model, fe_h, :ISO3, :effect, vv -> mean(skipmissing(vv)))

    fe_p_regional = CSV.read(pagedata("pm25_damages/damage_fe_productivity.csv"), DataFrame)
    fe_p = leftjoin(mapping, fe_p_regional, on=:REGION_4LETTER => :idx)
    comp[:fe_p] = readcountrydata_i_const(model, fe_p, :ISO3, :effect, vv -> mean(skipmissing(vv)))

    fe_d_regional = CSV.read(pagedata("pm25_damages/damage_fe_disutility.csv"), DataFrame)
    fe_d = leftjoin(mapping, fe_d_regional, on=:REGION_4LETTER => :idx)
    comp[:fe_d] = readcountrydata_i_const(model, fe_d, :ISO3, :effect, vv -> mean(skipmissing(vv)))

    return comp
end

