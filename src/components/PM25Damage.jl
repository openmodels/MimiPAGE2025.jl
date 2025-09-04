# ==============================================================================
# PM2.5 Damages Component
#
# Computes three cost components (MUSD/yr) per country & year using your
# chosen R Model 3 specification:
#   ln(C_k) = α_k
#             + (β_pm_k + β_pm_year_k * YEAR[t]) * PM25_TOTAL[t,c]
#             + θ_pop_k * ln(POP[t,c])
#             + θ_gdp_k * ln(GDP[t,c])
#   C_k = exp(ln(C_k))
#
# PM25_TOTAL is reconstructed internally from PM logs:
#   PM25_TOTAL[t,c] = exp(logpm_self[t,c]) + exp(logpm_export[t,c])
#
# Coefficients come from Monte Carlo draws stored in CSVs.
# ==============================================================================

using Mimi
using DataFrames
using CSV

# ---- Load Monte Carlo draws once ----
if !isdefined(Main, :pm25_hc_params)
    #global const pm25_hc_params = CSV.read("../data/pm25_damages/damage_mvrnorm_healthcare.csv", DataFrame)
    global const pm25_hc_params = CSV.read(joinpath(@__DIR__, "../../data/pm25_damages/damage_mvrnorm_healthcare.csv"), DataFrame)

end
if !isdefined(Main, :pm25_prod_params)
    #global const pm25_prod_params = CSV.read("../data/pm25_damages/damage_mvrnorm_productivity.csv", DataFrame)
    global const pm25_prod_params = CSV.read(joinpath(@__DIR__, "../../data/pm25_damages/damage_mvrnorm_productivity.csv"), DataFrame)

end
if !isdefined(Main, :pm25_dis_params)
    #global const pm25_dis_params = CSV.read("../data/pm25_damages/damage_mvrnorm_disutility.csv", DataFrame)
    global const pm25_dis_params = CSV.read(joinpath(@__DIR__, "../../data/pm25_damages/damage_mvrnorm_disutility.csv"), DataFrame)

end

@defcomp pm25_damages begin
    country = Index()
    time    = Index()

    # === Inputs ===
    y_year        = Parameter(index=[time], unit="year")    # calendar year (initpage fills this)
    pm_log_self   = Parameter(index=[time, country])        # from pm25_pollution :logpm_self
    pm_log_export = Parameter(index=[time, country])        # from pm25_pollution :logpm_export
    pop           = Parameter(index=[time, country])        # population level (as in R fit)
    gdp           = Parameter(index=[time, country])        # GDP level (as in R fit)
    sigma_min     = Parameter(default = 1e-12)              # numeric floor for logs

    # === Draw selector ===
    pm25_dmg_draw = Parameter{Int}()   # 0 = mean of draws, 1..N = specific row

    # === Coefficients (scalars; filled in init) ===
    # Healthcare
    β_h_pm       = Variable()
    β_h_pmyear   = Variable()
    θ_h_pop      = Variable()
    θ_h_gdp      = Variable()
    # Productivity
    β_p_pm       = Variable()
    β_p_pmyear   = Variable()
    θ_p_pop      = Variable()
    θ_p_gdp      = Variable()
    # Disutility
    β_d_pm       = Variable()
    β_d_pmyear   = Variable()
    θ_d_pop      = Variable()
    θ_d_gdp      = Variable()

    # Optional intercepts (default 0; calibrate later if desired)
    alpha_healthcare   = Parameter(default = 0.0)
    alpha_productivity = Parameter(default = 0.0)
    alpha_disutility   = Parameter(default = 0.0)

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
        v.β_h_pm     = hc[1]; v.β_h_pmyear = hc[2]
        v.θ_h_pop    = hc[3]; v.θ_h_gdp    = hc[4]
        # Productivity
        v.β_p_pm     = pr[1]; v.β_p_pmyear = pr[2]
        v.θ_p_pop    = pr[3]; v.θ_p_gdp    = pr[4]
        # Disutility
        v.β_d_pm     = di[1]; v.β_d_pmyear = di[2]
        v.θ_d_pop    = di[3]; v.θ_d_gdp    = di[4]
    end

    # ---- timestep: vectorized over countries at current time ----
    function run_timestep(p, v, d, t)
    idx = Mimi.TimestepIndex(t.t)   # ← add this

    yr  = p.y_year[idx]             # was p.y_year[t.t]
    σ   = p.sigma_min

    pm  = exp.(p.pm_log_self[idx, :]) .+ exp.(p.pm_log_export[idx, :])   # was [... t.t, :]
    pp  = max.(p.pop[idx, :], σ)                                         # was [... t.t, :]
    gd  = max.(p.gdp[idx, :], σ)                                         # was [... t.t, :]

    βh = v.β_h_pm .+ v.β_h_pmyear * yr
    βp = v.β_p_pm .+ v.β_p_pmyear * yr
    βd = v.β_d_pm .+ v.β_d_pmyear * yr

    lnCh = p.alpha_healthcare   .+ βh .* pm .+ v.θ_h_pop .* log.(pp) .+ v.θ_h_gdp .* log.(gd)
    lnCp = p.alpha_productivity .+ βp .* pm .+ v.θ_p_pop .* log.(pp) .+ v.θ_p_gdp .* log.(gd)
    lnCd = p.alpha_disutility   .+ βd .* pm .+ v.θ_d_pop .* log.(pp) .+ v.θ_d_gdp .* log.(gd)

    v.cost_healthcare[idx, :]   = exp.(lnCh)   # was [... t.t, :]
    v.cost_productivity[idx, :] = exp.(lnCp)
    v.cost_disutility[idx, :]   = exp.(lnCd)
    end
end


# Helper to add this component and default to "mean of draws", mirroring add_pm25_pollution
function add_pm25_damages(model::Model)
    comp = add_comp!(model, pm25_damages)
    comp[:pm25_dmg_draw] = 0
    return comp
end
