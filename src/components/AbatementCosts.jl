@defcomp AbatementCosts begin
    region = Index()
    country = Index()

    # From the AbatementCostParameters
    zc_zerocostemissions = Parameter(index=[time, region], unit="%")
    q0_absolutecutbacksatnegativecost = Parameter(index=[time, region], unit="Mtonne")
    blo = Parameter(index=[time, region], unit="per Mtonne")
    alo = Parameter(index=[time, region], unit="\$/tonne")
    bhi = Parameter(index=[time, region], unit="per Mtonne")
    ahi = Parameter(index=[time, region], unit="\$/tonne")

    # Driver of abatement costs
    e0_baselineemissions = Parameter(index=[region], unit="Mtonne/year")
    er_emissionsgrowth = Parameter(index=[time, region], unit="%")

    # Intermediate outputs
    cb_reductionsfromzerocostemissions = Variable(index=[time, region], unit="%")
    cbe_absoluteemissionreductions = Variable(index=[time, region], unit="Mtonne") # Goes to AbatementCostParameters

    # Main costs results
    mc_marginalcost = Variable(index=[time, region], unit="\$/tonne")
    tcq0 = Variable(index=[time, region], unit="\$million")
    tc_totalcost = Variable(index=[time, region], unit="\$million")

    # For mix-and-match
    model = Parameter{Model}()
    gdp = Parameter(index=[time, region], unit="\$M")
    gdp_national = Parameter(index=[time, country], unit="\$M")
    tc_totalcost_national = Variable(index=[time, country], unit="\$million")
    mac_draw = Parameter{Int64}()

    function run_timestep(p, v, d, t)

        for r in d.region
            v.cb_reductionsfromzerocostemissions[t,r] = max(p.zc_zerocostemissions[t,r] - p.er_emissionsgrowth[t,r], 0)
            v.cbe_absoluteemissionreductions[t,r] = v.cb_reductionsfromzerocostemissions[t,r] * p.e0_baselineemissions[r] / 100

            if v.cbe_absoluteemissionreductions[t,r] < p.q0_absolutecutbacksatnegativecost[t,r]
                v.mc_marginalcost[t,r] = p.alo[t,r] * (exp(p.blo[t,r] * (v.cbe_absoluteemissionreductions[t,r] - p.q0_absolutecutbacksatnegativecost[t,r])) - 1)
            else
                v.mc_marginalcost[t,r] = p.ahi[t,r] * (exp(p.bhi[t,r] * (v.cbe_absoluteemissionreductions[t,r] - p.q0_absolutecutbacksatnegativecost[t,r])) - 1)
            end

            if p.q0_absolutecutbacksatnegativecost[t,r] == 0.
                v.tcq0[t,r] = 0.
            else
                v.tcq0[t,r] = (p.alo[t,r] / p.blo[t,r]) * (1 - exp(-p.blo[t,r] * p.q0_absolutecutbacksatnegativecost[t,r])) - p.alo[t,r] * p.q0_absolutecutbacksatnegativecost[t,r]
            end

            if v.cbe_absoluteemissionreductions[t,r] < p.q0_absolutecutbacksatnegativecost[t,r]
                v.tc_totalcost[t,r] = (p.alo[t,r] / p.blo[t,r]) * (exp(p.blo[t,r] * (v.cbe_absoluteemissionreductions[t,r] - p.q0_absolutecutbacksatnegativecost[t,r])) - exp(-p.blo[t,r] * p.q0_absolutecutbacksatnegativecost[t,r])) - p.alo[t,r] * v.cbe_absoluteemissionreductions[t,r]
            else
                v.tc_totalcost[t,r] = (p.ahi[t,r] / p.bhi[t,r]) * (exp(p.bhi[t,r] * (v.cbe_absoluteemissionreductions[t,r] - p.q0_absolutecutbacksatnegativecost[t,r])) - 1) - p.ahi[t,r] * (v.cbe_absoluteemissionreductions[t,r] - p.q0_absolutecutbacksatnegativecost[t,r]) + v.tcq0[t,r]
            end
        end

        v.tc_totalcost_national[t, :] = regiontocountry(p.model, v.tc_totalcost[t, :] ./ p.gdp[t, :]) .* p.gdp_national[t, :]
    end
end

function addabatementcosts(model::Model, class::Symbol)
    componentname = Symbol("AbatementCosts$class")
    abatementcostscomp = add_comp!(model, AbatementCosts, componentname)

    if class == :CO2
        setdistinctparameter(model, componentname, :e0_baselineemissions, readpagedata(model, "data/e0_baselineCO2emissions.csv"))
    elseif class == :CH4
        ## :e0_baselineemissions set in model
    elseif class == :N2O
        setdistinctparameter(model, componentname, :e0_baselineemissions, readpagedata(model, "data/e0_baselineN2Oemissions.csv"))
    elseif class == :Lin
        setdistinctparameter(model, componentname, :e0_baselineemissions, readpagedata(model, "data/e0_baselineLGemissions.csv"))
    else
        error("Unknown class of abatement costs.")
    end

    abatementcostscomp[:model] = model
    abatementcostscomp[:mac_draw] = 0

    return abatementcostscomp
end
