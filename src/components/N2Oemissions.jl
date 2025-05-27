@defcomp n2oemissions begin
    region = Index()

    e_globalN2Oemissions = Variable(index=[time], unit="Mtonne/year")
    e0_baselineN2Oemissions = Parameter(index=[region], unit="Mtonne/year")
    e_regionalN2Oemissions = Variable(index=[time,region], unit="Mtonne/year")
    er_N2Oemissionsgrowth = Parameter(index=[time,region], unit="%")

    # read in counterfactual GDP in absence of growth effects (gdp_leveleffects) and actual GDP
    gdp_region = Parameter(index=[time, region], unit="\$M")
    gdp_leveleffect_region = Parameter(index=[time, region], unit="\$M")
    emfeed_emissionfeedback = Parameter{Bool}(unit="none", default=true)

    function run_timestep(p, v, d, t)
        # note that Hope (2009) states that Equations 1-12 for methane also apply to N2O

        # eq.4 in Hope (2006) - regional N2O emissions as % change from baseline
        for r in d.region
            v.e_regionalN2Oemissions[t,r] = p.er_N2Oemissionsgrowth[t,r] * p.e0_baselineN2Oemissions[r] / 100

            # rescale emissions based on GDP deviation from original scenario pathway
            if p.emfeed_emissionfeedback
                v.e_countryCO2emissions[t, r] = v.e_countryCO2emissions[t, r] * (p.gdp_region[t, r] / p.gdp_leveleffect_region[t, r])
            end
        end

        # eq. 5 in Hope (2006) - global N2O emissions are sum of regional emissions
        v.e_globalN2Oemissions[t] = sum(v.e_regionalN2Oemissions[t,:])
    end
end
