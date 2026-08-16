@defcomp n2oemissions begin
    region = Index()

    gains_match = Parameter{Bool}()
    e_globalN2Oemissions_gains = Parameter(index=[time], unit="Mtonne/year")

    e_globalN2Oemissions = Variable(index=[time], unit="Mtonne/year")
    e0_baselineN2Oemissions = Parameter(index=[region], unit="Mtonne/year")
    e_regionalN2Oemissions = Variable(index=[time,region], unit="Mtonne/year")
    er_N2Oemissionsgrowth = Parameter(index=[time,region], unit="%")

    # read in counterfactual GDP in absence of growth effects (gdp_baseline) and actual GDP
    gdppc_region = Parameter(index=[time, region], unit="\$/person")
    pop_population_region = Parameter(index=[time, region], unit="million person")
    gdp_baseline_region = Parameter(index=[time, region], unit="\$M")
    emfeed_emissionfeedback = Parameter{Bool}(unit="none", default=true)

    function run_timestep(p, v, d, t)
        # note that Hope (2009) states that Equations 1-12 for methane also apply to N2O

        # eq.4 in Hope (2006) - regional N2O emissions as % change from baseline
        for r in d.region
            v.e_regionalN2Oemissions[t,r] = p.er_N2Oemissionsgrowth[t,r] * p.e0_baselineN2Oemissions[r] / 100

            # rescale emissions based on GDP deviation from original scenario pathway
            if  !is_first(t) && p.emfeed_emissionfeedback
                v.e_regionalN2Oemissions[t, r] = v.e_regionalN2Oemissions[t, r] * (p.gdppc_region[t-1, r] * p.pop_population_region[t-1, r] / p.gdp_baseline_region[t-1, r])
            end
        end

        if p.gains_match
            n2opt = p.e_globalN2Oemissions_gains[t] / sum(v.e_regionalN2Oemissions[t, :])
            for rr in d.region
                v.e_regionalN2Oemissions[t, rr] = n2opt * v.e_regionalN2Oemissions[t, rr]
            end
        end

        # eq. 5 in Hope (2006) - global N2O emissions are sum of regional emissions
        v.e_globalN2Oemissions[t] = sum(v.e_regionalN2Oemissions[t,:])
    end
end

function addn2oemissions(model::Model, use_gains_n2o::Bool, gains_scenario::String)
    n2oemit = add_comp!(model, n2oemissions)

    gains_n2o = zeros(dim_count(model, :time))

    if use_gains_n2o
        emits = load_gains_emissions(model, gains_scenario)

        for tt in 1:dim_count(model, :time)
            gains_n2o[tt] = get_gains_value(emits, dim_keys(model, :time)[tt], "N2O_kt/yr", 7.205451799 * 1e3) / 1e3 # value from FaIR SSP245
        end
    end

    n2oemit[:gains_match] = use_gains_n2o
    n2oemit[:e_globalN2Oemissions_gains] = gains_n2o

    return n2oemit

end
