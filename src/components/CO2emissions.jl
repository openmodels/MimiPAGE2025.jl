@defcomp co2emissions begin
    country = Index()

    baselineemit = Parameter(index=[time, country], unit="MtCO2/year")
    fracabatedcarbon = Parameter(index=[time, country], unit="portion")

    e_countryCO2emissions = Variable(index=[time, country], unit="Mtonne/year")
    e_globalCO2emissions = Variable(index=[time], unit="Mtonne/year")
    
    # === Variables needed for PM2.5 pollution ===
    logco20        = Variable(index=[time, country], unit="log(MtCO2/year)")
    logco20xyear0  = Variable(index=[time, country], unit="log(MtCO2/year) * year")

    function run_timestep(p, v, d, t)
        idx = TimestepIndex(t.t)

        # eq.4 in Hope (2006) - regional CO2 emissions as % change from baseline
        for cc in d.country
            v.e_countryCO2emissions[idx, cc] = p.baselineemit[t, cc] * (1 - p.fracabatedcarbon[t, cc])
        end

        # eq. 5 in Hope (2006) - global CO2 emissions are sum of regional emissions
        v.e_globalCO2emissions[idx] = sum(v.e_countryCO2emissions[idx, :])

        # Derived variables for PM2.5
        for cc in d.country
            v.logco20[idx, cc] = log(v.e_countryCO2emissions[idx, cc] + 1e-6)  # Avoid log(0)
            v.logco20xyear0[idx, cc] = v.logco20[idx, cc] * (d.time[idx] - 2015) # '2015' is year_0
        end
    end
end




#=
    function run_timestep(p, v, d, t)

        # eq.4 in Hope (2006) - regional CO2 emissions as % change from baseline
        for cc in d.country
            v.e_countryCO2emissions[t,cc] = p.baselineemit[t,cc] * (1 - p.fracabatedcarbon[t, cc])
        end
        # eq. 5 in Hope (2006) - global CO2 emissions are sum of regional emissions
        v.e_globalCO2emissions[t] = sum(v.e_countryCO2emissions[t,:])
    end
end
=#





