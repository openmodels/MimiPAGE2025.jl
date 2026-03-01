@defcomp co2emissions_rcp begin
    country = Index()
    region = Index()

    model = Parameter{Model}()

    e0_baselineCO2emissions = Parameter(index=[region], unit="Mtonne/year")
    er_CO2emissionsgrowth = Parameter(index=[time, region], unit="%")

    e_regionalCO2emissions = Variable(index=[time,region], unit="Mtonne/year")
    e_globalCO2emissions = Variable(index=[time], unit="Mtonne/year")

    # read in counterfactual GDP in absence of growth effects (gdp_baseline) and actual GDP
    gdppc = Parameter(index=[time, country], unit="\$/person")
    pop_population = Parameter(index=[time, country], unit="million person")
    gdp_baseline = Parameter(index=[time, country], unit="\$M")
    emfeed_emissionfeedback = Parameter{Bool}(unit="none", default=true)

    function run_timestep(p, v, d, t)
        # eq.4 in Hope (2006) - regional CO2 emissions as % change from baseline
        for r in d.region
            v.e_regionalCO2emissions[t,r] = p.er_CO2emissionsgrowth[t,r] * p.e0_baselineCO2emissions[r] / 100
        end

        # rescale emissions based on GDP deviation from original scenario pathway
        if !is_first(t) && p.emfeed_emissionfeedback
            calc = countrytoregion(p.model, sum, p.gdppc[t-1, :] .* p.pop_population[t-1, :])
            base = countrytoregion(p.model, sum, p.gdp_baseline[t-1, :])

            for r in d.region
                v.e_regionalCO2emissions[t,r] = v.e_regionalCO2emissions[t,r] * (calc[r] / base[r])
            end

        end

        # eq. 5 in Hope (2006) - global CO2 emissions are sum of regional emissions
        v.e_globalCO2emissions[t] = sum(v.e_regionalCO2emissions[t,:])
    end
end

function addco2emissions_rcp(model::Model, name::Symbol)
    co2emit = add_comp!(model, co2emissions_rcp, name)

    co2emit[:model] = model

    co2emit
end
