include("../utils/gains.jl")

@defcomp co2emissions begin
    country = Index()

    baselineemit = Parameter(index=[time, country], unit="MtCO2/year")
    fracabatedcarbon = Parameter(index=[time, country], unit="portion")

    gains_match = Parameter{Bool}()
    e_globalCO2emissions_gains = Parameter(index=[time], unit="MtCO2/year")

    e_countryCO2emissions = Variable(index=[time, country], unit="Mtonne/year")
    e_globalCO2emissions = Variable(index=[time], unit="Mtonne/year")

    # read in counterfactual GDP in absence of growth effects (gdp_baseline) and actual GDP
    gdppc = Parameter(index=[time, country], unit="\$/person")
    pop_population = Parameter(index=[time, country], unit="million person")
    gdp_baseline = Parameter(index=[time, country], unit="million US\$2005/yr")
    emfeed_emissionfeedback = Parameter{Bool}(unit="none", default=true)

    function run_timestep(p, v, d, t)
        # eq.4 in Hope (2006) - regional CO2 emissions as % change from baseline
        for cc in d.country
            v.e_countryCO2emissions[t, cc] = p.baselineemit[t, cc] * (1 - p.fracabatedcarbon[t, cc])

            # rescale emissions based on GDP deviation from original scenario pathway
            if !is_first(t) && p.emfeed_emissionfeedback
                v.e_countryCO2emissions[t, cc] = v.e_countryCO2emissions[t, cc] * (p.gdppc[t-1, cc] * p.pop_population[t-1, cc] / p.gdp_baseline[t-1, cc])
            end
        end

        if p.gains_match
            co2pt = p.e_globalCO2emissions_gains[t] / sum(v.e_countryCO2emissions[t, :])
            for cc in d.country
                v.e_countryCO2emissions[t, cc] = co2pt * v.e_countryCO2emissions[t, cc]
            end
        end

        # eq. 5 in Hope (2006) - global CO2 emissions are sum of regional emissions
        v.e_globalCO2emissions[t] = sum(v.e_countryCO2emissions[t,:])
    end
end

function addco2emissions(model::Model, use_gains_co2::Bool, gains_scenario::String)
    co2emit = add_comp!(model, co2emissions)

    gains_co2 = zeros(Union{Missing, Float64}, dim_count(model, :time))

    if use_gains_co2
        baseline2 = load_pm25pollution_basedata(model, gains_scenario)

        for tt in 1:dim_count(model, :time)
            if tt == 1
                gains_co2[tt] = 37.09*1e3 # From 2019 World In Data, https://ourworldindata.org/co2-emissions
            else
                baseline_page, baseline_page_year = get_pm25pollution_baserows(model, gains_scenario, baseline2, dim_keys(model, :time)[tt])
                gains_co2[tt] = baseline_page."CO2 Mt CO2/yr"[1]
            end
        end
    end

    co2emit[:gains_match] = use_gains_co2
    co2emit[:e_globalCO2emissions_gains] = gains_co2

    return co2emit

end
