

@defcomp TotalAbatementCosts begin
    region = Index()
    country = Index()

    model = Parameter{Model}()
    tct_totalcosts_match = Parameter(index=[time], unit="billion US\$2010/yr")
    gainsmatch = Parameter{Bool}()

    tc_totalcosts_co2 = Parameter(index=[time, country], unit="\$million")
    tc_totalcosts_ch4 = Parameter(index=[time, region], unit="\$million")
    tc_totalcosts_n2o = Parameter(index=[time, region], unit="\$million")
    tc_totalcosts_linear = Parameter(index=[time, region], unit="\$million")
    gdp = Parameter(index=[time, country], unit="million US\$2005/yr")
    pop_population = Parameter(index=[time, country], unit="million person")
    pop_population_region = Parameter(index=[time, region], unit="million person")

    tct_totalcosts = Variable(index=[time, country], unit="million US\$2005/yr")
    tct_percap_totalcostspercap = Variable(index=[time, country], unit="\$/person")
    tct_percap_totalcostspercap_region = Variable(index=[time, region], unit="\$/person")

    function run_timestep(p, v, d, t)
        tct_totalcosts_partial_region = p.tc_totalcosts_n2o[t, :] .+ p.tc_totalcosts_ch4[t, :] .+ p.tc_totalcosts_linear[t, :] # $million
        tct_percap_totalcostspercap_partial_region = tct_totalcosts_partial_region ./ p.pop_population_region[t, :] # $/person

        tct_percap_totalcostspercap_partial = regiontocountry(p.model, tct_percap_totalcostspercap_partial_region) # $/person

        for cc in d.country
            v.tct_totalcosts[t, cc] = p.tc_totalcosts_co2[t, cc] + tct_percap_totalcostspercap_partial[cc] * p.pop_population[t, cc] # $million
        end

        # tct_saved = v.tct_totalcosts[t, :]
        for cc in d.country
            if p.gainsmatch
                # v.tct_totalcosts[t, cc] = (p.tct_totalcosts_match[t] * 1000 * 63.23579 / 69.49899) * tct_saved[cc] / sum(skipmissing(tct_saved))
                v.tct_totalcosts[t, cc] = (p.tct_totalcosts_match[t] * 1000 * 63.23579 / 69.49899) * p.gdp[t, cc] / sum(p.gdp[t, :])
            end

            v.tct_percap_totalcostspercap[t, cc] = v.tct_totalcosts[t, cc] / p.pop_population[t, cc] # $/person
        end

        v.tct_percap_totalcostspercap_region[t, :] = countrytoregion(p.model, mean, v.tct_percap_totalcostspercap[t, :])
    end
end

function get_climatecosts_value(coldat, climatecosts, year)
    if year < 2025
        return coldat[climatecosts.YEAR .== 2025][1]
    elseif year > 2100
        return coldat[climatecosts.YEAR .== 2100][1]
    elseif year == 2075
        return (coldat[climatecosts.YEAR .== 2070][1] + coldat[climatecosts.YEAR .== 2080][1]) / 2
    else
        return coldat[climatecosts.YEAR .== year][1]
    end
end

function addtotalabatementcosts(model::Model, gainsmatch::Bool, scenario::String)
    totalabatementcosts = add_comp!(model, TotalAbatementCosts)

    totalabatementcosts[:model] = model
    totalabatementcosts[:gainsmatch] = gainsmatch
    totalabatementcosts[:tct_totalcosts_match] = zeros(dim_count(model, :time))
    if gainsmatch
        climatecosts = CSV.read(pagedata("climate/climatecosts.csv"), DataFrame)

        if scenario == "Baseline"
            totalabatementcosts[:tct_totalcosts_match] = [get_climatecosts_value(climatecosts.Baseline, climatecosts, year) for year in dim_keys(model, :time)]
        else
            totalabatementcosts[:tct_totalcosts_match] = [get_climatecosts_value(climatecosts.Decarb, climatecosts, year) for year in dim_keys(model, :time)]
        end
    end

    totalabatementcosts
end
