@defcomp MarketDamageAQ_Generic begin
    country = Index()
    global_ch4_emissions = Parameter(index=[time], unit="Mtonne/year")
    per_mton_ch4 = Parameter(index=[country, 50], unit="\$/Mtonne")
    y_year_0 = Parameter(unit="year")
    y_year = Parameter(index=[10], unit="year")
    historical_emissions = Parameter(index=[50], unit="Mtonne/year") 

    total_market_damage = Variable(index=[time, country], unit="\$")

    function run_timestep(p, v, d, t)
        for c in d.country
            v.total_market_damage[t, c] = 0
            for tt in 1:50
                uu = gettime(t) - tt + 1
                if uu >= 1970 && uu <= 2019
                    v.total_market_damage[t, c] += p.historical_emissions[tt] * p.per_mton_ch4[c, tt]
                elseif uu >= 2020 && uu <= 2300
                    if uu in p.y_year
                        idx = findfirst(x -> x == uu, p.y_year)
                        e = p.global_ch4_emissions[TimestepIndex(idx)]
                        v.total_market_damage[t, c] += e * p.per_mton_ch4[c, tt]
                    else
                        i1, i2 = find_model_years(uu, p.y_year)
                        y1, y2 = p.y_year[i1], p.y_year[i2]
                        e1 = p.global_ch4_emissions[TimestepIndex(i1)]
                        e2 = p.global_ch4_emissions[TimestepIndex(i2)]
                        interpolated_emission = e1 + (e2 - e1) * (uu - y1) / (y2 - y1)
                        v.total_market_damage[t, c] += interpolated_emission * p.per_mton_ch4[c, tt]
                    end
                end
            end
        end
    end
end

function addMarketDamageAQ_Generic(model, instance_name, data_file, historical_emission_file)
    impact_data = myloadcsv(data_file)
    formatteddata = zeros(dim_count(model, :country), 50)
    for t in 1:50
        formatteddata[:, t] = readcountrydata_i_const(model, impact_data, :ISO3, Symbol(t))
    end

    historical_emissions_df = myloadcsv(historical_emission_file)
    historical_emissions_vec = [Float64(x) for x in historical_emissions_df[2, :]]

    comp = add_comp!(model, MarketDamageAQ_Generic, instance_name)
    comp[:y_year_0] = 2015.
    comp[:y_year] = [2020, 2030, 2040, 2050, 2075, 2100, 2150, 2200, 2250, 2300]
    comp[:per_mton_ch4] = formatteddata
    comp[:historical_emissions] = historical_emissions_vec
    return comp
end

function find_model_years(uu, y_year)
    i2 = findfirst(x -> x >= uu, y_year)
    if i2 === nothing || i2 == 1
        error("uu=$uu is out of the range of y_year=$(y_year)")
    end
    i1 = i2 - 1
    return i1, i2
end
