methane_on_lostworkhours = myloadcsv("data/MarketDamageAQ/Methane_LostWorkHours.csv")
historical_emissions = myloadcsv("data/MarketDamageAQ/Historical_MethaneEmissions.csv")

function find_model_years(uu, model_years)
    for i in 1:(length(model_years) - 1)
        if model_years[i] <= uu && uu <= model_years[i+1]
            return i, i+1
        end
    end
    error("Year not found between model years.")
end

function addMarketDamageAQ_LostWorkHours(model::Model)
    marketdamageaq_lostworkhours = add_comp!(model, MarketDamageAQ_LostWorkHours)
    formatteddata = zeros(dim_count(model, :country), 50)
    for t in 1:50
        formatteddata[:, t] = readcountrydata_i_const(model, methane_on_lostworkhours, :ISO3, Symbol(t))
    end

    # The year can be passed through model parameters
    marketdamageaq_lostworkhours[:y_year_0] = 2015.
    marketdamageaq_lostworkhours[:y_year] = [2020, 2030, 2040, 2050, 2075, 2100, 2150, 2200, 2250, 2300]

    marketdamageaq_lostworkhours[:lost_work_per_mton_ch4] = formatteddata
    return marketdamageaq_lostworkhours
end

@defcomp MarketDamageAQ_LostWorkHours begin
    country = Index()

    # Incoming parameter: Global CH4 emissions from ch4emissions component
    global_ch4_emissions = Parameter(index=[time], unit="Mtonne/year")

    # Impact parameters: value per Mt methane emitted in year 1, year 2, ..., year 50
    lost_work_per_mton_ch4 = Parameter(index=[country, 50], unit="\$/Mtonne")

    # Component variable to store total lost work hours yield value
    total_methane_on_lostworkhours = Variable(index=[time, country], unit="\$")

    # Define the y_year-0 parameter
    y_year_0 = Parameter(unit="year")

    # Define the y_year parameter (for interpolation)
    y_year = Parameter(index=[10], unit="year")  # 10 time points

    
    function run_timestep(p, v, d, t)
        for c in d.country
            v.total_methane_on_lostworkhours[t, c] = 0

            for tt in 1:50
                uu = gettime(t) - tt + 1  # Current Year
                
                # Using Historical Data
                if uu >= 1970 && uu <= 2019
                    v.total_methane_on_lostworkhours[t, c] += historical_emissions[2, string(uu)] * p.lost_work_per_mton_ch4[c, tt]
                elseif uu >= 2020 && uu <= 2300
                    if uu in p.y_year
                        idx = findfirst(x -> x == uu, p.y_year)
                        println(idx)
                        e = p.global_ch4_emissions[TimestepIndex(idx)]
                        v.total_methane_on_lostworkhours[t, c] += e * p.lost_work_per_mton_ch4[c, tt]
                    else
                        i1, i2 = find_model_years(uu, p.y_year)
                        y1, y2 = p.y_year[i1], p.y_year[i2]
                        e1 = p.global_ch4_emissions[TimestepIndex(i1)]
                        e2 = p.global_ch4_emissions[TimestepIndex(i2)]
                        interpolated_emission = e1 + (e2 - e1) * (uu - y1) / (y2 - y1)
                        v.total_methane_on_lostworkhours[t, c] += interpolated_emission * p.lost_work_per_mton_ch4[c, tt]
                    end
                end
            end
        end
    end
end

