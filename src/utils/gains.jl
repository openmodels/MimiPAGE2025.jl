function load_pm25pollution_basedata(model::Model, scenario::String)
    baseline = CSV.read(pagedata("pollution/baseline.csv"), DataFrame, missingstring="NA")
    baseline2 = leftjoin(gains_mapping, baseline, on=:REGION_4LETTER)
    baseline2 = baseline2[.!ismissing.(baseline2.IDYEARS), :]

    baseline2
end

function get_pm25pollution_baserows(model::Model, scenario::String, baseline2::DataFrame, year::Int64)
    if year < 2025
        return get_pm25pollution_baserows(model, scenario, baseline2, 2025)
    elseif year >= 2100
        baseline_after2100 = baseline2[(baseline2.IDYEARS .== 2100) .& (baseline2.IDSCENARIOS .== scenario), :]
        baseline_after2100_page = leftjoin(DataFrame(ISO3=dim_keys(model, :country)), baseline_after2100, on=:ISO3)
        baseline_after2100_page[ismissing.(baseline_after2100_page[!, :PM25_SELF]), :PM25_SELF] .= mean(skipmissing(baseline_after2100_page[!, :PM25_SELF]))
        baseline_after2100_page[ismissing.(baseline_after2100_page[!, :PM25_EXPORT]), :PM25_EXPORT] .= mean(skipmissing(baseline_after2100_page[!, :PM25_EXPORT]))

        baseline_page = baseline_after2100_page
        baseline_year = 2100
    elseif year == 2075
        baseline_page = get_pm25pollution_baserows(model, scenario, baseline2, 2070)[1]
        baseline_page_after = get_pm25pollution_baserows(model, scenario, baseline2, 2080)[1]
        for col in ["POPULATION", "GDP_GUSD2017_PPP", "CO2 Mt CO2/yr", "CH4 kt/yr", "PM25_TOTAL", "PM25_SELF", "PM25_EXPORT", "AP_CONTROL_COSTS_MEUR2015"]
            baseline_page[!, col] = (baseline_page[!, col] + baseline_page_after[!, col]) / 2
        end
        baseline_year = 2075
    else
        baseline_period = baseline2[(baseline2.IDYEARS .== year) .& (baseline2.IDSCENARIOS .== scenario), :]
        baseline_page = leftjoin(DataFrame(ISO3=dim_keys(model, :country)), baseline_period, on=:ISO3)
        baseline_page[ismissing.(baseline_page[!, :PM25_SELF]), :PM25_SELF] .= mean(skipmissing(baseline_page[!, :PM25_SELF]))
        baseline_page[ismissing.(baseline_page[!, :PM25_EXPORT]), :PM25_EXPORT] .= mean(skipmissing(baseline_page[!, :PM25_EXPORT]))

        baseline_year = year
    end

    return baseline_page, baseline_year
end
