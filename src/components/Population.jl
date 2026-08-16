include("../utils/country_tools.jl")

gains_mapping = CSV.read(pagedata("pollution/GAINS_4letter_regions_mapping.csv"), DataFrame)

@defcomp Population begin
    region = Index()
    country = Index()

    model = Parameter{Model}()
    pop_match = Parameter(index=[time, country], unit="people")
    gainsmatch = Parameter{Bool}()

    # Parameters
    y_year_0 = Parameter(unit="year")
    y_year = Parameter(index=[time], unit="year")
    popgrw_populationgrowth = Parameter(index=[time, country], unit="%/year") # From p.32 of Hope 2009
    pop0_initpopulation = Parameter(index=[country], unit="million person") # Population in y_year_0

    # Variables
    pop0_initpopulation_region = Variable(index=[region], unit="million person") # Population in y_year_0
    pop_population = Variable(index=[time, country], unit="million person")
    pop_population_region = Variable(index=[time, region], unit="million person")

    function init(p, v, d)
        byregion = countrytoregion(p.model, sum, p.pop0_initpopulation)
        for rr in d.region
            v.pop0_initpopulation_region[rr] = byregion[rr]
        end
    end

    function run_timestep(p, v, d, tt)
        for cc in d.country
            # Eq.28 in Hope 2002 (defined for GDP, but also applies to population)
            if is_first(tt)
                v.pop_population[tt, cc] = p.pop0_initpopulation[cc] * (1 + p.popgrw_populationgrowth[tt, cc] / 100)^(p.y_year[tt] - p.y_year_0)
            else
                v.pop_population[tt, cc] = v.pop_population[tt - 1, cc] * (1 + p.popgrw_populationgrowth[tt, cc] / 100)^(p.y_year[tt] - p.y_year[tt - 1])
            end
        end

        if p.gainsmatch
            for cc in d.country
                if p.pop_match[tt, cc] > 0
                    v.pop_population[tt, cc] = p.pop_match[tt, cc] / 1e6
                end
            end
        end

        v.pop_population_region[tt, :] = countrytoregion(p.model, sum, v.pop_population[tt, :])
    end
end

# Still need this function in order to set the parameters than depend on
# readpagedata, which takes model as an input. These cannot be set using
# the default keyword arg for now.
function addpopulation(model::Model, gainsmatch::Bool, scenario::String)
    populationcomp = add_comp!(model, Population)
    populationcomp[:model] = model

    populationcomp[:gainsmatch] = gainsmatch
    populationcomp[:pop_match] = zeros(dim_count(model, :time), dim_count(model, :country))
    if gainsmatch
        baseline = CSV.read(pagedata("pollution/baseline.csv"), DataFrame, missingstring="NA")
        mapping = leftjoin(get_countryinfo(), gains_mapping, on=:ISO3)
        mapping.REGION_4LETTER[ismissing.(mapping.REGION_4LETTER)] .= "Missing"

        pop_match = zeros(dim_count(model, :time), dim_count(model, :country))
        for reg in unique(mapping.REGION_4LETTER)
            if reg != "Missing"
                scaling = mapping.Pop2015[mapping.REGION_4LETTER .== reg] / sum(mapping.Pop2015[mapping.REGION_4LETTER .== reg])

                for tt in 1:dim_count(model, :time)
                    if dim_keys(model, :time)[tt] < 2025
                        value = baseline.POPULATION[(baseline.IDYEARS .== 2025) .& (baseline.IDSCENARIOS .== scenario) .& (baseline.REGION_4LETTER .== reg)]
                    elseif dim_keys(model, :time)[tt] == 2075
                        value = (baseline.POPULATION[(baseline.IDYEARS .== 2070) .& (baseline.IDSCENARIOS .== scenario) .& (baseline.REGION_4LETTER .== reg)] .+ baseline.POPULATION[(baseline.IDYEARS .== 2080) .& (baseline.IDSCENARIOS .== scenario) .& (baseline.REGION_4LETTER .== reg)]) ./ 2
                    elseif dim_keys(model, :time)[tt] > 2100
                        value = baseline.POPULATION[(baseline.IDYEARS .== 2100) .& (baseline.IDSCENARIOS .== scenario) .& (baseline.REGION_4LETTER .== reg)]
                    else
                        value = baseline.POPULATION[(baseline.IDYEARS .== dim_keys(model, :time)[tt]) .& (baseline.IDSCENARIOS .== scenario) .& (baseline.REGION_4LETTER .== reg)]
                    end
                    if length(value) > 0 && !ismissing(value[1])
                        pop_match[tt, mapping.REGION_4LETTER .== reg] .= scaling * value[1]
                    end
                end
            end
        end
        populationcomp[:pop_match] = pop_match
    end

    return populationcomp
end
