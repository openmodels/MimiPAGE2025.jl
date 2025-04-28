using Mimi

# Calculate temperature mortality damages
# Cromar et al. 2021

@defcomp CromarMortalityDamages begin

    country                 = Index() # Index for countries used in the PAGE

    β_mortality             = Parameter(index=[country]) # Coefficient relating global temperature to change in mortality rates.
    baseline_mortality_rate = Parameter(index=[time, country], unit = "deaths/1000 persons/yr") # Crude death rate in a given country (deaths per 1,000 population).
    temperature             = Parameter(index=[time], unit="degreeC") # Global average surface temperature anomaly relative to pre-industrial (°C).

    population              = Parameter(index=[time, country], unit="million person") # Population in a given country (millions of persons).
    vsl                     = Parameter(index=[time, country], unit="US\$2005/yr") # Value of a statistical life ($).

    mortality_change        = Variable(index=[time, country])  # Change in a country's baseline mortality rate due to combined effects of cold and heat (with positive values indicating increasing mortality rates).
    mortality_costs         = Variable(index=[time, country], unit="US\$2005/yr")  # Costs of temperature mortality based on the VSL ($).
    excess_death_rate       = Variable(index=[time, country], unit = "deaths/1000 persons/yr")  # Change in a country's baseline death rate due to combined effects of cold and heat (additional deaths per 1,000 population).
    excess_deaths           = Variable(index=[time, country], unit="persons")  # Additional deaths that occur in a country due to the combined effects of cold and heat (individual persons).


      function run_timestep(p, v, d, t)

        for c in d.country

            # Calculate change in a country's baseline mortality rate due to combined effects of heat and cold.
            v.mortality_change[t,c] = p.β_mortality[c] * p.temperature[t]

            # Calculate additional deaths per 1,000 population due to cold and heat.
            v.excess_death_rate[t,c] = p.baseline_mortality_rate[t,c] * v.mortality_change[t,c]

            # Calculate additional deaths that occur due to cold and heat (assumes population units in millions of persons so converts to thousands to match deathrates units).
            v.excess_deaths[t,c] = (p.population[t,c] .* 1000) * v.excess_death_rate[t,c]

            # Multiply excess deaths by the VSL.
            v.mortality_costs[t,c] = p.vsl[t,c] * v.excess_deaths[t,c]
        end
    end
end




function addcromarmortality(m::Model, SSP::String = "SSP2")
    add_comp!(m, CromarMortalityDamages, :CromarMortality)

    cromar_coeffs = load(pagedata("mortality/CromarMortality_damages_coefficients.csv")) |> DataFrame
    cromar_mapping_raw = load(pagedata("mortality/Mapping_countries_to_cromar_mortality_regions.csv")) |> DataFrame

    rename!(cromar_coeffs, "Cromar Region Name" => "cromar_region")
    cromar_mapping_with_beta = leftjoin(cromar_mapping_raw, cromar_coeffs[:, ["cromar_region", "Pooled Beta"]], on = :cromar_region)

    country_β_mortality2 = readcountrydata_i_const(m, cromar_mapping_with_beta, :ISO3, "Pooled Beta")

    update_param!(m, :CromarMortality, :β_mortality, country_β_mortality2)

    # baseline_mortality_rate
    baseline_mortality_data = load(pagedata("mortality/Mortality_cdr_spp_country_extensions_annual.csv")) |> DataFrame

    # Ensure we're selecting the correct scenario (SSP2 as proxy for SSP4, SSP1 for SSP5)
    mortality_SSP_map = Dict("SSP1" => "SSP1", "SSP2" => "SSP2", "SSP3" => "SSP3", "SSP4" => "SSP2", "SSP5" => "SSP1")

    mortality_data_filtered = baseline_mortality_data |>
        @filter(_.year in 2020:2300 && _.scenario == mortality_SSP_map[SSP]) |>
        @select(:year, :ISO, :cdf) |>  # cdr = crude death rate
        DataFrame

    mortality_data_filtered2 = readcountrydata_it_const(m, mortality_data_filtered, :ISO, :year, "cdf")

    update_param!(m, :CromarMortality, :baseline_mortality_rate, mortality_data_filtered2)
end
