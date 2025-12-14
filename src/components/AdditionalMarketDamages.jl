@defcomp AdditionalMarketDamages begin
    country = Index()

    gdp_baseline = Parameter(index=[time, country], unit="\$M")
    rgdp_per_cap_MarketRemainGDP = Parameter(index=[time, country], unit="\$/person")

    save_savingsrate = Parameter(index=[country], unit="%")

    one = Parameter(index=[time, country], unit="\$M/yr")
    two = Parameter(index=[time, country], unit="\$M/yr")
    pop_population = Parameter(index=[time, country], unit="million person")

    rgdp_per_cap_FullMarketRemainGDP = Variable(index=[time, country], unit="\$/person")
    rcons_per_cap_FullMarketRemainConsumption = Variable(index=[time, country], unit="\$/person")

    function run_timestep(p, v, d, t)
        for cc in d.country
            gdppc = p.gdp_baseline[t, cc] / p.pop_population[t, cc]
            fracloss_old = 1. - p.rgdp_per_cap_MarketRemainGDP[t, cc] / gdppc
            fracloss_new = (p.one[t, cc] + p.two[t, cc]) / p.gdp_baseline[t, cc]
            fracloss_combo = fracloss_old + fracloss_new / (1 + (1 / (1 - fracloss_old)) * fracloss_new) # Assumptotic to 1

            v.rgdp_per_cap_FullMarketRemainGDP[t, cc] = gdppc * (1 - fracloss_combo)
            v.rcons_per_cap_FullMarketRemainConsumption[t, cc] = (1 - p.save_savingsrate[cc] / 100) * v.rgdp_per_cap_FullMarketRemainGDP[t, cc]
        end
    end
end

function addadditionalmarketdamages(model::Model)
    additional = add_comp!(model, AdditionalMarketDamages)

    println("NOTE: Need to update to costs in PM25_delta_impact_cost_mort_morb_20251203.xlsx")

    additional[:one] = zeros(dim_count(model, :time), dim_count(model, :country))
    additional[:two] = zeros(dim_count(model, :time), dim_count(model, :country))

    return additional
end
