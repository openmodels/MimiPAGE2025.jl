## Constructs a new GDP value for use in all following components
## Informed by last period's remaining GDP after impacts

capitalparams = CSV.read(pagedata("other/capital.csv"), DataFrame)

function modelstock(k0, remain, change, nyear)
    ## Let K_t = p K_t-1 + Y
    ## Result is K_N = p^N K_0 + Y (1 - p^N) / (1 - p)
    (remain.^nyear) .* k0 .+ change .* (1 .- remain.^nyear) ./ (1 .- remain)
end

@defcomp Capital begin
    country = Index()

    model = Parameter{Model}()
    y_year = Parameter(index=[time], unit="year")
    y_year_0 = Parameter(unit="year", default=2015.)

    capital_draw = Parameter{Int64}(default=-1)

    save_savingsrate = Parameter(index=[country], unit="%")
    depreciationrate = Variable(index=[country])
    capitalshare = Variable(index=[country])
    capital0 = Variable(index=[country], unit="\$M")

    gdp0_initgdp = Parameter(index=[country], unit="\$M")
    gdp_baseline = Parameter(index=[time, country], unit="million US\$2005/yr")
    rgdp_percap_impacts = Parameter(index=[time, country], unit="\$/person")
    i1log_impactlogchange = Parameter(index=[time, country]) # one-year impact that persists
    pop_population = Parameter(index=[time, country], unit="million person")

    ## Persistence modeling
    persist_omega = Parameter(default=0.46) # 1 - persistence
    persist_delta = Variable(index=[time, country]) # portion of GDP
    nlag_burke = Variable() # Calculated based on persistence

    fracloss = Variable(index=[time, country])
    capital_baseline = Variable(index=[time, country], unit="\$M")
    capital = Variable(index=[time, country], unit="\$M")

    gdp_capital = Variable(index=[time, country], unit="million US\$2005/yr")
    gdp_capital_region = Variable(index=[time, region], unit="\$M")
    cons_capital_consumption = Variable(index=[time, country], unit="\$million")
    cons_percap_capital_consumption = Variable(index=[time, country], unit="\$/person")

    function init(pp, vv, dd)
        mc = pp.capital_draw == -1 ? nothing : pp.capital_draw
        vv.depreciationrate[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "deprrate")
        vv.capitalshare[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "capshare")

        ## For missing countries, set capital0 to the stable level
        capital0 = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "procap0", sum; allowmissing=true)
        capital0_stable = (pp.save_savingsrate ./ 100) .* pp.gdp0_initgdp ./ vv.depreciationrate
        capital0[ismissing.(capital0)] = capital0_stable[ismissing.(capital0)]
        vv.capital0[:] = capital0

        vv.nlag_burke = modelstock(0., (1 - pp.persist_omega), 1., 5) # Exactly 5 years, to match up with -5 below
    end

    function run_timestep(pp, vv, dd, tt)
        for cc in dd.country
            if is_first(tt)
                vv.capital_baseline[tt, cc] = modelstock(vv.capital0[cc], (1 - vv.depreciationrate[cc]), (pp.save_savingsrate[cc] / 100) * pp.gdp0_initgdp[cc], pp.y_year[tt] - pp.y_year_0)
                vv.capital[tt, cc] = vv.capital_baseline[tt, cc]
                vv.fracloss[tt, cc] = 0.
                vv.persist_delta[tt, cc] = 0.
                vv.gdp_capital[tt, cc] = pp.gdp_baseline[tt, cc]
            else
                vv.capital_baseline[tt, cc] = modelstock(vv.capital_baseline[tt-1, cc], (1 - vv.depreciationrate[cc]), (pp.save_savingsrate[cc] / 100) * pp.gdp_baseline[tt-1, cc], pp.y_year[tt] - pp.y_year[tt-1])
                rgdp_impacts = max(pp.rgdp_percap_impacts[tt-1, cc], 365) * pp.pop_population[tt-1, cc] # clip at $1 / day
                vv.capital[tt, cc] = modelstock(vv.capital[tt-1, cc], (1 - vv.depreciationrate[cc]), (pp.save_savingsrate[cc] / 100) * rgdp_impacts, pp.y_year[tt] - pp.y_year[tt-1])

                ## We assume that there are additional persistence impacts, not through capital channel
                ## Calculate a portion of GDP lost due to previous years' impacts
                vv.persist_delta[tt, cc] = modelstock(modelstock(vv.persist_delta[tt-1, cc], (1 - pp.persist_omega), -pp.i1log_impactlogchange[tt-1, cc], pp.y_year[tt] - pp.y_year[tt-1] - 5), (1 - pp.persist_omega), 0., 5) # Assume these values up to 5 years before next period
                if vv.persist_delta[tt, cc] > 0.
                    vv.persist_delta[tt, cc] = vv.persist_delta[tt, cc] / (1 + vv.persist_delta[tt, cc]) # asymptotic approach to 1
                end

                fracloss1 = 1 - (vv.capital[tt, cc] / vv.capital_baseline[tt, cc])^vv.capitalshare[cc]
                fracloss2 = vv.persist_delta[tt, cc]
                vv.fracloss[tt, cc] = fracloss1 + fracloss2 - fracloss1 * fracloss2 # Assume overlap at higher levels
                vv.gdp_capital[tt, cc] = (1 - vv.fracloss[tt, cc]) * pp.gdp_baseline[tt, cc]
            end
        end

        vv.cons_capital_consumption[tt, :] = vv.gdp_capital[tt, :] .* (1 .- pp.save_savingsrate[:] ./ 100)
        vv.cons_percap_capital_consumption[tt, :] = vv.cons_capital_consumption[tt, :] ./ pp.pop_population[tt, :]
        vv.gdp_capital_region[tt, :] = countrytoregion(pp.model, sum, vv.gdp_capital[tt, :])
    end
end

function addcapital(model::Model)
    capital = add_comp!(model, Capital)

    capital[:model] = model
    capital[:capital_draw] = -1

    return capital
end
