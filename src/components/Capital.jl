## Constructs a new GDP value for use in all following components
## Informed by last period's remaining GDP after impacts

capitalparams = CSV.read(pagedata("other/capital.csv"), DataFrame)

@defcomp Capital begin
    country = Index()

    model = Parameter{Model}()
    capital_draw = Parameter{Int64}()

    save_savingsrate = Variable(index=[country])
    depreciationrate = Variable(index=[country])
    capitalshare = Variable(index=[country])
    capital0 = Variable(index=[country], unit="\$M")

    gdp0_initgdp = Parameter(index=[country], unit="\$M")
    gdp_baseline = Parameter(index=[time, country], unit="\$M")
    rgdp_percap_impacts = Parameter(index=[time, country], unit="\$/person")
    pop_population = Parameter(index=[time, country], unit="million person")

    fracloss = Variable(index=[time, country])
    capital_baseline = Variable(index=[time, country], unit="\$M")
    capital = Variable(index=[time, country], unit="\$M")

    gdp_capital = Variable(index=[time, country], unit="\$M")
    gdp_capital_region = Variable(index=[time, region], unit="\$M")
    cons_capital_consumption = Variable(index=[time, country], unit="\$million")
    cons_percap_capital_consumption = Variable(index=[time, country], unit="\$/person")

    function init(pp, vv, dd)
        mc = pp.capital_draw == -1 ? nothing : pp.capital_draw
        vv.save_savingsrate[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "saverate") * 100
        vv.depreciationrate[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "deprrate")
        vv.capitalshare[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "capshare")

        ## For missing countries, set capital0 to the stable level
        capital0 = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "procap0", sum; allowmissing=true)
        capital0_stable = (vv.save_savingsrate ./ 100) .* pp.gdp0_initgdp ./ vv.depreciationrate
        capital0[ismissing.(capital0)] = capital0_stable[ismissing.(capital0)]
        vv.capital0[:] = capital0
    end

    function run_timestep(pp, vv, dd, tt)
        for cc in dd.country
            if is_first(tt)
                vv.capital_baseline[tt, cc] = (vv.save_savingsrate[cc] / 100) * pp.gdp0_initgdp[cc] + (1 - vv.depreciationrate[cc]) * vv.capital0[cc]
                vv.capital[tt, cc] = vv.capital_baseline[tt, cc]
                vv.fracloss[tt, cc] = 0.
                vv.gdp_capital[tt, cc] = pp.gdp_baseline[tt, cc]
            else
                vv.capital_baseline[tt, cc] = (vv.save_savingsrate[cc] / 100) * pp.gdp_baseline[tt-1, cc] + (1 - vv.depreciationrate[cc]) * vv.capital_baseline[tt-1, cc]
                rgdp_impacts = pp.rgdp_percap_impacts[tt-1, cc] * pp.pop_population[tt-1, cc]
                vv.capital[tt, cc] = (vv.save_savingsrate[cc] / 100) * rgdp_impacts + (1 - vv.depreciationrate[cc]) * vv.capital[tt-1, cc]

                vv.fracloss[tt, cc] = 1 - (vv.capital[tt, cc] / vv.capital_baseline[tt, cc])^vv.capitalshare[cc]
                vv.gdp_capital[tt, cc] = (1 - vv.fracloss[tt, cc]) * pp.gdp_baseline[tt, cc]
            end
        end

        vv.cons_capital_consumption[tt, :] = vv.gdp_capital[tt, :] .* (1 .- vv.save_savingsrate[:] ./ 100)
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
