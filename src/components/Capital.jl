capitalparams = CSV.read(pagedata("other/capital.csv"), DataFrame)


@defcomp Capital begin
    country = Index()

    model = Parameter{Model}()
    capital_draw = Parameter{Int64}()

    savingsrate = Variable(index=[country])
    depreciationrate = Variable(index=[country])
    capitalshare = Variable(index=[country])
    capital0 = Variable(index=[country], unit="\$M")

    gdp0_initgdp = Parameter(index=[country], unit="\$M")
    gdp_baseline = Parameter(index=[time, country], unit="\$M")
    rgdp_percap_impacts = Parameter(index=[time, country], unit="\$/person")
    pop_population = Parameter(index=[time, country], unit="million person")

    rgdp_impacts = Variable(index=[time, country], unit="\$M")
    fracloss = Variable(index=[time, country])
    capital_baseline = Variable(index=[time, country], unit="\$M")
    capital = Variable(index=[time, country], unit="\$M")
    rgdp_capital = Variable(index=[time, country], unit="\$M")

    function init(pp, vv, dd)
        mc = pp.capital_draw == -1 ? nothing : pp.capital_draw
        vv.savingsrate[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "saverate") * 100
        vv.depreciationrate[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "deprrate")
        vv.capitalshare[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "capshare")

        ## For missing countries, set capital0 to the stable level
        capital0 = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "procap0", sum; allowmissing=true)
        capital0_stable = vv.savingsrate .* pp.gdp0_initgdp ./ vv.depreciationrate
        capital0[ismissing.(capital0)] = capital0_stable[ismissing.(capital0)]
        vv.capital0[:] = capital0
    end

    function run_timestep(pp, vv, dd, tt)
        for cc in dd.country
            vv.rgdp_impacts[tt, cc] = pp.rgdp_percap_impacts[tt, cc] * pp.pop_population[tt, cc]
            if is_first(tt)
                vv.fracloss[tt, cc] = 0.
                vv.rgdp_capital[tt, cc] = vv.rgdp_impacts[tt, cc]
                vv.capital_baseline[tt, cc] = vv.savingsrate[cc] * pp.gdp_baseline[tt, cc] + (1 - vv.depreciationrate[cc]) * vv.capital0[cc]
                vv.capital[tt, cc] = vv.savingsrate[cc] * vv.rgdp_capital[tt, cc] + (1 - vv.depreciationrate[cc]) * vv.capital0[cc]
            else
                vv.fracloss[tt, cc] = 1 - (vv.capital[tt-1, cc] / vv.capital_baseline[tt-1, cc])^vv.capitalshare[cc]
                vv.rgdp_capital[tt, cc] = (1 - vv.fracloss[tt, cc]) * vv.rgdp_impacts[tt, cc]
                vv.capital_baseline[tt, cc] = vv.savingsrate[cc] * pp.gdp_baseline[tt, cc] + (1 - vv.depreciationrate[cc]) * vv.capital_baseline[tt-1, cc]
                vv.capital[tt, cc] = vv.savingsrate[cc] * vv.rgdp_capital[tt, cc] + (1 - vv.depreciationrate[cc]) * vv.capital[tt-1, cc]
            end
        end
    end
end

function addcapital(model::Model)
    capital = add_comp!(model, Capital)

    capital[:model] = model
    capital[:capital_draw] = -1

    return capital
end
