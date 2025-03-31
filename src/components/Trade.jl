include("../utils/trade.jl")

@defcomp Trade begin
    country = Index()

    model = Parameter{Model}()
    gdp0_initgdp = Parameter(index=[country], unit="\$M") # GDP in y_year_0
    save_savingsrate = Parameter(unit="%", default=15.)

    pop_population = Parameter(index=[time, country], unit="million person")
    gdp_baseline = Parameter(index=[time, country], unit="\$M") # before capital effects, if used
    gdp = Parameter(index=[time, country], unit="\$M") # after capital effects, if used
    rgdp_percap_impacts = Parameter(index=[time, country], unit="\$/person")

    global_loss = Variable(index=[time])
    domar_change = Variable(index=[time])
    logscalebys = Variable(index=[time])
    isat_local_ofgdp = Variable(index=[time, country])
    isat_after_ofgdp = Variable(index=[time, country])

    rcons_per_cap_TradeRemainConsumption = Variable(index=[time, country], unit="\$/person")
    rgdp_per_cap_TradeRemainGDP = Variable(index=[time, country], unit="\$/person")

    function run_timestep(pp, vv, dd, tt)
        rgdp_impacts = pp.rgdp_percap_impacts[tt, :] .* pp.pop_population[tt, :]
        vv.isat_local_ofgdp[tt, :] = (pp.gdp_baseline[tt, :] .- rgdp_impacts) ./ pp.gdp[tt, :]

        output = calc_domar_distribute_method(pp.model, Vector{Float64}(-vv.isat_local_ofgdp[tt, :]))

        global_gdp = sum(pp.gdp0_initgdp)
        vv.global_loss[tt] = sum(replace(output.totimpacts2.fracloss_export, missing => 0) .* pp.gdp0_initgdp)
        vv.domar_change[tt] = output.domar_change

        domar_loss = -output.domar_change * global_gdp
        if domar_loss / vv.global_loss[tt] > 0.01 && domar_loss / vv.global_loss[tt] < 100
            vv.logscalebys[tt] = log(domar_loss / vv.global_loss[tt])
        end

        mod = lm(@formula(scalebys ~ 1), DataFrame(scalebys=[0.; vv.logscalebys[:]]))

        if length(residuals(mod)) < 2
            smoothscaleby = 1.
        else
            smoothscaleby = exp.(predict(mod)) .* exp(var(residuals(mod)) / 2)
        end

        vv.isat_after_ofgdp[tt, :] = smoothscaleby[1] * output.totimpacts2.fracloss_export
        # isat = (GDPbase - RGDP) / GDP => RGDP = GDPbase - isat * GDP
        vv.rgdp_per_cap_TradeRemainGDP[tt, :] = (pp.gdp_baseline[tt, :] .- pp.gdp[tt, :] .* vv.isat_after_ofgdp[tt, :]) ./ pp.pop_population[tt, :]
        vv.rcons_per_cap_TradeRemainConsumption[tt, :] = vv.rgdp_per_cap_TradeRemainGDP[tt, :] .* (1 - pp.save_savingsrate / 100)
    end
end

function addtrade(model::Model)
    trade = add_comp!(model, Trade)

    trade[:model] = model

    return trade
end
