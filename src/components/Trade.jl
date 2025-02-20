include("../utils/trade.jl")

@defcomp Trade begin
    country = Index()

    model = Parameter{Model}()
    isat_local_ofgdp = Parameter(index=[time, country])

    logscalebys = Variable(index=[time])
    isat_after_ofgdp = Variable(index=[time, country])

    function run_timestep(pp, vv, dd, tt)
        output = calc_domar_distribute_method1(get_time(tt), dim_keys(pp.model, :country), pp.isat_local_ofgdp)

        thisglobal = output.global
        thisglobal[!, :yy] = thisglobal.domar_change .* thisglobal.global_gdp
        if thisglobal.yy > 0
            logscaleby = log(thisglobal.yy) - log(thisglobal.global_loss)
            if logscaleby >= 0
                pp.logscalebys[tt] = logscaleby
            end
        end

        mod = lm(@formula(scalebys ~ 1), DataFrame(scalebys=[0.; pp.logscalebys]))
        smoothscaleby = exp.(predict(mod, DataFrame(years=get_time(tt)))) .* exp(var(residuals(mod)) / 2)

        vv.isat_after_ofgdp[tt, :] = calc_domar_distribute_method2(smoothscaleby, dim_keys(pp.model, :country), output.totimpacts2)
    end
end

function addtrade(model::Model)
    trade = add_comp!(model, Trade)

    trade[:model] = model

    return trade
end
