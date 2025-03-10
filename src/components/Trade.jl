include("../utils/trade.jl")

@defcomp Trade begin
    country = Index()

    model = Parameter{Model}()
    total_damages_peryear = Parameter(index=[time, country], unit="\$million")
    gdp = Parameter(index=[time, country], unit="\$M")
    gdp0_initgdp = Parameter(index=[country], unit="\$M") # GDP in y_year_0

    logscalebys = Variable(index=[time])
    isat_local_ofgdp = Variable(index=[time, country])
    isat_after_ofgdp = Variable(index=[time, country])

    function run_timestep(pp, vv, dd, tt)
        vv.isat_local_ofgdp[tt, :] = pp.total_damages_peryear[tt, :] ./ pp.gdp[tt, :]

        output = calc_domar_distribute_method(dim_keys(pp.model, :country), Vector{Float64}(vv.isat_local_ofgdp[tt, :]))

        global_gdp = sum(pp.gdp0_initgdp)
        global_loss = sum(replace(output.totimpacts2.fracloss_export, missing => 0) .* pp.gdp0_initgdp)

        yy = output.domar_change * global_gdp
        if yy > 0
            logscaleby = log(yy) - log(global_loss)
            if logscaleby >= 0
                vv.logscalebys[tt] = logscaleby
            end
        end

        mod = lm(@formula(scalebys ~ 1), DataFrame(scalebys=[0.; vv.logscalebys[:]]))

        smoothscaleby = exp.(predict(mod)) .* exp(var(residuals(mod)) / 2)

        vv.isat_after_ofgdp[tt, :] = smoothscaleby[1] * output.totimpacts2.fracloss_export
    end
end

function addtrade(model::Model)
    trade = add_comp!(model, Trade)

    trade[:model] = model

    return trade
end
