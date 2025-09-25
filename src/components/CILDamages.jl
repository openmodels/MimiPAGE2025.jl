@defcomp CILDamages begin
    country = Index()
    time = Index()

    rt_g_globaltemperature = Parameter(index=[time], unit="degreeC")

    gdp = Parameter(index=[time, country], unit="\$M")
    gdp0_initgdp = Parameter(index=[country], unit="\$M")

    pop_population = Parameter(index=[time, country], unit="million person")
    pop0_initpopulation = Parameter(index=[country], unit="million person")

    gamma = Parameter()
    alpha = Parameter(index=[country])
    beta = Parameter(index=[country])

    outcome = Variable(index=[time,country])

    function run_timestep(pp, vv, dd, tt)
        gdppc = pp.gdp[tt, :] ./ pp.pop_population[tt, :]
        gdppc0 = pp.gdp0_initgdp ./ pp.pop0_initpopulation

        vv.outcome[tt, :] = (pp.alpha * pp.rt_g_globaltemperature[tt] .+ pp.beta * pp.rt_g_globaltemperature[tt]^2) .* (gdppc ./ gdppc0).^pp.gamma
    end
end

function addcildamages(model::Model, name::Symbol, datafile::String)
    cildamages = add_comp!(model, CILDamages, name)

    polys = CSV.read(pagedata(datafile), DataFrame)
    cildamages[:gamma] = gamma = median(polys.gamma)

    rows = polys[polys.gamma .== gamma, :]

    cildamages[:alpha] = readcountrydata_i_const(model, rows, :region, :alpha)
    cildamages[:beta] = readcountrydata_i_const(model, rows, :region, :beta)

    cildamages
end

function initmc(inst::ModelInstance)
    polys = CSV.read(pagedata(datafile), DataFrame)

    gamma = sample(polys.gamma)
    update_param!(inst, :gamma, gamma)

    rows = polys[polys.gamma .== gamma, :]
    alpha0 = readcountrydata_i_const(model, rows, :region, :alpha)
    beta0 = readcountrydata_i_const(model, rows, :region, :beta)
    vcv11 = readcountrydata_i_const(model, rows, :region, :sigma11)
    vcv12 = readcountrydata_i_const(model, rows, :region, :sigma12)
    vcv22 = readcountrydata_i_const(model, rows, :region, :sigma22)

    alphas = rep(0., dim_count(inst, :country))
    betas = rep(0., dim_count(inst, :country))
    for cc in 1:dim_count(inst, :country)
        mvn = Distributions.MvNormal([alpha0[cc], beta0[cc]], [vcv11[cc] vcv12[cc]; vcv12[cc] vcv22[cc]])
        alphabeta = rand(mvn)
        alphas[cc] = alphabeta[1]
        betas[cc] = alphabeta[2]
    end

    update_param!(inst, :alpha, alphas)
    update_param!(inst, :beta, betas)
end
