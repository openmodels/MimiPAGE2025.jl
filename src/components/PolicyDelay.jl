using Interpolations
delays = myloadcsv("data/other/delays.csv")

@defcomp PolicyDelay begin
    country = Index()

    model = Parameter{Model}()
    delay_draw = Parameter{Int64}()

    delay = Variable(index=[country], unit="year")

    carbonprice_raw = Parameter(index=[time, country], unit="\$2010/tCO2")
    carbonprice = Variable(index=[time, country], unit="\$2010/tCO2")

    function init(pp, vv, dd)
        if pp.delay_draw == 0
            mc_delays = combine(delays, names(delays) .=> mean)
            rename!(mc_delays, :china_mean => :china, :india_mean => :india, :mexico_mean => :mexico, :nigeria_mean => :nigeria)
        else
            mc_delays = delays[pp.delay_draw, :]
        end

        onedelays = [mc_delays.china[1], mc_delays.india[1], mc_delays.mexico[1], mc_delays.nigeria[1]]
        vv.delay[:] = sample(onedelays, dim_count(pp.model, :country), replace=true)
        vv.delay[dim_keys(pp.model, :country) .== "CHN"] .= mc_delays.china[1]
        vv.delay[dim_keys(pp.model, :country) .== "IND"] .= mc_delays.india[1]
        vv.delay[dim_keys(pp.model, :country) .== "MEX"] .= mc_delays.mexico[1]
        vv.delay[dim_keys(pp.model, :country) .== "NGA"] .= mc_delays.nigeria[1]
    end

    function run_timestep(pp, vv, dd, tt)
        for cc in dd.country
            if vv.delay[cc] < 0 || is_first(tt)
                vv.carbonprice[tt, cc] = pp.carbonprice_raw[tt, cc]
            else
                interpolation = LinearInterpolation(dim_keys(model, :time)[1:tt.t],
                                                    [pp.carbonprice_raw[TimestepIndex(it), cc] for it in 1:tt.t], extrapolation_bc=Flat())
                vv.carbonprice[tt, cc] = interpolation(gettime(tt) - vv.delay[cc])
            end
        end
    end
end

function addpolicydelay(model::Model)
    policydelay = add_comp!(model, PolicyDelay)

    policydelay[:model] = model
    policydelay[:delay_draw] = 0

    return policydelay
end
