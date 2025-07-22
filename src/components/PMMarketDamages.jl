@defcomp PMMarketDamages begin
    country = Index()
    time = Index()

    pm_total = Parameter(index=[time, country], unit="μg/m^3")  # PM2.5 from exported emissions

    # Take conservative minimum of log-linear and log-log effect
    ## 20µg/m3 in Eastern Europe and other regions (soft upper-bound of OECD PM)
    ## But Largest effect on agricultural sector (outdoor labor) so expect to apply to poor regions

    pm2lngdp = Parameter(default=-0.00545, unit="log-point / µg/m3") # SE = 0.00156, Main text Table 5
    lnpm2lngdp = Parameter(default=-0.1175, unit="log-point / log-point") # SE = 0.0199, Appendix B.3.9 Table 24

    dlngdp = Variable(index=[time,country], unit="log-point")
    cumullngdp = Variable(index=[time,country], unit="log-point")
    totalchange = Variable(index=[time,country], unit="%")

    function run_timestep(pp, vv, dd, tt)
        if is_first(tt)
            vv.dlngdp[tt, :] .= 0.
            vv.cumullngdp[tt, :] .= 0.
            vv.totalchange[tt, :] .= 0.
        else
            # Clip benefits/losses at 20 µg/m3
            pm_total_clip = min.(20., pp.pm_total[tt, :])
            lag_pm_total_clip = min.(20., pp.pm_total[tt-1, :])
            value_lin = pp.pm2lngdp * (pm_total_clip - lag_pm_total_clip)
            value_log = pp.pm2lngdp * (log.(pm_total_clip) - log.(lag_pm_total_clip))
            for cc in dd.country
                if value_lin[cc] > 0
                    vv.dlngdp[tt, cc] = min(value_lin[cc], value_log[cc])
                else
                    vv.dlngdp[tt, cc] = max(value_lin[cc], value_log[cc])
                end
            end

            vv.cumullngdp[tt, :] = vv.cumullngdp[tt-1, :] + vv.dlngdp[tt, :]
            vv.totalchange[tt, :] = (exp.(vv.cumullngdp[tt, :]) .- 1) .* 100
        end
    end
end

