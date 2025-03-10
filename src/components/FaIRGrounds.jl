using MimiFAIRv2

@defcomp FaIRGrounds begin
    country = Index()

    fairmi = Parameter{ModelInstance}()
    clock = Variable{Clock}()

    y_year = Parameter(index=[time], unit="year")
    y_year_0 = Parameter(unit="year")

    e_globalCO2emissions = Parameter(index=[time], unit="Mtonne/year")
    e_globalCH4emissions = Parameter(index=[time], unit="Mtonne/year")
    rt_g_globaltemperature_pre = Parameter(index=[time], unit="degreeC")
    rt_g_globaltemperature_post = Variable(index=[time], unit="degreeC")

    ## Can I take in F from fairmodel? Then feed that into GlobalTemperature
    ## Probably need two GlobalTemperature components, with with SAF and one without.

    function init(pp, vv, dd)
        # Based on Base.run(mi::ModelInstance, ...)
        time_keys::Vector{Int} = dim_keys(pp.fairmi.md, :time)

        vv.clock = Clock(time_keys)

        dim_val_named_tuple = NamedTuple(name => (name == :time ? timesteps(clock) : collect(values(dim))) for (name, dim) in dim_dict(pp.fairmi.md))

        init(pp.fairmi, dim_val_named_tuple)

        while TIMECHECK(pp.y_year_0)
            run_timestep(pp.fairmi, vv.clock, dim_val_named_tuple)
            advance(vv.clock)
        end
    end

    function run_timestep(pp, vv, dd, tt)
        E_co2 = pp.e_globalCO2emissions / 1000 / 3.67 # GtC yr⁻¹
        E_ch4 = pp.e_globalCH4emissions # TgCH₄ yr⁻¹

        fair_co2 = pp.fairmi[:co2_cycle, :E_co2]
        fair_ch4 = pp.fairmi[:ch4_cycle, :E_ch4]

        for ii in TIMESTEPS(is_first(tt) ? p.y_year_0 : p.y_year[tt-1], p.y_year[tt])
            fair_co2[ii] = E_co2
            fair_ch4[ii] = E_ch4
        end

        update_param!(pp.fairmi, :co2_cycle, :E_co2, fair_co2)
        update_param!(pp.fairmi, :ch4_cycle, :E_ch4, fair_ch4)

        while TIMECHECK(pp.y_year[tt])
            run_timestep(pp.fairmi, vv.clock, dim_val_named_tuple)
            advance(vv.clock)
        end
    end
end

function addfairgrounds(model::Model)
    fairgrounds = add_comp!(model, FaIRGrounds)

    fairmodel = MimiFAIRv2.get_model(end_year=2300)
    fairgrounds[:fairmi] = build(fairmodel)

    return fairgrounds
end
