using MimiFAIRv2
import Mimi.ModelInstance, Mimi.Clock, Mimi.build, Mimi.dim_dict, Mimi.timesteps

@defcomp FaIRGrounds begin
    country = Index()

    fairmi = Parameter{ModelInstance}()
    prepare_instance = Parameter{Function}()
    fair_draw = Parameter{Int64}()

    clock = Variable{Any}()

    y_year = Parameter(index=[time], unit="year")
    y_year_0 = Parameter(unit="year")

    e_globalCO2emissions = Parameter(index=[time], unit="Mtonne/year")
    e_globalCH4emissions = Parameter(index=[time], unit="Mtonne/year")
    e_globalN2Oemissions = Parameter(index=[time], unit="Mtonne/year")
    # e_globalLGemissions = Parameter(index=[time], unit="Mtonne/year")
    # exf_excessforcing = Parameter(index=[time], unit="W/m2")
    # e_globalSulphateemissions = Parameter(index=[time, region], unit="???")

    perm_tot_e_co2 = Parameter(index=[time], unit="Mtonne")
    perm_tot_ce_ch4 = Parameter(index=[time], unit="Mtonne")

    biascorrection = Variable()

    rt_g_globaltemperature_pre_static = Parameter(index=[time], unit="degreeC")
    rt_g_globaltemperature_pre_seaice = Parameter(index=[time], unit="degreeC")
    rt_g_globaltemperature = Variable(index=[time], unit="degreeC")

    function init(pp, vv, dd)
        if pp.fair_draw != 0
            pp.prepare_instance(pp.fairmi, pp.fair_draw)
        end

        # Based on Base.run(mi::ModelInstance, ...)
        time_keys::Vector{Int} = dim_keys(pp.fairmi.md, :time)

        vv.clock = Clock(time_keys)

        dim_val_named_tuple = NamedTuple(name => (name == :time ? timesteps(vv.clock) : collect(values(dim))) for (name, dim) in dim_dict(pp.fairmi.md))

        Mimi.init(pp.fairmi, dim_val_named_tuple)

        while gettime(vv.clock) <= pp.y_year_0
            Mimi.run_timestep(pp.fairmi, vv.clock, dim_val_named_tuple)
            Mimi.advance(vv.clock)
        end

        ## Calculate bias correction
        fairtime = dim_keys(pp.fairmi, :time)
        calctemp = mean(pp.fairmi[:temperature, :T][findfirst(fairtime .== 1995):findfirst(fairtime .== 2014)])
        vv.biascorrection = 0.85 - calctemp
    end

    function run_timestep(pp, vv, dd, tt)
        fairtime = dim_keys(pp.fairmi, :time)
        if !is_first(tt)
            E_co2 = (pp.e_globalCO2emissions[tt-1] + pp.perm_tot_e_co2[tt]) / 1000 / 3.67 # GtC yr⁻¹
            E_ch4 = (pp.e_globalCH4emissions[tt-1] + pp.perm_tot_ce_ch4[tt] - pp.perm_tot_ce_ch4[tt-1]) # TgCH₄ yr⁻¹
            E_n2o = pp.e_globalN2Oemissions[tt-1] * 0.6367 # TgN yr⁻¹ (2 * 14.01 / 44.01)

            fair_co2 = pp.fairmi[:co2_cycle, :E_co2]
            fair_ch4 = pp.fairmi[:ch4_cycle, :E_ch4]
            fair_n2o = pp.fairmi[:n2o_cycle, :E_n2o]

            for ii in findfirst(fairtime .== (is_first(tt) ? pp.y_year_0 : pp.y_year[tt-1]))+1:findfirst(fairtime .== pp.y_year[tt])
                fair_co2[ii] = E_co2
                fair_ch4[ii] = E_ch4
                fair_n2o[ii] = E_n2o
            end

            update_param!(pp.fairmi, :co2_cycle, :E_co2, fair_co2)
            update_param!(pp.fairmi, :ch4_cycle, :E_ch4, fair_ch4)
            update_param!(pp.fairmi, :n2o_cycle, :E_n2o, fair_n2o)
        end

        dim_val_named_tuple = NamedTuple(name => (name == :time ? timesteps(vv.clock) : collect(values(dim))) for (name, dim) in dim_dict(pp.fairmi.md))
        while gettime(vv.clock) <= pp.y_year[tt]
            Mimi.run_timestep(pp.fairmi, vv.clock, dim_val_named_tuple)
            Mimi.advance(vv.clock)
        end

        vv.rt_g_globaltemperature[tt] = pp.fairmi[:temperature, :T][findfirst(fairtime .== pp.y_year[tt])] + vv.biascorrection + pp.rt_g_globaltemperature_pre_seaice[tt] - pp.rt_g_globaltemperature_pre_static[tt]
    end
end

function addfairgrounds(model::Model, scenario::String)
    fairgrounds = add_comp!(model, FaIRGrounds)

    mapping = Dict("Zero Emissions & SSP1"=>"ssp119", "1.5 degC Target"=>"ssp119", "RCP1.9 & SSP1"=>"ssp119", "2 degC Target"=>"ssp126", "RCP2.6 & SSP1"=>"ssp126",
                   "NDCs"=>"ssp245", "NDCs Partial"=>"ssp245", "RCP4.5 & SSP2"=>"ssp245", "BAU"=>"ssp370", "RCP8.5 & SSP5"=>"ssp585", "RCP8.5 & SSP2"=>"ssp585")

    fairmodel = MimiFAIRv2.get_model(end_year=2300, emissions_forcing_scenario=mapping[scenario])
    fairgrounds[:fairmi] = build(fairmodel)
    fairgrounds[:fair_draw] = 0
    fairgrounds[:prepare_instance] = (mi, ii) -> nothing

    fairgrounds[:rt_g_globaltemperature_pre_static] = zeros(dim_count(model, :time))
    fairgrounds[:rt_g_globaltemperature_pre_seaice] = zeros(dim_count(model, :time))

    return fairgrounds
end
