using MimiFAIRv2
import Mimi.ModelInstance, Mimi.Clock, Mimi.build, Mimi.dim_dict, Mimi.timesteps

include("../utils/gains.jl")

@defcomp FaIRGrounds begin
    country = Index()

    fairmi = Parameter{ModelInstance}()
    prepare_instance = Parameter{Function}()
    fair_draw = Parameter{Int64}()

    pulse_gas = Parameter{String}()
    pulse_year = Parameter{Int64}()
    pulse_size = Parameter() # given in Mt for all gases

    clock = Variable{Any}()

    y_year = Parameter(index=[time], unit="year")
    y_year_0 = Parameter(unit="year")

    e_globalCO2emissions = Parameter(index=[time], unit="Mtonne/year")
    e_globalCH4emissions = Parameter(index=[time], unit="Mtonne/year")
    e_globalN2Oemissions = Parameter(index=[time], unit="Mtonne/year")

    e_globalNH3emissions = Parameter(index=[time], unit="Mtonne/year")
    e_globalBCemissions = Parameter(index=[time], unit="Mtonne/year")
    e_globalSO2emissions = Parameter(index=[time], unit="Mtonne/year")
    e_flourinated_ratio = Parameter(index=[time], unit="fraction")
    e_nox_ratio = Parameter(index=[time], unit="fraction")
    e_voc_ratio = Parameter(index=[time], unit="fraction")
    tempscaling_2030 = Parameter(unit="fraction")

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
            ## Apply correction based on linear approximation
            if pp.y_year[tt-1] > 2020
                totalused_co2 = pp.e_globalCO2emissions[tt-2] * (pp.y_year[tt-1] - pp.y_year[tt-2])
                newestimate_co2 = (pp.e_globalCO2emissions[tt-2] + pp.e_globalCO2emissions[tt-1]) * (pp.y_year[tt-1] - pp.y_year[tt-2]) / 2
                correctperyr_co2 = (newestimate_co2 - totalused_co2) / ((pp.y_year[tt] < 2300 ? pp.y_year[tt] : 2350) - pp.y_year[tt-1])

                totalused_ch4 = pp.e_globalCH4emissions[tt-2] * (pp.y_year[tt-1] - pp.y_year[tt-2])
                newestimate_ch4 = (pp.e_globalCH4emissions[tt-2] + pp.e_globalCH4emissions[tt-1]) * (pp.y_year[tt-1] - pp.y_year[tt-2]) / 2
                correctperyr_ch4 = (newestimate_ch4 - totalused_ch4) / ((pp.y_year[tt] < 2300 ? pp.y_year[tt] : 2350) - pp.y_year[tt-1])

                totalused_n2o = pp.e_globalN2Oemissions[tt-2] * (pp.y_year[tt-1] - pp.y_year[tt-2])
                newestimate_n2o = (pp.e_globalN2Oemissions[tt-2] + pp.e_globalN2Oemissions[tt-1]) * (pp.y_year[tt-1] - pp.y_year[tt-2]) / 2
                correctperyr_n2o = (newestimate_n2o - totalused_n2o) / ((pp.y_year[tt] < 2300 ? pp.y_year[tt] : 2350) - pp.y_year[tt-1])
            else
                correctperyr_co2 = correctperyr_ch4 = correctperyr_n2o = 0
            end

            E_co2 = (pp.e_globalCO2emissions[tt-1] + pp.perm_tot_e_co2[tt-1] + correctperyr_co2) / 1000 / 3.67 # GtC yr⁻¹
            if tt.t > 2
                E_ch4 = pp.e_globalCH4emissions[tt-1] + (pp.perm_tot_ce_ch4[tt-1] - pp.perm_tot_ce_ch4[tt-2]) / (pp.y_year[tt-1] - pp.y_year[tt-2]) + correctperyr_ch4 # TgCH₄ yr⁻¹
            else
                E_ch4 = pp.e_globalCH4emissions[tt-1] # TgCH₄ yr⁻¹
            end
            E_n2o = (pp.e_globalN2Oemissions[tt-1] + correctperyr_n2o) * 0.6367 # TgN yr⁻¹ (2 * 14.01 / 44.01)

            fair_co2 = pp.fairmi[:co2_cycle, :E_co2]
            fair_ch4 = pp.fairmi[:ch4_cycle, :E_ch4]
            fair_n2o = pp.fairmi[:n2o_cycle, :E_n2o]
            fair_flourinated = pp.fairmi[:flourinated_cycles, :E_flourinated]
            fair_aerosols = pp.fairmi[:aerosol_plus_cycles, :E_aerosol_plus]

            for ii in findfirst(fairtime .== (is_first(tt) ? pp.y_year_0 : pp.y_year[tt-1]))+1:findfirst(fairtime .== pp.y_year[tt])
                fair_co2[ii] = E_co2 + (pp.pulse_gas == "co2" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0) / 1000 / 3.67
                fair_ch4[ii] = E_ch4 + (pp.pulse_gas == "ch4" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0)
                fair_n2o[ii] = E_n2o + (pp.pulse_gas == "n2o" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0)
                for gg in 1:dim_count(pp.fairmi, :flourinated_gases)
                    fair_flourinated[ii, gg] = pp.e_flourinated_ratio[tt] * fair_flourinated[ii, gg] + (gg == 1 && pp.pulse_gas == "fgas" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0)
                end
                fair_aerosols[ii, dim_keys(pp.fairmi, :aerosol_plus_gases) .== "nh3"] .= pp.e_globalNH3emissions[tt-1] + (pp.pulse_gas == "nh3" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0)
                for gg in findall(dim_keys(pp.fairmi, :aerosol_plus_gases) ∈ ["nox", "nox_avi"])
                    fair_aerosols[ii, gg] = pp.e_nox_ratio[tt] * fair_aerosols[ii, gg] + (dim_keys(pp.fairmi, :aerosol_plus_gases)[gg] == "nox" && pp.pulse_gas == "nox" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0)
                end
                fair_aerosols[ii, dim_keys(pp.fairmi, :aerosol_plus_gases) .== "bc"] .= pp.e_globalBCemissions[tt-1] + (pp.pulse_gas == "bc" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0)
                fair_aerosols[ii, dim_keys(pp.fairmi, :aerosol_plus_gases) .== "so2"] .= pp.e_globalSO2emissions[tt-1] + (pp.pulse_gas == "so2" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0)
                for gg in findall(dim_keys(pp.fairmi, :aerosol_plus_gases) ∈ ["nmvoc"])
                    fair_aerosols[ii, gg] = pp.e_voc_ratio[tt] * fair_aerosols[ii, gg] + (dim_keys(pp.fairmi, :aerosol_plus_gases)[gg] == "nmvoc" && pp.pulse_gas == "nmvoc" && fairtime[ii] == pp.pulse_year ? pp.pulse_size : 0)
                end
            end

            update_param!(pp.fairmi, :co2_cycle, :E_co2, fair_co2)
            update_param!(pp.fairmi, :ch4_cycle, :E_ch4, fair_ch4)
            update_param!(pp.fairmi, :n2o_cycle, :E_n2o, fair_n2o)
            update_param!(pp.fairmi, :flourinated_cycles, :E_flourinated, fair_flourinated)
            update_param!(pp.fairmi, :aerosol_plus_cycles, :E_aerosol_plus, fair_aerosols)
        end

        dim_val_named_tuple = NamedTuple(name => (name == :time ? timesteps(vv.clock) : collect(values(dim))) for (name, dim) in dim_dict(pp.fairmi.md))
        while gettime(vv.clock) <= pp.y_year[tt]
            Mimi.run_timestep(pp.fairmi, vv.clock, dim_val_named_tuple)
            Mimi.advance(vv.clock)
        end

        fairtemp = pp.fairmi[:temperature, :T][findfirst(fairtime .== pp.y_year[tt])]
        vv.rt_g_globaltemperature[tt] = fairtemp + vv.biascorrection + pp.rt_g_globaltemperature_pre_seaice[tt] - pp.rt_g_globaltemperature_pre_static[tt]
        if !is_first(tt)
            vv.rt_g_globaltemperature[tt] = (vv.rt_g_globaltemperature[tt] - vv.rt_g_globaltemperature[TimestepIndex(1)]) * pp.tempscaling_2030 + vv.rt_g_globaltemperature[TimestepIndex(1)]
        end
    end
end

function addfairgrounds(model::Model, scenario::String, gains_scenario::String)
    fairgrounds = add_comp!(model, FaIRGrounds)

    mapping = Dict("Zero Emissions & SSP1"=>"ssp119", "1.5 degC Target"=>"ssp119", "RCP1.9 & SSP1"=>"ssp119", "2 degC Target"=>"ssp126", "RCP2.6 & SSP1"=>"ssp126",
                   "NDCs"=>"ssp245", "NDCs Partial"=>"ssp245", "RCP4.5 & SSP2"=>"ssp245", "BAU"=>"ssp370", "RCP8.5 & SSP5"=>"ssp585", "RCP8.5 & SSP2"=>"ssp585", "RCP2.6 & SSP2"=>"ssp126", "RCP4.5 & SSP3"=> "ssp245", "RCP2.6 & SSP3"=>"ssp126", "RCP1.9 & SSP2"=>"ssp119", "RCP1.9 & SSP3"=>"ssp119")

    fairmodel = MimiFAIRv2.get_model(end_year=2300, emissions_forcing_scenario=mapping[scenario])
    fairgrounds[:fairmi] = build(fairmodel)
    fairgrounds[:fair_draw] = 0
    fairgrounds[:prepare_instance] = (mi, ii) -> nothing

    fairgrounds[:rt_g_globaltemperature_pre_static] = zeros(dim_count(model, :time))
    fairgrounds[:rt_g_globaltemperature_pre_seaice] = zeros(dim_count(model, :time))

    fairgrounds[:perm_tot_e_co2] = zeros(dim_count(model, :time))
    fairgrounds[:perm_tot_ce_ch4] = zeros(dim_count(model, :time))

    # Base values from FaIR SSP245
    scenemits = load_gains_emissions(model, gains_scenario)
    fairgrounds[:e_globalNH3emissions] = [get_gains_value(scenemits, dim_keys(model, :time)[tt], "NH3_kt/yr", 60.12536147 * 1e3) for tt in 1:dim_count(model, :time)] ./ 1e3
    fairgrounds[:e_globalBCemissions] = [get_gains_value(scenemits, dim_keys(model, :time)[tt], "PM_BC_kt/yr", 6.723748379 * 1e3) for tt in 1:dim_count(model, :time)] ./ 1e3
    fairgrounds[:e_globalSO2emissions] = [get_gains_value(scenemits, dim_keys(model, :time)[tt], "SO2_kt/yr", 77.64105797 * 1e3) for tt in 1:dim_count(model, :time)] ./ 1e3

    baseemits = load_gains_emissions(model, "Baseline")

    scenfgas = [get_gains_value(scenemits, dim_keys(model, :time)[tt], "FGAS_Mt CO2eq/yr", 1.) for tt in 1:dim_count(model, :time)]
    basefgas = [get_gains_value(baseemits, dim_keys(model, :time)[tt], "FGAS_Mt CO2eq/yr", 1.) for tt in 1:dim_count(model, :time)]

    fairgrounds[:e_flourinated_ratio] = scenfgas ./ basefgas

    scennox = [get_gains_value(scenemits, dim_keys(model, :time)[tt], "NOX_kt/yr", 1.) for tt in 1:dim_count(model, :time)]
    basenox = [get_gains_value(baseemits, dim_keys(model, :time)[tt], "NOX_kt/yr", 1.) for tt in 1:dim_count(model, :time)]

    fairgrounds[:e_nox_ratio] = scennox ./ basenox

    scenvoc = [get_gains_value(scenemits, dim_keys(model, :time)[tt], "VOC_kt/yr", 1.) for tt in 1:dim_count(model, :time)]
    basevoc = [get_gains_value(baseemits, dim_keys(model, :time)[tt], "VOC_kt/yr", 1.) for tt in 1:dim_count(model, :time)]

    fairgrounds[:e_voc_ratio] = scenvoc ./ basevoc

    if gains_scenario == "Baseline"
        fairgrounds[:tempscaling_2030] = 1.
    else
        fairgrounds[:tempscaling_2030] = 0.9
    end

    fairgrounds[:pulse_gas] = "none"
    fairgrounds[:pulse_year] = 2025
    fairgrounds[:pulse_size] = 1.

    return fairgrounds
end
