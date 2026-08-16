using Random
import Mimi.add_save!
include("../../src/main_model.jl")
include("../../src/mcs.jl")
include("../../src/compute_scc.jl")

pulse_gases = ["co2", "ch4", "n2o", "fgas", "nh3", "nox", "bc", "so2", "nmvoc"]
columns = ["CO2_Mt CO2/yr", "CH4_kt/yr", "N2O_kt/yr", "FGAS_Mt CO2eq/yr",
           "NH3_kt/yr", "NOX_kt/yr", "PM_BC_kt/yr", "SO2_kt/yr", "VOC_kt/yr"]
emits = CSV.read(pagedata("climate/gains-emissions.csv"), DataFrame, missingstring="NA")
emits2fair = [1., 1 / 1000., 1 / 1000., 1.,
              1 / 1000., 1 / 1000., 1 / 1000., 1 / 1000., 1 / 1000.]

## Find out the best size of a CH4 pulse
df = DataFrame(pulse_mt=Float64[], scc=Float64[], d2050dM=Float64[])
pp = 2
pulse_year = 2025
pulse_gas = pulse_gases[pp]
for pulse_size in emits[1, columns[pp]] .* exp.(range(log(.001), 0, 10))
    model = getpage("RCP4.5 & SSP2"; pm25_scenario="Baseline", config_capital="full", pm25_useekc=true, pm25_useext=false, pm25_gainsmatch=true, emissionfeedback=true, use_delays=false, vsl_calib=:oecd_global, use_gains_ghg=true, use_seaice=false, use_permafrost=false, config_nonmarketdmg="none")

    scc, _, mm = compute_scc(model, year=pulse_year, pulse_size=pulse_size, pulse_gas=pulse_gas)
    ## mm.base[:FaIRGrounds, :rt_g_globaltemperature]
    ## mm.modified[:FaIRGrounds, :rt_g_globaltemperature]

    row = [pulse_size, scc, mm[:FaIRGrounds, :rt_g_globaltemperature][dim_keys(model, :time) .== 2050][1]]
    println(row)
    push!(df, row)
end

for marketonly in [true, false]
    df = DataFrame(gas=String[], year=Int64[], scc=Float64[])
    for pp in 1:length(pulse_gases)
        pulse_gas = pulse_gases[pp]
        println(pulse_gas)
        for pulse_year in 2025:5:2100
            if marketonly
                model = getpage("RCP4.5 & SSP2"; pm25_scenario="Baseline", config_capital="full", pm25_useekc=true, pm25_useext=false, pm25_gainsmatch=true, emissionfeedback=true, use_delays=false, vsl_calib=:oecd_global, use_gains_ghg=true, use_seaice=false, use_permafrost=false, config_nonmarketdmg="none")
            else
                model = getpage("RCP4.5 & SSP2"; pm25_scenario="Baseline", config_capital="full", pm25_useekc=true, pm25_useext=false, pm25_gainsmatch=true, emissionfeedback=true, use_delays=false, vsl_calib=:oecd_global, use_gains_ghg=true, use_seaice=false, use_permafrost=false)
            end

            if pulse_gas == "so2"
                pulse_size = emits[1, columns[pp]] / 10
            else
                pulse_size = emits[1, columns[pp]] * emits2fair[pp]
            end
            scc, _, _ = compute_scc(model, year=pulse_year, pulse_size=pulse_size, pulse_gas=pulse_gas)
            println([pulse_year, scc])

            if pulse_gas == "n2o"
                push!(df, [columns[pp], pulse_year, scc * 0.6367])
            else
                push!(df, [columns[pp], pulse_year, scc])
            end

            #sccs, _, _ = compute_scc(model, year=pulse_year, pulse_size=pulse_size, pulse_gas=pulse_gas, n=10, seed=20260306)
        end
    end

    units = ["2017\$ / t CO2", "2017\$ / t CH4", "2017\$ / t N2O",
             "2017\$ / t FGAS", "2017\$ / t NH3", "2017\$ / t NOX",
             "2017\$ / t BC", "2017\$ / t SO2", "2017\$ / t VOC"]
    df.Units = [units[findfirst(columns .== gas)] for gas in df.gas]
    df."Pulse Year" = df.year
    df."SC-GHG" = df.scc * 100 / 81.551

    if marketonly
        CSV.write("sc-ghg-marketonly.csv", df[!, 4:6])
    else
        CSV.write("sc-ghg.csv", df[!, 4:6])
    end
end

dfs = [1; model[:EquityWeighting, :dfc_consumptiondiscountrate][:, 3]]
years = [2015; dim_keys(model, :time)]
log.(dfs[2:end] ./ dfs[1:end-1]) ./ diff(years)
