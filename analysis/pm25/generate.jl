import Mimi.add_save!
include("../../src/main_model.jl")
include("../../src/mcs.jl")

model = getpage()
run(model)

df = getdataframe(model, :PM25Pollution, :pm_total)
df[df.country .== "AUS", :]

mcnum = 100

for scenario_eco in ["SSP2", "SSP3"]
    for scenario_ghg in ["Baseline", "Decarb"]
        rcp = (scenario_ghg == "Baseline" ? "RCP4.5" : "RCP2.6")
        for scenario_pol in ["CLE", "MTFR"]
            for useekc in [false, true]
                println([scenario_eco, scenario_ghg, scenario_pol, useekc])
                model = getpage("$(rcp) & $(scenario_eco)"; pm25_scenario=Symbol("$(scenario_ghg)_$(scenario_pol)"), pm25_useekc=useekc)
                run(model)
                CSV.write("pmtotal_$(scenario_eco)_$(scenario_ghg)_$(scenario_pol)_$(useekc).csv", getdataframe(model, :PM25Pollution, :pm_total))

                mcs = getsim(model);
                add_save!(mcs, (:PM25Pollution, :pm_total))
                res = run(mcs, model, mcnum; trials_output_filename="trialdata.csv", results_output_dir="output_$(scenario_eco)_$(scenario_ghg)_$(scenario_pol)_$(useekc)")
            end
        end
    end
end
