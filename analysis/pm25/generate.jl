include("../../src/main_model.jl")
model = getpage()
run(model)

for scenario in [:Decarb, :Baseline]
    for useekc in [false, true]
        println([scenario, useekc])
        model = getpage("RCP4.5 & SSP2"; pm25_scenario=scenario, pm25_useekc=useekc)
        run(model)
        CSV.write("pmtotal_$(scenario)_$(useekc).csv", getdataframe(model, :PM25Pollution, :pm_total))
    end
end

model = getpage("RCP4.5 & SSP2"; pm25_scenario=:Baseline, pm25_useekc=false)
run(model)

## Monte Carlo for Baseline
import Mimi.add_save!
include("../../src/main_model.jl")
include("../../src/mcs.jl")

mcnum = 1000

model = getpage("RCP4.5 & SSP2"; pm25_scenario=:Baseline, pm25_useekc=true)
run(model)
mcs = getsim(model);
add_save!(mcs, (:PM25Pollution, :pm_total))
res = run(mcs, model, mcnum; trials_output_filename="trialdata.csv", results_output_dir="output")

CSV.write("pmtotal_$(scenario)_$(useekc).csv", getdataframe(model, :pm25_pollution, :pm_total))
