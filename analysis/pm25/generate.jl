include("main_model.jl"); model = getpage(); run(model)

for scenario in [:Decarb, :Baseline]
    for useekc in [false, true]
        println([scenario, useekc])
        model = getpage("RCP4.5 & SSP2"; pm25_scenario=scenario, pm25_useekc=useekc)
        run(model)
        CSV.write("pmtotal_$(scenario)_$(useekc).csv", getdataframe(model, :pm25_pollution, :pm_total))
    end
end

model = getpage("RCP4.5 & SSP2"; pm25_scenario=:Baseline, pm25_useekc=false)
run(model)
