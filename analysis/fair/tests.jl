import Mimi.add_save!
include("../../src/main_model.jl")
include("../../src/mcs.jl")

model = getpage()
run(model)

getdataframe(model, :FaIRGrounds, :rt_g_globaltemperature)

mcs = getsim(model, 1000)

output_path = "output"
res = run(mcs, model, 1000; trials_output_filename=joinpath(output_path, "trialdata.csv"), results_output_dir=output_path)

import CSV
df = CSV.read("output/FaIRGrounds_rt_g_globaltemperature.csv", DataFrame)
df2 = combine(groupby(df, :time)) do sdf
    (mean = mean(sdf.rt_g_globaltemperature), std = std(sdf.rt_g_globaltemperature))
end
