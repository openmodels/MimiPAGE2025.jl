import Mimi.add_save!
include("../../src/main_model.jl")
include("../../src/mcs.jl")

model = getpage()
run(model)

getdataframe(model, :FaIRGrounds, :rt_g_globaltemperature)

mcs = getsim(model)
add_save!(mcs, (:FaIRGrounds, :rt_g_globaltemperature))

output_path = "output"
res = run(mcs, model, 1000; trials_output_filename=joinpath(output_path, "trialdata.csv"), results_output_dir=output_path)

