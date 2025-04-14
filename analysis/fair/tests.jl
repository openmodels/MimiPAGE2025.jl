include("../../src/main_model.jl")

model = getpage()
run(model)

getdataframe(model, :FaIRGrounds, :rt_g_globaltemperature)
