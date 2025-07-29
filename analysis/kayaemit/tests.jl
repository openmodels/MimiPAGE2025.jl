include("../../src/main_model.jl")

for emissionfeedback in [false, true]
    model = getpage(emissionfeedback=emissionfeedback)
    run(model)
    println([emissionfeedback, model[:FaIRGrounds, :rt_g_globaltemperature][10]])
end
