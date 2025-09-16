using Plots

include("../../src/main_model.jl")
model = getpage("RCP4.5 & SSP2"; pm25_scenario=Symbol("Baseline_CLE"), use_delays=false)
run(model)
before = getdataframe(model, :FaIRGrounds, :rt_g_globaltemperature)
before_co2 = getdataframe(model, :FaIRGrounds, :e_globalCO2emissions)

model = getpage("RCP4.5 & SSP2"; pm25_scenario=Symbol("Baseline_CLE"))
run(model)
after = getdataframe(model, :FaIRGrounds, :rt_g_globaltemperature)
after_co2 = getdataframe(model, :FaIRGrounds, :e_globalCO2emissions)

tdiff = after.rt_g_globaltemperature .- before.rt_g_globaltemperature
cdiff = after_co2.e_globalCO2emissions .- before_co2.e_globalCO2emissions
dim_keys(model, :time)

p = plot(before.time, before.rt_g_globaltemperature, label="Without Delays", linewidth=2, linestyle=:dash,
         xlims=(2020, 2100), ylims=(0, 5))
plot!(after.time, after.rt_g_globaltemperature, label="With Delays", linewidth=2, linestyle=:solid)
xlabel!("Year")
ylabel!("Global Temperature Anomaly (°C)")
title!("Temperature Projections")
savefig("delay-gmst.pdf")

p = plot(before.time, before_co2.e_globalCO2emissions, label="Without Delays", linewidth=2, linestyle=:dash,
         xlims=(2020, 2100))
plot!(after.time, after_co2.e_globalCO2emissions, label="With Delays", linewidth=2, linestyle=:solid)
xlabel!("Year")
ylabel!("Global CO2 Emissions (Mt / year)")
title!("CO2 Emissions Projections")
savefig("delay-co2emit.pdf")


raw = sum(model[:PolicyDelay, :carbonprice_raw] .* model[:Population, :pop_population], dims=2) ./ sum(model[:Population, :pop_population], dims=2)

dly = sum(model[:PolicyDelay, :carbonprice] .* model[:Population, :pop_population], dims=2) ./ sum(model[:Population, :pop_population], dims=2)

DataFrame(year=dim_keys(model, :time), cred=vec(1 .- dly ./ raw))

mcs = getsim(model, 1000)
add_save!(mcs, (:CarbonPriceInfer, :carbonprice))
add_save!(mcs, (:PolicyDelay, :carbonprice))

res = run(mcs, model, 1000; trials_output_filename="output/trialdata.csv", results_output_dir="output")

before = getdataframe(res, :CarbonPriceInfer, :carbonprice)
after = getdataframe(res, :PolicyDelay, :carbonprice)
pop = getdataframe(res, :Population, :pop_population)

df = leftjoin(leftjoin(before, after, on=[:country, :time, :trialnum], makeunique=true), pop, on=[:country, :time, :trialnum])
df[!, :diff] = 1. .- (df.carbonprice_1 .+ .01) ./ (df.carbonprice .+ .01)
df2 = combine(grp -> sum(grp.diff .* grp.pop_population) / sum(grp.pop_population), groupby(df, [:trialnum, :time]))
df3 = combine(groupby(df2, :time), :x1 => (x -> [(mean(x), quantile(x, .25), quantile(x, .75))]) => [:mu, :ci25, :ci75])

plot(df3.time, df3.mu, ribbon=(df3.mu .- df3.ci25, df3.ci75 .- df3.mu),
     label="Mean", c=:blue, α=0.5, linewidth=2, legend=:topright,
     xlabel="Year", ylabel="Carbon price reduction (%)", title="Reduction in carbon price due to delays",
     xlims=(2020, 2100), yticks=(0:0.1:0.5, ["$(x * 100)%" for x in 0:0.1:0.5]))
savefig("delay-carbonprice.pdf")
