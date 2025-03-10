include("../../src/main_model.jl")

# Test 1: Unconnected capital looks okay?
# Test 2: Using savings rate everywhere looks okay?
# Test 3: Adding in feedback looks okay?

model = getpage(config_capital="full")
run(model)

df = getdataframe(model, :Capital, :fracloss)
quantile(df.fracloss[df.time .== 2100], [0, .25, .75, 1])
df[(df.time .== 2100) .& (df.fracloss .> .1), :]

df = getdataframe(model, :Capital, :capital_baseline)
quantile(df.capital_baseline[df.time .== 2100] ./ df.capital_baseline[df.time .== 2020], [0, .25, .75, 1])
findfirst(df.capital_baseline[df.time .== 2100] ./ df.capital_baseline[df.time .== 2020] .> 20)
df[df.time .== 2100, :][19, :]
