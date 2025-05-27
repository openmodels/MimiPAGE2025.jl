using Statistics

include("../utils/country_tools.jl")

@defcomp ch4emissions begin
    region = Index()
    country = Index()

    model = Parameter{Model}()

    e0_baselineCH4emissions = Parameter(index=[country], unit="Mtonne/year")
    e0_baselineCH4emissions_region = Variable(index=[region], unit="Mtonne/year")

    er_CH4emissionsgrowth_region = Parameter(index=[time,region], unit="%")

    e_regionalCH4emissions_region = Variable(index=[time, region], unit="Mtonne/year")
    e_regionalCH4emissions = Variable(index=[time, country], unit="Mtonne/year")

    e_globalCH4emissions = Variable(index=[time], unit="Mtonne/year")

    # read in counterfactual GDP in absence of growth effects (gdp_leveleffects) and actual GDP
    gdp = Parameter(index=[time, country], unit="\$M")
    gdp_leveleffect = Parameter(index=[time, country], unit="\$M")
    emfeed_emissionfeedback = Parameter{Bool}(unit="none", default=true)

    function run_timestep(p, v, d, t)
        # eq.4 in Hope (2006) - regional CH4 emissions as % change from baseline
        er_CH4emissionsgrowth = regiontocountry(p.model, p.er_CH4emissionsgrowth_region[t, :])
        for cc in d.country
            v.e_regionalCH4emissions[t, cc] = er_CH4emissionsgrowth[cc] * p.e0_baselineCH4emissions[cc] / 100

            # rescale emissions based on GDP deviation from original scenario pathway
            if p.emfeed_emissionfeedback
                v.e_regionalCH4emissions[t, cc] = v.e_regionalCH4emissions[t, cc] * (p.gdp[t, cc] / p.gdp_leveleffect[t, cc])
            end
        end

        vv.e0_baselineCH4emissions_region[:] = countrytoregion(pp.model, sum, pp.e0_baselineCH4emissions)

        # eq. 5 in Hope (2006) - global CH4 emissions are sum of regional emissions
        v.e_globalCH4emissions[t] = sum(v.e_regionalCH4emissions[t, :])
    end
end

function addch4emissions(model::Model)
    ch4emit = add_comp!(model, ch4emissions)

    ch4emit[:model] = model
    baselineemits = CSV.read(pagedata("climate/ch4emit.csv"), DataFrame)
    baselineemits2 = DataFrame(iso=baselineemits[!, 1], value=vec(mean(Matrix(baselineemits[!, 2:12]), dims=2)) / 1000) # Convert kt to Mt
    e0_baselineCH4emissions = readcountrydata_i_const(model, baselineemits2, :iso, :value, sum; allowmissing=true)
    e0_baselineCH4emissions[ismissing.(e0_baselineCH4emissions)] .= 0
    ch4emit[:e0_baselineCH4emissions] = convert(Vector{Float64}, e0_baselineCH4emissions)

    return ch4emit
end
