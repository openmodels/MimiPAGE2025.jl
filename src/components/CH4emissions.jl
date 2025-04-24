using Statistics

include("../utils/country_tools.jl")

@defcomp ch4emissions begin
    region = Index()
    country = Index()

    model = Parameter{Model}()
    
    y_year = Parameter(index=[time], unit="year")
    y_year_0 = Parameter(unit="year")

    e0_baselineCH4emissions = Parameter(index=[country], unit="Mtonne/year")
    e0_baselineCH4emissions_region = Variable(index=[region], unit="Mtonne/year")

    er_CH4emissionsgrowth_region = Parameter(index=[time,region], unit="%")

    e_regionalCH4emissions_region = Variable(index=[time, region], unit="Mtonne/year")
    e_regionalCH4emissions = Variable(index=[time, country], unit="Mtonne/year")

    e_globalCH4emissions = Variable(index=[time], unit="Mtonne/year")
    
    # === Variables needed for PM2.5 pollution ===
    logch40        = Variable(index=[time, country], unit="log(Mtonne/year)")
    logch40xyear0  = Variable(index=[time, country], unit="log(Mtonne/year) * year")


    function init(pp, vv, dd)
        vv.e0_baselineCH4emissions_region[:] = countrytoregion(pp.model, sum, pp.e0_baselineCH4emissions)
    end

    function run_timestep(p, v, d, t)
        # eq.4 in Hope (2006) - regional CH4 emissions as % change from baseline
        for r in d.region
            v.e_regionalCH4emissions_region[t,r] = p.er_CH4emissionsgrowth_region[t,r] * v.e0_baselineCH4emissions_region[r] / 100
        end

        er_CH4emissionsgrowth = regiontocountry(p.model, p.er_CH4emissionsgrowth_region[t, :])
        v.e_regionalCH4emissions[t, :] = er_CH4emissionsgrowth .* p.e0_baselineCH4emissions[:] / 100

        # eq. 5 in Hope (2006) - global CH4 emissions are sum of regional emissions
        v.e_globalCH4emissions[t] = sum(v.e_regionalCH4emissions[t, :])
        
        # Compute log-transformed emissions for PM2.5 component
        for cc in d.country
            v.logch40[t, cc] = log(v.e_regionalCH4emissions[t, cc] + 1e-6)
            v.logch40xyear0[t, cc] = v.logch40[t, cc] * (p.y_year[t] - p.y_year_0)
        end
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
