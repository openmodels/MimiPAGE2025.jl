using Interpolations

dasgupta_reg2reg = Dict("Global" => [], "Asia" => ["China", "SEAsia"], "Europe" => ["EU", "USSR", "OECD"],
                        "Africa" => ["Africa"], "Americas" => ["USA", "LatAmerica"])

@defcomp DasguptaLabor begin
    country = Index()
    dasguptaregion = Index()
    time = Index()

    model = Parameter{Model}()

    rtl_realizedtemperature_absolute = Parameter(index=[time, country], unit="degreeC")
    rtl_0_realizedtemperature_absolute = Parameter(index=[country], unit="degreeC")
    rt_g_globaltemperature = Parameter(index=[time], unit="degreeC")
    gdp = Parameter(index=[time, country], unit="million US\$2005/yr")
    pop_population = Parameter(index=[time, country], unit="million person")

    exposure_high = Parameter(index=[country], unit="fraction")
    labor_share = Parameter(index=[country], unit="fraction")

    alpha_high = Parameter(index=[country])
    beta_high = Parameter(index=[country])
    alpha_low = Parameter(index=[country])
    beta_low = Parameter(index=[country])
    effective_15_low = Parameter(index=[dasguptaregion], unit="percent")
    effective_20_low = Parameter(index=[dasguptaregion], unit="percent")
    effective_30_low = Parameter(index=[dasguptaregion], unit="percent")
    effective_15_high = Parameter(index=[dasguptaregion], unit="percent")
    effective_20_high = Parameter(index=[dasguptaregion], unit="percent")
    effective_30_high = Parameter(index=[dasguptaregion], unit="percent")

    loglabor = Variable(index=[time, country])

    effective_low_region = Variable(index=[time, dasguptaregion], unit="percent")
    effective_high_region = Variable(index=[time, dasguptaregion], unit="percent")

    effective = Variable(index=[time, country], unit="percent")

    damages = Variable(index=[time, country], unit="million US\$2005/yr")

    function run_timestep(pp, vv, dd, tt)
        vv.loglabor[tt, :] = (pp.alpha_high .* (pp.rtl_realizedtemperature_absolute[tt, :] .- pp.rtl_0_realizedtemperature_absolute) .+ pp.beta_high .* (pp.rtl_realizedtemperature_absolute[tt, :].^2 .- pp.rtl_0_realizedtemperature_absolute.^2)) .* pp.exposure_high .+ (pp.alpha_low .* (pp.rtl_realizedtemperature_absolute[tt, :] .- pp.rtl_0_realizedtemperature_absolute) .+ pp.beta_low .* (pp.rtl_realizedtemperature_absolute[tt, :].^2 .- pp.rtl_0_realizedtemperature_absolute.^2)) .* (1 .- pp.exposure_high)

        ## Model:
        ## effective_i ~ T_i^2
        ## effective_region = sum(pop_i * effective_i) / sum(pop_i)
        countrymapping = get_countrymapping()
        countries = dim_keys(pp.model, :country)
        for rr in dd.dasguptaregion
            effective_low_linint = linear_interpolation([0., 1.5, 2., 3.], [0., pp.effective_15_low[rr], pp.effective_20_low[rr], pp.effective_30_low[rr]], extrapolation_bc=Line())
            effective_high_linint = linear_interpolation([0., 1.5, 2., 3.], [0., pp.effective_15_high[rr], pp.effective_20_high[rr], pp.effective_30_high[rr]], extrapolation_bc=Line())

            vv.effective_low_region[tt, rr] = effective_low_linint(pp.rt_g_globaltemperature[tt])
            vv.effective_high_region[tt, rr] = effective_high_linint(pp.rt_g_globaltemperature[tt])

            indexes = []
            for region in dasgupta_reg2reg[dim_keys(pp.model, :dasguptaregion)[rr]]
                append!(indexes, [findfirst(country .== countries) for country in countrymapping[region]])
            end
            indexes = Int64[x for x in indexes if !isnothing(x)]

            ## 1. pop-weighted average of countries is effective_region: sum(pop_i * effective_i) / sum(pop_i) = effective
            ## 2. country-specific value is proportional to factor: effective_i = k * factor_i * effective
            ## So, k * effective * sum(pop_i * factor_i) / sum(pop_i) = effective

            countryfactors = (pp.rtl_realizedtemperature_absolute[tt, indexes].^2 .- pp.rtl_0_realizedtemperature_absolute[indexes].^2)
            scalefactor = sum(pp.pop_population[tt, indexes]) / sum(pp.pop_population[tt, indexes] .* countryfactors)

            effective_low = scalefactor * countryfactors * vv.effective_low_region[tt, rr]
            effective_high = scalefactor * countryfactors * vv.effective_high_region[tt, rr]

            vv.effective[tt, indexes] = effective_high .* pp.exposure_high[indexes] .+ effective_low .* (1 .- pp.exposure_high[indexes])
        end

        vv.damages[tt, :] = pp.gdp[tt, :] .* (1 .- vv.effective[tt, :] / 100.) .^ pp.labor_share
    end
end

function adddasguptalabor(model::Model)
    comp = add_comp!(model, DasguptaLabor)

    comp[:model] = model

    exposure_high = readcountrydata_i_const(model, "damages/API_SL.AGR.EMPL.ZS_DS2_en_csv_v2_1327.csv", "Country Code", "2020") ./ 100
    exposure_high2 = coalesce.(exposure_high, mean(skipmissing(exposure_high)))
    comp[:exposure_high] = exposure_high2
    comp[:labor_share] = readcountrydata_i_const(model, "damages/labor-share-of-gdp.csv", "Code", "10.4.1 - Labour share of GDP (%) - SL_EMP_GTOTL") / 100
    comp[:rtl_0_realizedtemperature_absolute] = get_countryinfo().Temp2010

    # Don't bother with uncertainty: all SEs except Europe are 0
    df = myloadcsv("damages/dasgupta-labor.csv")
    # Calculate betas off of peak instead
    ## alpha T + beta T^2 => alpha / (2 * beta) = peak => beta = alpha / (2 * peak)
    df.T2_high = -df.T_high ./ (2 .* df.peak_high)
    df.T2_low = -df.T_low ./ (2 .* df.peak_low)
    reg2reg = Dict("SEAsia" => "Asia", "EU" => "Europe", "USSR" => "Europe", "Africa" => "Africa",
                   "LatAmerica" => "Americas", "OECD" => "Europe",
                   "China" => "Asia", "USA" => "Americas")
    par2col = Dict(:alpha_high => :T_high, :beta_high => :T2_high,
                   :alpha_low => :T_low, :beta_low => :T2_low)
    for (par, col) in collect(par2col)
        byregion = [df[findfirst(df.Region .== reg2reg[region]), col] for region in dim_keys(model, :region)]
        comp[par] = regiontocountry(model, byregion)
    end

    df2 = myloadcsv("damages/dasgupta-labor-effective.csv")
    par2col = Dict(:effective_15_low => :low_1_5, :effective_20_low => :low_2,
                   :effective_30_low => :low_3, :effective_15_high => :high_1_5,
                   :effective_20_high => :high_2, :effective_30_high => :high_3)
    for (par, col) in collect(par2col)
        comp[par] = [df2[findfirst(df2.Region .== region), col] for region in dim_keys(model, :dasguptaregion)]
    end

    comp
end
