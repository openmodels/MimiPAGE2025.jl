function price2frac(carbonprice, minp, maxp)
    if maxp == Inf
        max.(0, min.((carbonprice .- minp) ./ maxp, 1))
    else
        max.(0, min.((carbonprice .- minp) ./ (maxp - minp), 1))
    end
end

macs = myloadcsv("data/macs.csv")

@defcomp AbatementCostsCO2 begin
    country = Index()
    region = Index()

    # Uncertainty parameters
    model = Parameter{Model}()
    y_year = Parameter(index=[time], unit="year")
    mac_draw = Parameter{Int64}()

    bau_co2emissions = Parameter(index=[time, region], unit="%")
    e0_baselineCO2emissions_country = Parameter(index=[country], unit="Mtonne/year")
    baselineemit = Variable(index=[time, country], unit="MtCO2/year")

    gdp = Parameter(index=[time, country], unit="\$M")
    carbonprice = Parameter(index=[time, country], unit="\$2010/tCO2")

    ## Parameters set by init to MC values

    # Decrease in CO2 for a given tax
    ac_0_20_co2 = Variable(index=[country], unit="MtCO2/\$2010")
    ac_20_50_co2 = Variable(index=[country], unit="MtCO2/\$2010")
    ac_50_100_co2 = Variable(index=[country], unit="MtCO2/\$2010")
    ac_100_200_co2 = Variable(index=[country], unit="MtCO2/\$2010")
    ac_200_500_co2 = Variable(index=[country], unit="MtCO2/\$2010")
    ac_500_inf_co2 = Variable(index=[country], unit="MtCO2/\$2010")
    ac_0_20xyear_co2 = Variable(index=[country], unit="MtCO2/\$2010/year")
    ac_20_50xyear_co2 = Variable(index=[country], unit="MtCO2/\$2010/year")
    ac_50_100xyear_co2 = Variable(index=[country], unit="MtCO2/\$2010/year")
    ac_100_200xyear_co2 = Variable(index=[country], unit="MtCO2/\$2010/year")
    ac_200_500xyear_co2 = Variable(index=[country], unit="MtCO2/\$2010/year")
    ac_500_infxyear_co2 = Variable(index=[country], unit="MtCO2/\$2010/year")
    lag_value_co2 = Variable(index=[country], unit="MtCO2")

    # Decrease in GDP for a given tax
    ac_0_20_gdp = Variable(index=[country], unit="\$2010/\$2010")
    ac_0_20_gdp = Variable(index=[country], unit="\$2010/\$2010")
    ac_20_50_gdp = Variable(index=[country], unit="\$2010/\$2010")
    ac_50_100_gdp = Variable(index=[country], unit="\$2010/\$2010")
    ac_100_200_gdp = Variable(index=[country], unit="\$2010/\$2010")
    ac_200_500_gdp = Variable(index=[country], unit="\$2010/\$2010")
    ac_500_inf_gdp = Variable(index=[country], unit="\$2010/\$2010")
    ac_0_20xyear_gdp = Variable(index=[country], unit="\$2010/\$2010/year")
    ac_20_50xyear_gdp = Variable(index=[country], unit="\$2010/\$2010/year")
    ac_50_100xyear_gdp = Variable(index=[country], unit="\$2010/\$2010/year")
    ac_100_200xyear_gdp = Variable(index=[country], unit="\$2010/\$2010/year")
    ac_200_500xyear_gdp = Variable(index=[country], unit="\$2010/\$2010/year")
    ac_500_infxyear_gdp = Variable(index=[country], unit="\$2010/\$2010/year")
    lag_value_gdp = Variable(index=[country], unit="\$2010")

    fracabatedcarbon = Variable(index=[time, country], unit="portion") # portion abated
    loggdpcost = Variable(index=[time, country], unit="log diff") # log difference
    tc_totalcost_national = Variable(index=[time, country], unit="\$million")
    tc_totalcost_region = Variable(index=[time, region], unit="\$million")

    function init(pp, vv, dd)
        if pp.mac_draw == -1
            macs2 = im_to_i(macs, "iso", "bs", nothing)
        else
            macs2 = im_to_i(macs, "iso", "bs", pp.mac_draw)
        end

        vv.ac_0_20_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_0-20_co2")
        vv.ac_20_50_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_20-50_co2")
        vv.ac_50_100_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_50-100_co2")
        vv.ac_100_200_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_100-200_co2")
        vv.ac_200_500_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_200-500_co2")
        vv.ac_500_inf_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_500-inf_co2")
        vv.ac_0_20xyear_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_0-20xyear_co2")
        vv.ac_20_50xyear_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_20-50xyear_co2")
        vv.ac_50_100xyear_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_50-100xyear_co2")
        vv.ac_100_200xyear_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_100-200xyear_co2")
        vv.ac_200_500xyear_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_200-500xyear_co2")
        vv.ac_500_infxyear_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_500-infxyear_co2")
        vv.lag_value_co2[:] = readcountrydata_i_const(pp.model, macs2, "iso", "lag_value_co2")
        vv.ac_0_20_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_0-20_gdp")
        vv.ac_20_50_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_20-50_gdp")
        vv.ac_50_100_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_50-100_gdp")
        vv.ac_100_200_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_100-200_gdp")
        vv.ac_200_500_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_200-500_gdp")
        vv.ac_500_inf_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_500-inf_gdp")
        vv.ac_0_20xyear_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_0-20xyear_gdp")
        vv.ac_20_50xyear_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_20-50xyear_gdp")
        vv.ac_50_100xyear_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_50-100xyear_gdp")
        vv.ac_100_200xyear_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_100-200xyear_gdp")
        vv.ac_200_500xyear_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_200-500xyear_gdp")
        vv.ac_500_infxyear_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "ac_500-infxyear_gdp")
        vv.lag_value_gdp[:] = readcountrydata_i_const(pp.model, macs2, "iso", "lag_value_gdp")
    end

    function run_timestep(pp, vv, dd, tt)
        ac_0_20_co2 = vv.ac_0_20_co2 + vv.ac_0_20xyear_co2 * (gettime(tt) - 2000)
        ac_20_50_co2 = vv.ac_20_50_co2 + vv.ac_20_50xyear_co2 * (gettime(tt) - 2000)
        ac_50_100_co2 = vv.ac_50_100_co2 + vv.ac_50_100xyear_co2 * (gettime(tt) - 2000)
        ac_100_200_co2 = vv.ac_100_200_co2 + vv.ac_100_200xyear_co2 * (gettime(tt) - 2000)
        ac_200_500_co2 = vv.ac_200_500_co2 + vv.ac_200_500xyear_co2 * (gettime(tt) - 2000)
        ac_500_inf_co2 = vv.ac_500_inf_co2 + vv.ac_500_infxyear_co2 * (gettime(tt) - 2000)

        rawtonnesabated = ac_0_20_co2 .* price2frac(pp.carbonprice[tt, :], 0, 20) +
            ac_20_50_co2 .* price2frac(pp.carbonprice[tt, :], 20, 50) +
            ac_50_100_co2 .* price2frac(pp.carbonprice[tt, :], 50, 100) +
            ac_100_200_co2 .* price2frac(pp.carbonprice[tt, :], 100, 200) +
            ac_200_500_co2 .* price2frac(pp.carbonprice[tt, :], 200, 500) +
            ac_500_inf_co2 .* price2frac(pp.carbonprice[tt, :], 500, Inf) # MtCO2

        # Calculate baseline emissions
        vv.baselineemit[tt, :] = pp.e0_baselineCO2emissions_country .* regiontocountry(pp.model, pp.bau_co2emissions[tt, :]) / 100

        rawfractargetabated = -rawtonnesabated ./ vv.baselineemit[tt,:] # fraction abated
        # Regularize so not over 1 and goes to 1 as p -> inf
        regfractargetabated = rawfractargetabated ./ (exp.(-pp.carbonprice[tt, :] / 500) + rawfractargetabated)
        regfractargetabated[rawfractargetabated .> 1.] .= rawfractargetabated[rawfractargetabated .> 1.]

        # Use autoreg factor to approach target
        if is_first(tt)
            vv.fracabatedcarbon[tt,:] = regfractargetabated
        else
            # delta y = (y_goal - y_t) / tau = y_t+1 - y_t
            #   => y_t+1 = y_goal / tau + (1 - 1 / tau) y_t
            #   lag_value_co2 is (1 - 1 / tau)
            invtau = (1 .- vv.lag_value_co2)
            invtau[invtau .<= 0] .= mean(invtau[invtau .> 0])
            yp_yearsperiod = pp.y_year[tt] - pp.y_year[tt - 1]
            # G v + (1 - v) y -> G v + (1 - v) (G v + (1 - v) y)
            # -> G (2v - v^2) + (1 - v)^2 y
            autoreg = (1 .- invtau).^(yp_yearsperiod / 5)
            vv.fracabatedcarbon[tt,:] = regfractargetabated .* (1 .- autoreg) .+ autoreg .* vv.fracabatedcarbon[tt-1,:]
        end

        ac_0_20_gdp = vv.ac_0_20_gdp + vv.ac_0_20xyear_gdp * (2050 - gettime(tt))
        ac_20_50_gdp = vv.ac_20_50_gdp + vv.ac_20_50xyear_gdp * (2050 - gettime(tt))
        ac_50_100_gdp = vv.ac_50_100_gdp + vv.ac_50_100xyear_gdp * (2050 - gettime(tt))
        ac_100_200_gdp = vv.ac_100_200_gdp + vv.ac_100_200xyear_gdp * (2050 - gettime(tt))
        ac_200_500_gdp = vv.ac_200_500_gdp + vv.ac_200_500xyear_gdp * (2050 - gettime(tt))
        ac_500_inf_gdp = vv.ac_500_inf_gdp + vv.ac_500_infxyear_gdp * (2050 - gettime(tt))

        # Drop "vv.lag_value_gdp .* gdplogdiff", since want difference from baseline, but this is baseline
        vv.loggdpcost[tt,:] = ac_0_20_gdp .* price2frac(pp.carbonprice[tt, :], 0, 20) +
            ac_20_50_gdp .* price2frac(pp.carbonprice[tt, :], 20, 50) +
            ac_50_100_gdp .* price2frac(pp.carbonprice[tt, :], 50, 100) +
            ac_100_200_gdp .* price2frac(pp.carbonprice[tt, :], 100, 200) +
            ac_200_500_gdp .* price2frac(pp.carbonprice[tt, :], 200, 500) +
            ac_500_inf_gdp .* price2frac(pp.carbonprice[tt, :], 500, Inf) # log difference
        vv.loggdpcost[tt,:] = min.(0, vv.loggdpcost[tt,:])

        vv.tc_totalcost_national[tt,:] = pp.gdp[tt,:] .* (1 .- exp.(vv.loggdpcost[tt,:]))
        vv.tc_totalcost_region[tt, :] = countrytoregion(pp.model, sum, vv.tc_totalcost_national[tt,:])
    end
end

function addabatementcostsco2(model::Model)
    abatementcostscomp = add_comp!(model, AbatementCostsCO2)

    abatementcostscomp[:model] = model
    abatementcostscomp[:mac_draw] = -1
    abatementcostscomp[:carbonprice] = zeros(dim_count(model, :time), dim_count(model, :country))

    return abatementcostscomp
end
