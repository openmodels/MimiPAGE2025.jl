@defcomp AdaptationCosts begin
    region = Index()

    model = Parameter{Model}()
    y_year_0 = Parameter(unit="year")
    y_year_lssp = Parameter(unit="year", default=2100.)
    y_year = Parameter(index=[time], unit="year")
    gdp = Parameter(index=[time, region], unit="\$M")
    cf_costregional = Parameter(index=[region], unit="none") # first value should be 1.

    automult_autonomoustechchange = Parameter(unit="none", default=.65)
    impmax_maximumadaptivecapacity = Parameter(index=[region], unit="driver")
    # tolerability parameters
    plateau_increaseintolerableplateaufromadaptation = Parameter(index=[region], unit="driver")
    pstart_startdateofadaptpolicy = Parameter(index=[region], unit="year")
    pyears_yearstilfulleffect = Parameter(index=[region], unit="year")
    impred_eventualpercentreduction = Parameter(index=[region], unit="%")
    istart_startdate = Parameter(index=[region], unit="year")
    iyears_yearstilfulleffect = Parameter(index=[region], unit="year")

    cp_costplateau_eu = Parameter(unit="%GDP/driver")
    ci_costimpact_eu = Parameter(unit="%GDP/%driver")

    atl_adjustedtolerablelevel = Variable(index=[time, region]) # Unit depends on instance (degreeC or m)
    imp_adaptedimpacts = Variable(index=[time, region], unit="%")

    # Mostly for debugging
    autofac_autonomoustechchangefraction = Variable(index=[time], unit="none")
    acp_adaptivecostplateau = Variable(index=[time, region], unit="\$million")
    aci_adaptivecostimpact = Variable(index=[time, region], unit="\$million")

    ac_adaptivecosts = Variable(index=[time, region], unit="\$million")

    # For mix-and-match
    gdp_national = Parameter(index=[time, country], unit="\$M")
    ac_adaptivecosts_national = Variable(index=[time, country], unit="\$million")
    sealevelcost_draw = Parameter{Int64}()

    function run_timestep(p, v, d, tt)

        # Hope (2009), p. 21, equation -5
        auto_autonomoustechchangepercent = (1 - p.automult_autonomoustechchange^(1 / (p.y_year_lssp - p.y_year_0))) * 100 # % per year
        v.autofac_autonomoustechchangefraction[tt] = (1 - auto_autonomoustechchangepercent / 100)^(p.y_year[tt] - p.y_year_0) # Varies by year

        for rr in d.region
            # calculate adjusted tolerable level and max impact based on adaptation policy
            if (p.y_year[tt] - p.pstart_startdateofadaptpolicy[rr]) < 0
                v.atl_adjustedtolerablelevel[tt,rr] = 0
            elseif ((p.y_year[tt] - p.pstart_startdateofadaptpolicy[rr]) / p.pyears_yearstilfulleffect[rr]) < 1.
                v.atl_adjustedtolerablelevel[tt,rr] =
                    ((p.y_year[tt] - p.pstart_startdateofadaptpolicy[rr]) / p.pyears_yearstilfulleffect[rr]) *
                    p.plateau_increaseintolerableplateaufromadaptation[rr]
            else
                v.atl_adjustedtolerablelevel[tt,rr] = p.plateau_increaseintolerableplateaufromadaptation[rr]
            end

            if (p.y_year[tt] - p.istart_startdate[rr]) < 0
                v.imp_adaptedimpacts[tt,rr] = 0
            elseif ((p.y_year[tt] - p.istart_startdate[rr]) / p.iyears_yearstilfulleffect[rr]) < 1
                v.imp_adaptedimpacts[tt,rr] =
                    (p.y_year[tt] - p.istart_startdate[rr]) / p.iyears_yearstilfulleffect[rr] *
                    p.impred_eventualpercentreduction[rr]
            else
                v.imp_adaptedimpacts[tt,rr] = p.impred_eventualpercentreduction[rr]
            end

            # Hope (2009), p. 25, equations 1-2
            cp_costplateau_regional = p.cp_costplateau_eu * p.cf_costregional[rr]
            ci_costimpact_regional = p.ci_costimpact_eu * p.cf_costregional[rr]

            # Hope (2009), p. 25, equations 3-4
            v.acp_adaptivecostplateau[tt, rr] = v.atl_adjustedtolerablelevel[tt, rr] * cp_costplateau_regional * p.gdp[tt, rr] * v.autofac_autonomoustechchangefraction[tt] / 100
            v.aci_adaptivecostimpact[tt, rr] = v.imp_adaptedimpacts[tt, rr] * ci_costimpact_regional * p.gdp[tt, rr] * p.impmax_maximumadaptivecapacity[rr] * v.autofac_autonomoustechchangefraction[tt] / 100

            # Hope (2009), p. 25, equation 5
            v.ac_adaptivecosts[tt, rr] = v.acp_adaptivecostplateau[tt, rr] + v.aci_adaptivecostimpact[tt, rr]
        end

        ac_adaptivecosts_perusd = v.ac_adaptivecosts[tt, :] ./ p.gdp[tt, :]
        v.ac_adaptivecosts_national[tt, :] = regiontocountry(p.model, ac_adaptivecosts_perusd) .* p.gdp_national[tt, :]
    end
end

function addadaptationcosts_economic(model::Model; config_marketdmg::String="default")
    adaptationcosts = add_comp!(model, AdaptationCosts, :AdaptiveCostsEconomic)
    adaptationcosts[:model] = model

    # Economic-specific parameters
    setdistinctparameter(model, :AdaptiveCostsEconomic, :impmax_maximumadaptivecapacity, readpagedata(model, "data/impmax_economic.csv"))
    setdistinctparameter(model, :AdaptiveCostsEconomic, :plateau_increaseintolerableplateaufromadaptation, readpagedata(model, "data/plateau_increaseintolerableplateaufromadaptationM.csv"))
    setdistinctparameter(model, :AdaptiveCostsEconomic, :pstart_startdateofadaptpolicy, readpagedata(model, "data/pstart_startdateofadaptpolicyM.csv"))
    setdistinctparameter(model, :AdaptiveCostsEconomic, :pyears_yearstilfulleffect, readpagedata(model, "data/pyears_yearstilfulleffectM.csv"))
    setdistinctparameter(model, :AdaptiveCostsEconomic, :impred_eventualpercentreduction, readpagedata(model, "data/impred_eventualpercentreductionM.csv"))
    setdistinctparameter(model, :AdaptiveCostsEconomic, :istart_startdate, readpagedata(model, "data/istart_startdateM.csv"))
    setdistinctparameter(model, :AdaptiveCostsEconomic, :iyears_yearstilfulleffect, readpagedata(model, "data/iyears_yearstilfulleffectM.csv"))
    if config_marketdmg == "none"
        setdistinctparameter(model, :AdaptiveCostsEconomic, :cp_costplateau_eu, 0.)
        setdistinctparameter(model, :AdaptiveCostsEconomic, :ci_costimpact_eu, 0.)
    else
        setdistinctparameter(model, :AdaptiveCostsEconomic, :cp_costplateau_eu, 0.0116666666666667)
        setdistinctparameter(model, :AdaptiveCostsEconomic, :ci_costimpact_eu, 0.0040000000)
    end

    # For mix-and-match
    setdistinctparameter(model, :AdaptiveCostsEconomic, :sealevelcost_draw, 0)

    return adaptationcosts
end

function addadaptationcosts_noneconomic(model::Model; config_nonmarketdmg::String="default")
    adaptationcosts = add_comp!(model, AdaptationCosts, :AdaptiveCostsNonEconomic)
    adaptationcosts[:model] = model

    # Non-economic-specific parameters
    setdistinctparameter(model, :AdaptiveCostsNonEconomic, :impmax_maximumadaptivecapacity, readpagedata(model, "data/impmax_noneconomic.csv"))
    setdistinctparameter(model, :AdaptiveCostsNonEconomic, :plateau_increaseintolerableplateaufromadaptation, readpagedata(model, "data/plateau_increaseintolerableplateaufromadaptationNM.csv"))
    setdistinctparameter(model, :AdaptiveCostsNonEconomic, :pstart_startdateofadaptpolicy, readpagedata(model, "data/pstart_startdateofadaptpolicyNM.csv"))
    setdistinctparameter(model, :AdaptiveCostsNonEconomic, :pyears_yearstilfulleffect, readpagedata(model, "data/pyears_yearstilfulleffectNM.csv"))
    setdistinctparameter(model, :AdaptiveCostsNonEconomic, :impred_eventualpercentreduction, readpagedata(model, "data/impred_eventualpercentreductionNM.csv"))
    setdistinctparameter(model, :AdaptiveCostsNonEconomic, :istart_startdate, readpagedata(model, "data/istart_startdateNM.csv"))
    setdistinctparameter(model, :AdaptiveCostsNonEconomic, :iyears_yearstilfulleffect, readpagedata(model, "data/iyears_yearstilfulleffectNM.csv"))
    if config_nonmarketdmg == "none"
        setdistinctparameter(model, :AdaptiveCostsNonEconomic, :cp_costplateau_eu, 0.)
        setdistinctparameter(model, :AdaptiveCostsNonEconomic, :ci_costimpact_eu, 0.)
    else
        setdistinctparameter(model, :AdaptiveCostsNonEconomic, :cp_costplateau_eu, 0.0233333333333333)
        setdistinctparameter(model, :AdaptiveCostsNonEconomic, :ci_costimpact_eu, 0.00566666666666667)
    end

    # For mix-and-match
    setdistinctparameter(model, :AdaptiveCostsNonEconomic, :sealevelcost_draw, 0)

    return adaptationcosts
end

function addadaptationcosts_sealevel_regional(model::Model; config_slrdmg::String="default")
    adaptationcosts = add_comp!(model, AdaptationCosts, :AdaptiveCostsSeaLevel)
    adaptationcosts[:model] = model

    # Sea Level-specific parameters
    setdistinctparameter(model, :AdaptiveCostsSeaLevel, :impmax_maximumadaptivecapacity, readpagedata(model, "data/regional/impmax_sealevel.csv"))
    setdistinctparameter(model, :AdaptiveCostsSeaLevel, :plateau_increaseintolerableplateaufromadaptation, readpagedata(model, "data/regional/sealevel_plateau.csv"))
    setdistinctparameter(model, :AdaptiveCostsSeaLevel, :pstart_startdateofadaptpolicy, readpagedata(model, "data/regional/sealeveladaptstart.csv"))
    setdistinctparameter(model, :AdaptiveCostsSeaLevel, :pyears_yearstilfulleffect, readpagedata(model, "data/regional/sealeveladapttimetoeffect.csv"))
    setdistinctparameter(model, :AdaptiveCostsSeaLevel, :impred_eventualpercentreduction, readpagedata(model, "data/regional/sealevelimpactreduction.csv"))
    setdistinctparameter(model, :AdaptiveCostsSeaLevel, :istart_startdate, readpagedata(model, "data/regional/sealevelimpactstart.csv"))
    setdistinctparameter(model, :AdaptiveCostsSeaLevel, :iyears_yearstilfulleffect, readpagedata(model, "data/regional/sealevelimpactyearstoeffect.csv"))
    if config_slrdmg == "none"
        setdistinctparameter(model, :AdaptiveCostsSeaLevel, :cp_costplateau_eu, 0.)
        setdistinctparameter(model, :AdaptiveCostsSeaLevel, :ci_costimpact_eu, 0.)
    else
        setdistinctparameter(model, :AdaptiveCostsSeaLevel, :cp_costplateau_eu, 0.0233333333333333)
        setdistinctparameter(model, :AdaptiveCostsSeaLevel, :ci_costimpact_eu, 0.00116666666666667)
    end

    # For mix-and-match
    setdistinctparameter(model, :AdaptiveCostsSeaLevel, :sealevelcost_draw, 0)

    return adaptationcosts
end
