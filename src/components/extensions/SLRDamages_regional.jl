@defcomp SLRDamages_regional begin
    region = Index()

    model = Parameter{Model}()
    y_year = Parameter(index=[time], unit="year")
    y_year_0 = Parameter(unit="year")

    ## Country-level inputs for mix-and-match
    gdp = Parameter(index=[time, country], unit="\$M") # ignore
    pop_population = Parameter(index=[time, country], unit="million person")
    cons_percap_consumption = Parameter(index=[time, country], unit="\$/person")
    cons_percap_consumption_0 = Parameter(index=[country], unit="\$/person")
    tct_per_cap_totalcostspercap = Parameter(index=[time,country], unit="\$/person")
    act_percap_adaptationcosts = Parameter(index=[time, country], unit="\$/person")
    sealevelcost_draw = Parameter{Int64}()

    # incoming parameters from SeaLevelRise
    s_sealevel = Parameter(index=[time], unit="m")

    # component parameters
    impmax_maxSLRforadaptpolicySLR = Parameter(index=[region], unit="m")

    save_savingsrate = Parameter(unit="%", default=15.00) # pp33 PAGE09 documentation, "savings rate".
    wincf_weightsfactor_sea = Parameter(index=[region], unit="")
    W_SatCalibrationSLR = Parameter(default=1.0) # pp33 PAGE09 documentation, "Sea level impact at calibration sea level rise"
    ipow_SLRIncomeFxnExponent = Parameter(default=-0.30)
    pow_SLRImpactFxnExponent = Parameter(default=0.7333333333333334)
    iben_SLRInitialBenefit = Parameter(default=0.00)
    scal_calibrationSLR = Parameter(default=0.5)
    GDP_per_cap_focus_0_FocusRegionEU = Parameter(unit="\$/person", default=34298.93698672955)

    # component variables
    cons_percap_aftercosts_regional = Variable(index=[time, region], unit="\$/person")
    gdp_percap_aftercosts_regional = Variable(index=[time, region], unit="\$/person")

    atl_adjustedtolerablelevelofsealevelrise = Parameter(index=[time,region], unit="m") # meter
    imp_actualreductionSLR = Parameter(index=[time, region], unit="%")
    i_regionalimpactSLR = Variable(index=[time, region], unit="m")

    iref_ImpactatReferenceGDPperCapSLR = Variable(index=[time, region])
    igdp_ImpactatActualGDPperCapSLR = Variable(index=[time, region])

    isatg_impactfxnsaturation = Parameter(unit="unitless")
    isat_ImpactinclSaturationandAdaptationSLR = Variable(index=[time,region])

    isat_per_cap_SLRImpactperCapinclSaturationandAdaptation_regional = Variable(index=[time, region], unit="\$/person")
    rcons_per_cap_SLRRemainConsumption_regional = Variable(index=[time, region], unit="\$/person") # include?
    rgdp_per_cap_SLRRemainGDP_regional = Variable(index=[time, region], unit="\$/person")

    ## Country-level outputs for mix-and-match
    rcons_per_cap_SLRRemainConsumption = Variable(index=[time, country], unit="\$/person")
    rgdp_per_cap_SLRRemainGDP = Variable(index=[time, country], unit="\$/person")
    cons_percap_aftercosts = Variable(index=[time, country], unit="\$/person")
    gdp_percap_aftercosts = Variable(index=[time, country], unit="\$/person")
    d_percap_slr = Variable(index=[time, country], unit="\$/person")
    isat_per_cap_SLRImpactperCapinclSaturationandAdaptation = Variable(index=[time, country], unit="\$/person")

    function run_timestep(p, v, d, t)
        ## Translate from country to region for mix-and-match
        for cc in d.country
            v.cons_percap_aftercosts[t, cc] = p.cons_percap_consumption[t, cc] - p.tct_per_cap_totalcostspercap[t, cc] - p.act_percap_adaptationcosts[t, cc]

            if (v.cons_percap_aftercosts[t, cc] < 1.)
                v.cons_percap_aftercosts[t, cc] = 1.
            end

            v.gdp_percap_aftercosts[t, cc] = v.cons_percap_aftercosts[t, cc] / (1 - p.save_savingsrate / 100)
        end

        v.cons_percap_aftercosts_regional[t, :] = countrytoregion(p.model, weighted_mean, v.cons_percap_aftercosts[t, :], p.pop_population[t, :])
        v.gdp_percap_aftercosts_regional[t, :] = countrytoregion(p.model, weighted_mean, v.gdp_percap_aftercosts[t, :], p.pop_population[t, :])

        for r in d.region
            if (p.s_sealevel[t] - p.atl_adjustedtolerablelevelofsealevelrise[t,r]) < 0
                v.i_regionalimpactSLR[t,r] = 0
            else
                v.i_regionalimpactSLR[t,r] = p.s_sealevel[t] - p.atl_adjustedtolerablelevelofsealevelrise[t,r]
            end

            v.iref_ImpactatReferenceGDPperCapSLR[t,r] = p.wincf_weightsfactor_sea[r] * ((p.W_SatCalibrationSLR + p.iben_SLRInitialBenefit * p.scal_calibrationSLR) *
                (v.i_regionalimpactSLR[t,r] / p.scal_calibrationSLR)^p.pow_SLRImpactFxnExponent - v.i_regionalimpactSLR[t,r] * p.iben_SLRInitialBenefit)

            v.igdp_ImpactatActualGDPperCapSLR[t,r] = v.iref_ImpactatReferenceGDPperCapSLR[t,r] *
                    (v.gdp_percap_aftercosts_regional[t,r] / p.GDP_per_cap_focus_0_FocusRegionEU)^p.ipow_SLRIncomeFxnExponent

            if v.igdp_ImpactatActualGDPperCapSLR[t,r] < p.isatg_impactfxnsaturation
                v.isat_ImpactinclSaturationandAdaptationSLR[t,r] = v.igdp_ImpactatActualGDPperCapSLR[t,r]
            else
                v.isat_ImpactinclSaturationandAdaptationSLR[t,r] = p.isatg_impactfxnsaturation +
                    ((100 - p.save_savingsrate) - p.isatg_impactfxnsaturation) *
                    ((v.igdp_ImpactatActualGDPperCapSLR[t,r] - p.isatg_impactfxnsaturation) /
                    (((100 - p.save_savingsrate) - p.isatg_impactfxnsaturation) +
                    (v.igdp_ImpactatActualGDPperCapSLR[t,r] - p.isatg_impactfxnsaturation)))
            end
            if v.i_regionalimpactSLR[t,r] < p.impmax_maxSLRforadaptpolicySLR[r]
                v.isat_ImpactinclSaturationandAdaptationSLR[t,r] = v.isat_ImpactinclSaturationandAdaptationSLR[t,r] * (1 - p.imp_actualreductionSLR[t,r] / 100)
            else
                v.isat_ImpactinclSaturationandAdaptationSLR[t,r] = v.isat_ImpactinclSaturationandAdaptationSLR[t,r] * (1 - (p.imp_actualreductionSLR[t,r] / 100) * p.impmax_maxSLRforadaptpolicySLR[r] /
                    v.i_regionalimpactSLR[t,r])
            end

            v.isat_per_cap_SLRImpactperCapinclSaturationandAdaptation_regional[t,r] = (v.isat_ImpactinclSaturationandAdaptationSLR[t,r] / 100) * v.gdp_percap_aftercosts_regional[t,r]
            v.rcons_per_cap_SLRRemainConsumption_regional[t,r] = max.(v.cons_percap_aftercosts_regional[t,r] - v.isat_per_cap_SLRImpactperCapinclSaturationandAdaptation_regional[t,r], 1.)
            v.rgdp_per_cap_SLRRemainGDP_regional[t,r] = v.rcons_per_cap_SLRRemainConsumption_regional[t,r] / (1 - p.save_savingsrate / 100)

        end

        ## Translate from region to country for mix-and-match
        v.isat_per_cap_SLRImpactperCapinclSaturationandAdaptation[t, :] = regiontocountry(p.model, v.isat_per_cap_SLRImpactperCapinclSaturationandAdaptation_regional[t, :])
        v.d_percap_slr[t, :] = v.isat_per_cap_SLRImpactperCapinclSaturationandAdaptation[t, :]
        v.rcons_per_cap_SLRRemainConsumption[t, :] = v.cons_percap_aftercosts[t, :] - v.isat_per_cap_SLRImpactperCapinclSaturationandAdaptation[t, :]
        v.rgdp_per_cap_SLRRemainGDP[t, :] = v.rcons_per_cap_SLRRemainConsumption[t, :] / (1 - p.save_savingsrate / 100)
    end
end


# Still need this function in order to set the parameters than depend on
# readpagedata, which takes model as an input. These cannot be set using
# the default keyword arg for now.
function addslrdamages_regional(model::Model)
    SLRDamagescomp = add_comp!(model, SLRDamages_regional, :SLRDamages)

    SLRDamagescomp[:model] = model
    SLRDamagescomp[:sealevelcost_draw] = 0
    SLRDamagescomp[:wincf_weightsfactor_sea] = readpagedata(model, "data/wincf_weightsfactor_sea.csv")
    SLRDamagescomp[:impmax_maxSLRforadaptpolicySLR] = readpagedata(model, "data/regional/impmax_sealevel.csv")

    return SLRDamagescomp
end
