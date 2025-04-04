function buildpage(m::Model, scenario::String, use_permafrost::Bool=true, use_seaice::Bool=true; use_rffsp::Bool=false,
                   config_marketdmg::String="adaptive", config_nonmarketdmg::String="national", config_slrdmg::String="national",
                   config_discontinuity::String="default",
                   config_abatement::String="national", config_downscaling::String="mcpr", use_subnational::Bool=true,
                   use_trade::Bool=true)
    # add all the components
    scenario = addrcpsspscenario(m, scenario)
    if use_rffsp
        socioscenario = addrffspscenario(m)
        socioscenario[:grw_gdpgrowthrate_rcp] = scenario[:grw_gdpgrowthrate]
        socioscenario[:er_CO2emissionsgrowth_rcp] = scenario[:er_CO2emissionsgrowth]
        socioscenario[:er_CH4emissionsgrowth_rcp] = scenario[:er_CH4emissionsgrowth]
        socioscenario[:er_N2Oemissionsgrowth_rcp] = scenario[:er_N2Oemissionsgrowth]
        socioscenario[:er_LGemissionsgrowth_rcp] = scenario[:er_LGemissionsgrowth]
        socioscenario[:pse_sulphatevsbase_rcp] = scenario[:pse_sulphatevsbase]
        socioscenario_comp = :RFFSPScenario
    else
        socioscenario = scenario
        socioscenario_comp = :RCPSSPScenario
    end
    carbonpriceinfer = addcarbonpriceinfer(m)

    # Socio-Economics
    population = addpopulation(m)
    gdp = addgdp(m)

    gdp[:pop0_initpopulation_region] = population[:pop0_initpopulation_region]

    if config_abatement == "national"
        abateco2 = addabatementcostsco2(m)
        abateco2[:e0_baselineCO2emissions_country] = carbonpriceinfer[:e0_baselineCO2emissions_country]
        abateco2[:carbonprice] = carbonpriceinfer[:carbonprice]
        abateco2[:gdp] = gdp[:gdp]
    end

    if config_downscaling == "mcpr"
        glotemp = addglobaltemperature(m, use_seaice)
        regtemp = addregiontemperature(m)

        regtemp_comp = :RegionTemperature
        glotemp_comp = :GlobalTemperature
        regtemp[:rt_g_globaltemperature] = glotemp[:rt_g_globaltemperature]
    elseif config_downscaling == "pageice"
        climtemp = addclimatetemperature_pageice(m, use_seaice)
        glotemp = climtemp
        regtemp = climtemp
        regtemp_comp = :GlobalTemperature
        glotemp_comp = :GlobalTemperature
    else
        throw(ArgumentError("Unknown downscaling configuration: $config_downscaling"))
    end
    if use_permafrost
        permafrost_sibcasa = add_comp!(m, PermafrostSiBCASA)
        permafrost_jules = add_comp!(m, PermafrostJULES)
        permafrost = add_comp!(m, PermafrostTotal)
    end
    if config_abatement == "national"
        co2emit = add_comp!(m, co2emissions)
    elseif config_abatement == "pageice"
        co2emit = add_comp!(m, co2emissions_regional, :co2emissions)
    else
        throw(ArgumentError("Unknown abatement configuration: $config_abatement"))
    end
    co2cycle = addco2cycle(m, use_permafrost)
    add_comp!(m, co2forcing)
    ch4emit = add_comp!(m, ch4emissions)
    ch4cycle = addch4cycle(m, use_permafrost)
    add_comp!(m, ch4forcing)
    n2oemit = add_comp!(m, n2oemissions)
    add_comp!(m, n2ocycle)
    add_comp!(m, n2oforcing)
    lgemit = add_comp!(m, LGemissions)
    add_comp!(m, LGcycle)
    add_comp!(m, LGforcing)
    sulfemit = add_comp!(m, SulphateForcing)
    totalforcing = add_comp!(m, TotalForcing)
    add_comp!(m, SeaLevelRise)

    # Abatement Costs
    allabatement_comps = [
        (:AbatementCostParametersCH4, :AbatementCostsCH4, :er_CH4emissionsgrowth),
        (:AbatementCostParametersN2O, :AbatementCostsN2O, :er_N2Oemissionsgrowth),
        (:AbatementCostParametersLin, :AbatementCostsLin, :er_LGemissionsgrowth)]
    if config_abatement == "pageice"
        addabatementcostparameters(m, :CO2)
        push!(allabatement_comps, (:AbatementCostParametersCO2, :AbatementCostsCO2, :er_CO2emissionsgrowth))
    end
    addabatementcostparameters(m, :CH4)
    addabatementcostparameters(m, :N2O)
    addabatementcostparameters(m, :Lin)

    set_param!(m, :q0propmult_cutbacksatnegativecostinfinalyear, 0.8833333333333333)
    set_param!(m, :qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear, 1.1166666666666666)
    set_param!(m, :c0mult_mostnegativecostinfinalyear, 0.9333333333333334)
    set_param!(m, :curve_below_curvatureofMACcurvebelowzerocost, .5)
    set_param!(m, :curve_above_curvatureofMACcurveabovezerocost, .4)
    set_param!(m, :cross_experiencecrossoverratio, .2)
    set_param!(m, :learn_learningrate, .2)
    set_param!(m, :equity_prop_equityweightsproportion, 1)
    set_param!(m, :y_year_0, 2015)

    set_param!(m, :automult_autonomoustechchange, .65)

    if config_abatement == "pageice"
        addabatementcosts(m, :CO2)
    end
    addabatementcosts(m, :CH4)
    addabatementcosts(m, :N2O)
    addabatementcosts(m, :Lin)
    addtotalabatementcosts(m)

    # Adaptation Costs
    if config_slrdmg in ["none", "national"]
        adaptationcosts_sealevel = addadaptationcosts_sealevel(m; config_slrdmg)
    elseif config_slrdmg == "pageice"
        adaptationcosts_sealevel = addadaptationcosts_sealevel_regional(m)
    end
    adaptationcosts_economic = addadaptationcosts_economic(m; config_marketdmg)
    adaptationcosts_noneconomic = addadaptationcosts_noneconomic(m; config_nonmarketdmg)

    totaladaptcost = addtotaladaptationcosts(m)

    # Impacts
    if config_slrdmg in ["none", "national"]
        slrdamages = addslrdamages(m, config_slrdmg)
    elseif config_slrdmg == "pageice"
        slrdamages = addslrdamages_regional(m)
    else
        throw(ArgumentError("Unknown SLR damages configuration: $config_slrdmg"))
    end
    if config_marketdmg in ["none", "nooffset", "constoffset", "adaptive"]
        marketdamagesburke = addmarketdamagesburke(m, config_marketdmg)
    elseif config_marketdmg == "pageice"
        marketdamagesburke = addmarketdamagesburke_regional(m)
    else
        throw(ArgumentError("Unknown Market damages configuration: $config_marketdmg"))
    end
    if use_trade
        trade = addtrade(m)
    end
    if config_nonmarketdmg in ["none", "pinational", "national"]
        nonmarketdamages = addnonmarketdamages(m; config_nonmarketdmg)
    elseif config_nonmarketdmg == "pageice"
        nonmarketdamages = addnonmarketdamages_regional(m)
    else
        throw(ArgumentError("Unknown Non-market damages configuration: $config_nonmarketdmg"))
    end
    discontinuity = adddiscontinuity(m; config_discontinuity)

    # Total costs component
    add_comp!(m, TotalCosts)

    # Equity weighting and Total Costs
    countrylevelnpv = addcountrylevelnpv(m, use_subnational)
    equityweighting = addequityweighting(m, use_subnational)

    # connect parameters together
    connect_param!(m, glotemp_comp => :fant_anthroforcing, :TotalForcing => :fant_anthroforcing)

    if use_permafrost
        permafrost_sibcasa[:rt_g] = glotemp[:rt_g_globaltemperature]
        permafrost_jules[:rt_g] = glotemp[:rt_g_globaltemperature]
        permafrost[:perm_sib_ce_co2] = permafrost_sibcasa[:perm_sib_ce_co2]
        permafrost[:perm_sib_e_co2] = permafrost_sibcasa[:perm_sib_e_co2]
        permafrost[:perm_sib_ce_ch4] = permafrost_sibcasa[:perm_sib_ce_ch4]
        permafrost[:perm_jul_ce_co2] = permafrost_jules[:perm_jul_ce_co2]
        permafrost[:perm_jul_e_co2] = permafrost_jules[:perm_jul_e_co2]
        permafrost[:perm_jul_ce_ch4] = permafrost_jules[:perm_jul_ce_ch4]
    end

    carbonpriceinfer[:er_CO2emissionsgrowth] = socioscenario[:er_CO2emissionsgrowth]

    if config_abatement == "national"
        co2emit[:baselineemit] = abateco2[:baselineemit]
        co2emit[:fracabatedcarbon] = abateco2[:fracabatedcarbon]
    elseif config_abatement == "pageice"
        co2emit[:er_CO2emissionsgrowth] = scenario[:er_CO2emissionsgrowth]
    end

    connect_param!(m, :CO2Cycle => :e_globalCO2emissions, :co2emissions => :e_globalCO2emissions)
    connect_param!(m, :CO2Cycle => :rt_g_globaltemperature, glotemp_comp => :rt_g_globaltemperature)
    if use_permafrost
        co2cycle[:permte_permafrostemissions] = permafrost[:perm_tot_e_co2]
    end

    connect_param!(m, :co2forcing => :c_CO2concentration, :CO2Cycle => :c_CO2concentration)

    ch4emit[:er_CH4emissionsgrowth] = scenario[:er_CH4emissionsgrowth]

    connect_param!(m, :CH4Cycle => :e_globalCH4emissions, :ch4emissions => :e_globalCH4emissions)
    connect_param!(m, :CH4Cycle => :rtl_g0_baselandtemp, regtemp_comp => :rtl_g0_baselandtemp)
    connect_param!(m, :CH4Cycle => :rtl_g_landtemperature, regtemp_comp => :rtl_g_landtemperature)
    if use_permafrost
        ch4cycle[:permtce_permafrostemissions] = permafrost[:perm_tot_ce_ch4]
    end

    connect_param!(m, :ch4forcing => :c_CH4concentration, :CH4Cycle => :c_CH4concentration)
    connect_param!(m, :ch4forcing => :c_N2Oconcentration, :n2ocycle => :c_N2Oconcentration)

    n2oemit[:er_N2Oemissionsgrowth] = scenario[:er_N2Oemissionsgrowth]

    connect_param!(m, :n2ocycle => :e_globalN2Oemissions, :n2oemissions => :e_globalN2Oemissions)
    connect_param!(m, :n2ocycle => :rtl_g0_baselandtemp, regtemp_comp => :rtl_g0_baselandtemp)
    connect_param!(m, :n2ocycle => :rtl_g_landtemperature, regtemp_comp => :rtl_g_landtemperature)

    connect_param!(m, :n2oforcing => :c_CH4concentration, :CH4Cycle => :c_CH4concentration)
    connect_param!(m, :n2oforcing => :c_N2Oconcentration, :n2ocycle => :c_N2Oconcentration)

    lgemit[:er_LGemissionsgrowth] = scenario[:er_LGemissionsgrowth]

    connect_param!(m, :LGcycle => :e_globalLGemissions, :LGemissions => :e_globalLGemissions)
    connect_param!(m, :LGcycle => :rtl_g0_baselandtemp, regtemp_comp => :rtl_g0_baselandtemp)
    connect_param!(m, :LGcycle => :rtl_g_landtemperature, regtemp_comp => :rtl_g_landtemperature)

    connect_param!(m, :LGforcing => :c_LGconcentration, :LGcycle => :c_LGconcentration)

    sulfemit[:pse_sulphatevsbase] = scenario[:pse_sulphatevsbase]
    sulfemit[:area_region] = regtemp[:area_region]

    connect_param!(m, :TotalForcing => :f_CO2forcing, :co2forcing => :f_CO2forcing)
    connect_param!(m, :TotalForcing => :f_CH4forcing, :ch4forcing => :f_CH4forcing)
    connect_param!(m, :TotalForcing => :f_N2Oforcing, :n2oforcing => :f_N2Oforcing)
    connect_param!(m, :TotalForcing => :f_lineargasforcing, :LGforcing => :f_LGforcing)
    totalforcing[:exf_excessforcing] = scenario[:exf_excessforcing]
    connect_param!(m, :TotalForcing => :fs_sulfateforcing, :SulphateForcing => :fs_sulphateforcing)
    totalforcing[:area_region] = regtemp[:area_region]

    connect_param!(m, :SeaLevelRise => :rt_g_globaltemperature, glotemp_comp => :rt_g_globaltemperature)

    population[:popgrw_populationgrowth] = socioscenario[:popgrw_populationgrowth]

    connect_param!(m, :GDP => :pop_population, :Population => :pop_population)
    connect_param!(m, :GDP => :pop_population_region, :Population => :pop_population_region)
    gdp[:grw_gdpgrowthrate] = socioscenario[:grw_gdpgrowthrate]

    for (abatementcostparameters, abatementcosts, er_parameter) in allabatement_comps
        connect_param!(m, abatementcostparameters => :yagg, :GDP => :yagg_periodspan)
        connect_param!(m, abatementcostparameters => :cbe_absoluteemissionreductions, abatementcosts => :cbe_absoluteemissionreductions)

        connect_param!(m, abatementcosts => :zc_zerocostemissions, abatementcostparameters => :zc_zerocostemissions)
        connect_param!(m, abatementcosts => :q0_absolutecutbacksatnegativecost, abatementcostparameters => :q0_absolutecutbacksatnegativecost)
        connect_param!(m, abatementcosts => :blo, abatementcostparameters => :blo)
        connect_param!(m, abatementcosts => :alo, abatementcostparameters => :alo)
        connect_param!(m, abatementcosts => :bhi, abatementcostparameters => :bhi)
        connect_param!(m, abatementcosts => :ahi, abatementcostparameters => :ahi)
        connect_param!(m, abatementcosts => :er_emissionsgrowth, socioscenario_comp => er_parameter)

        # For mix-and-match
        connect_param!(m, abatementcosts => :gdp, :GDP => :gdp_region)
        connect_param!(m, abatementcosts => :gdp_national, :GDP => :gdp)

    end

    connect_param!(m, :TotalAbatementCosts => :tc_totalcosts_co2, :AbatementCostsCO2 => :tc_totalcost_national)
    connect_param!(m, :TotalAbatementCosts => :tc_totalcosts_n2o, :AbatementCostsN2O => :tc_totalcost)
    connect_param!(m, :TotalAbatementCosts => :tc_totalcosts_ch4, :AbatementCostsCH4 => :tc_totalcost)
    connect_param!(m, :TotalAbatementCosts => :tc_totalcosts_linear, :AbatementCostsLin => :tc_totalcost)
    connect_param!(m, :TotalAbatementCosts => :pop_population, :Population => :pop_population)
    connect_param!(m, :TotalAbatementCosts => :pop_population_region, :Population => :pop_population_region)

    connect_param!(m, :AdaptiveCostsEconomic => :gdp, :GDP => :gdp_region)
    connect_param!(m, :AdaptiveCostsEconomic => :gdp_national, :GDP => :gdp)
    connect_param!(m, :AdaptiveCostsNonEconomic => :gdp, :GDP => :gdp_region)
    connect_param!(m, :AdaptiveCostsNonEconomic => :gdp_national, :GDP => :gdp)
    if config_slrdmg in ["none", "national"]
        connect_param!(m, :AdaptiveCostsSeaLevel => :gdp, :GDP => :gdp)
        connect_param!(m, :AdaptiveCostsSeaLevel => :s_sealevel, :SeaLevelRise => :s_sealevel)
    elseif config_slrdmg == "pageice"
        connect_param!(m, :AdaptiveCostsSeaLevel => :gdp, :GDP => :gdp_region)
        connect_param!(m, :AdaptiveCostsSeaLevel => :gdp_national, :GDP => :gdp)
    end

    connect_param!(m, :TotalAdaptationCosts => :ac_adaptationcosts_economic, :AdaptiveCostsEconomic => :ac_adaptivecosts)
    connect_param!(m, :TotalAdaptationCosts => :ac_adaptationcosts_noneconomic, :AdaptiveCostsNonEconomic => :ac_adaptivecosts)
    if config_slrdmg in ["none", "national"]
        connect_param!(m, :TotalAdaptationCosts => :ac_adaptationcosts_sealevelrise, :AdaptiveCostsSeaLevel => :ac_adaptivecosts)
    elseif config_slrdmg == "pageice"
        connect_param!(m, :TotalAdaptationCosts => :ac_adaptationcosts_sealevelrise, :AdaptiveCostsSeaLevel => :ac_adaptivecosts_national)
    end
    connect_param!(m, :TotalAdaptationCosts => :pop_population, :Population => :pop_population)
    connect_param!(m, :TotalAdaptationCosts => :pop_population_region, :Population => :pop_population_region)

    connect_param!(m, :SLRDamages => :gdp, :GDP => :gdp)
    connect_param!(m, :SLRDamages => :pop_population, :Population => :pop_population)
    connect_param!(m, :SLRDamages => :s_sealevel, :SeaLevelRise => :s_sealevel)
    connect_param!(m, :SLRDamages => :cons_percap_consumption, :GDP => :cons_percap_consumption)
    connect_param!(m, :SLRDamages => :cons_percap_consumption_0, :GDP => :cons_percap_consumption_0)
    connect_param!(m, :SLRDamages => :tct_per_cap_totalcostspercap, :TotalAbatementCosts => :tct_percap_totalcostspercap)
    connect_param!(m, :SLRDamages => :act_percap_adaptationcosts, :TotalAdaptationCosts => :act_percap_adaptationcosts)
    if config_slrdmg == "pageice"
        connect_param!(m, :SLRDamages => :atl_adjustedtolerablelevelofsealevelrise, :AdaptiveCostsSeaLevel => :atl_adjustedtolerablelevel, ignoreunits=true) # For mix-and-match
        connect_param!(m, :SLRDamages => :imp_actualreductionSLR, :AdaptiveCostsSeaLevel => :imp_adaptedimpacts) # For mix-and-match
        connect_param!(m, :SLRDamages => :isatg_impactfxnsaturation, :GDP => :isatg_impactfxnsaturation) # For mix-and-match
    end

    connect_param!(m, :MarketDamagesBurke => :rtl_realizedtemperature_absolute, regtemp_comp => :rtl_realizedtemperature_absolute)
    connect_param!(m, :MarketDamagesBurke => :rgdp_per_cap_SLRRemainGDP, :SLRDamages => :rgdp_per_cap_SLRRemainGDP)
    connect_param!(m, :MarketDamagesBurke => :rcons_per_cap_SLRRemainConsumption, :SLRDamages => :rcons_per_cap_SLRRemainConsumption)
    connect_param!(m, :MarketDamagesBurke => :isatg_impactfxnsaturation, :GDP => :isatg_impactfxnsaturation)
    connect_param!(m, :MarketDamagesBurke => :gdp, :GDP => :gdp)
    connect_param!(m, :MarketDamagesBurke => :pop_population, :Population => :pop_population)

    if use_trade
        trade[:gdp_baseline] = gdp[:gdp]
        connect_param!(m, :Trade => :gdp, :GDP => :gdp)
        trade[:pop_population] = population[:pop_population]
        trade[:rgdp_percap_impacts] = marketdamagesburke[:rgdp_per_cap_MarketRemainGDP]
    end

    connect_param!(m, :NonMarketDamages => :rtl_realizedtemperature_change, regtemp_comp => :rtl_realizedtemperature_change)
    connect_param!(m, :NonMarketDamages => :rtl_g_landtemperature, regtemp_comp => :rtl_g_landtemperature)
    connect_param!(m, :NonMarketDamages => :rt_g_globaltemperature, glotemp_comp => :rt_g_globaltemperature)
    if use_trade
        connect_param!(m, :NonMarketDamages => :rgdp_per_cap_MarketRemainGDP, :Trade => :rgdp_per_cap_TradeRemainGDP)
        connect_param!(m, :NonMarketDamages => :rcons_per_cap_MarketRemainConsumption, :Trade => :rcons_per_cap_TradeRemainConsumption)
    else
        connect_param!(m, :NonMarketDamages => :rgdp_per_cap_MarketRemainGDP, :MarketDamagesBurke => :rgdp_per_cap_MarketRemainGDP)
        connect_param!(m, :NonMarketDamages => :rcons_per_cap_MarketRemainConsumption, :MarketDamagesBurke => :rcons_per_cap_MarketRemainConsumption)
    end
    connect_param!(m, :NonMarketDamages => :atl_adjustedtolerableleveloftemprise, :AdaptiveCostsNonEconomic => :atl_adjustedtolerablelevel, ignoreunits=true)
    connect_param!(m, :NonMarketDamages => :imp_actualreduction, :AdaptiveCostsNonEconomic => :imp_adaptedimpacts)
    connect_param!(m, :NonMarketDamages => :isatg_impactfxnsaturation, :GDP => :isatg_impactfxnsaturation)
    if config_nonmarketdmg == "pageice"
        connect_param!(m, :NonMarketDamages => :pop_population, :Population => :pop_population)
    end

    connect_param!(m, :Discontinuity => :rt_g_globaltemperature, glotemp_comp => :rt_g_globaltemperature)
    connect_param!(m, :Discontinuity => :rgdp_per_cap_NonMarketRemainGDP, :NonMarketDamages => :rgdp_per_cap_NonMarketRemainGDP)
    connect_param!(m, :Discontinuity => :rcons_per_cap_NonMarketRemainConsumption, :NonMarketDamages => :rcons_per_cap_NonMarketRemainConsumption)
    connect_param!(m, :Discontinuity => :isatg_saturationmodification, :GDP => :isatg_impactfxnsaturation)

    connect_param!(m, :TotalCosts => :population, :Population => :pop_population)
    connect_param!(m, :TotalCosts => :period_length, :GDP => :yagg_periodspan)
    connect_param!(m, :TotalCosts => :abatement_costs_percap_peryear, :TotalAbatementCosts => :tct_percap_totalcostspercap)
    connect_param!(m, :TotalCosts => :adaptation_costs_percap_peryear, :TotalAdaptationCosts => :act_percap_adaptationcosts)
    connect_param!(m, :TotalCosts => :slr_damages_percap_peryear, :SLRDamages => :d_percap_slr)
    connect_param!(m, :TotalCosts => :market_damages_percap_peryear, :MarketDamagesBurke => :isat_per_cap_ImpactperCapinclSaturationandAdaptation)
    connect_param!(m, :TotalCosts => :non_market_damages_percap_peryear, :NonMarketDamages => :isat_per_cap_ImpactperCapinclSaturationandAdaptation)
    connect_param!(m, :TotalCosts => :discontinuity_damages_percap_peryear, :Discontinuity => :isat_per_cap_DiscImpactperCapinclSaturation)

    connect_param!(m, :CountryLevelNPV => :pop_population, :Population => :pop_population)
    connect_param!(m, :CountryLevelNPV => :tct_percap_totalcosts_total, :TotalAbatementCosts => :tct_percap_totalcostspercap)
    connect_param!(m, :CountryLevelNPV => :act_percap_adaptationcosts, :TotalAdaptationCosts => :act_percap_adaptationcosts)
    connect_param!(m, :CountryLevelNPV => :cons_percap_consumption, :GDP => :cons_percap_consumption)
    connect_param!(m, :CountryLevelNPV => :cons_percap_consumption_0, :GDP => :cons_percap_consumption_0)
    connect_param!(m, :CountryLevelNPV => :cons_percap_aftercosts, :SLRDamages => :cons_percap_aftercosts)
    connect_param!(m, :CountryLevelNPV => :rcons_percap_dis, :Discontinuity => :rcons_per_cap_DiscRemainConsumption)
    connect_param!(m, :CountryLevelNPV => :yagg_periodspan, :GDP => :yagg_periodspan)
    countrylevelnpv[:grw_gdpgrowthrate] = socioscenario[:grw_gdpgrowthrate]
    countrylevelnpv[:popgrw_populationgrowth] = socioscenario[:popgrw_populationgrowth]

    connect_param!(m, :EquityWeighting => :pop_population, :Population => :pop_population)
    connect_param!(m, :EquityWeighting => :tct_percap_totalcosts_total, :TotalAbatementCosts => :tct_percap_totalcostspercap)
    connect_param!(m, :EquityWeighting => :act_percap_adaptationcosts, :TotalAdaptationCosts => :act_percap_adaptationcosts)
    connect_param!(m, :EquityWeighting => :cons_percap_consumption, :GDP => :cons_percap_consumption)
    connect_param!(m, :EquityWeighting => :cons_percap_consumption_0, :GDP => :cons_percap_consumption_0)
    connect_param!(m, :EquityWeighting => :cons_percap_aftercosts, :SLRDamages => :cons_percap_aftercosts)
    connect_param!(m, :EquityWeighting => :rcons_percap_dis, :Discontinuity => :rcons_per_cap_DiscRemainConsumption)
    connect_param!(m, :EquityWeighting => :yagg_periodspan, :GDP => :yagg_periodspan)
    equityweighting[:grw_gdpgrowthrate] = socioscenario[:grw_gdpgrowthrate]
    equityweighting[:popgrw_populationgrowth] = socioscenario[:popgrw_populationgrowth]

    return m
end

function initpage(m::Model)
    p = load_parameters(m)
    p["y_year_0"] = 2015.
    p["y_year"] = Mimi.dim_keys(m.md, :time)
    set_leftover_params!(m, p)
end

function getpage(scenario::String="RCP4.5 & SSP2", use_permafrost::Bool=true, use_seaice::Bool=true; use_rffsp::Bool=false,
                 config_marketdmg::String="adaptive", config_nonmarketdmg::String="national", config_slrdmg::String="national",
                 config_discontinuity::String="default",
                 config_abatement::String="national", config_downscaling::String="mcpr", use_subnational::Bool=true,
                 use_trade::Bool=true)
    m = Model()
    set_dimension!(m, :time, [2020, 2030, 2040, 2050, 2075, 2100, 2150, 2200, 2250, 2300])
    set_dimension!(m, :region, ["EU", "USA", "OECD","USSR","China","SEAsia","Africa","LatAmerica"])
    set_dimension!(m, :country, get_countryinfo().ISO3)

    buildpage(m, scenario, use_permafrost, use_seaice; use_rffsp=use_rffsp, config_marketdmg=config_marketdmg,
              config_nonmarketdmg=config_nonmarketdmg, config_slrdmg=config_slrdmg, config_discontinuity=config_discontinuity,
              config_abatement=config_abatement, config_downscaling=config_downscaling, use_subnational=use_subnational,
              use_trade=use_trade)

    # next: add vector and panel example
    initpage(m)

    return m
end

get_model = getpage
