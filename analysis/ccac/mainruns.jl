include("../../src/main_model.jl")

model = getpage("RCP4.5 & SSP2"; pm25_scenario="Baseline", use_seaice=false, use_permafrost=false, pm25_useekc=true, pm25_useext=false, pm25_gainsmatch=true,
                vsl_calib=:oecd_global, use_gains_ghg=true, use_delays=false, config_capital="full", emissionfeedback=true)
run(model)

# model2 = getpage("RCP2.6 & SSP1"; pm25_scenario="Long-term Climate Action", use_seaice=false, use_permafrost=false, pm25_useekc=false, pm25_useext=true, pm25_gainsmatch=true,
#                 vsl_calib=:oecd_global, use_gains_ghg=true, use_delays=false, config_capital="full", emissionfeedback=true)
# run(model2)

# m = MimiFAIRv2.get_model(emissions_forcing_scenario="ssp126", start_year=1750, end_year=2100)
# run(m)
# m[:temperature, :T][dim_keys(m, :time) .>= 2015]

outdir = "mainruns"

runspecs = DataFrame(name=["Baseline", "LTCAction", "IntegratedAction",
                           "Baseline_SSP3", "LTCAction_SSP3", "IntegratedAction_SSP3",
                           "Baseline_modpm", "IntegratedAction_modpm",
                           "Baseline_noekc", "IntegratedAction_noekc",
                           "Baseline_nofeedback", "IntegratedAction_nofeedback",
                           "IntegratedAction_delay",
                           "Baseline_pageghg", "IntegratedAction_pageghg",
                           "Baseline_tippt", "IntegratedAction_tippt",
                           "IntegratedAction_ghhalfcost", "IntegratedAction_ghdblcost"],
                     scenario_emt=["Baseline", "Long-term Climate Action", "Integrated Action",
                                   "Baseline", "Long-term Climate Action", "Integrated Action",
                                   "Baseline", "Integrated Action",
                                   "Baseline", "Integrated Action",
                                   "Baseline", "Integrated Action",
                                   "Delayed Action",
                                   "Baseline", "Integrated Action",
                                   "Baseline", "Integrated Action",
                                   "Integrated Action", "Integrated Action"],
                     scenario_eco=["SSP2", "SSP2", "SSP2",
                                   "SSP3", "SSP3", "SSP3",
                                   "SSP2", "SSP2",
                                   "SSP2", "SSP2",
                                   "SSP2", "SSP2",
                                   "SSP2",
                                   "SSP2", "SSP2",
                                   "SSP2", "SSP2",
                                   "SSP2", "SSP2"],
                     pm25_gainsmatch=[true, true, true,
                                      true, true, true,
                                      false, false,
                                      true, true,
                                      true, true,
                                      true,
                                      true, true,
                                      true, true,
                                      false, false],
                     pm25_useext=[false, false, false,
                                  false, false, false,
                                  true, true,
                                  false, false,
                                  false, false,
                                  false,
                                  false, false,
                                  false, false,
                                  true, true],
                     pm25_useekc=[true, true, true,
                                  true, true, true,
                                  true, true,
                                  false, false,
                                  true, true,
                                  true,
                                  true, true,
                                  true, true,
                                  true, true],
                     use_capital=[true, true, true,
                                  true, true, true,
                                  true, true,
                                  true, true,
                                  false, false,
                                  true,
                                  true, true,
                                  true, true,
                                  true, true],
                     emissionfeedback=[true, true, true,
                                       true, true, true,
                                       true, true,
                                       true, true,
                                       false, false,
                                       true,
                                       true, true,
                                       true, true,
                                       true, true],
                     use_delays=[false, false, false,
                                 false, false, false,
                                 false, false,
                                 false, false,
                                 false, false,
                                 true,
                                 false, false,
                                 false, false,
                                 false, false],
                     gh_control_factor=[1., 1., 1.,
                                        1., 1., 1.,
                                        1., 1.,
                                        1., 1.,
                                        1., 1.,
                                        1.,
                                        1., 1.,
                                        1., 1.,
                                        0.5, 2.0],
                     use_pageghg=[false, false, false,
                                  false, false, false,
                                  false, false,
                                  false, false,
                                  false, false,
                                  false,
                                  true, true,
                                  false, false,
                                  false, false],
                     use_tippt=[false, false, false,
                                false, false, false,
                                false, false,
                                false, false,
                                false, false,
                                false,
                                false, false,
                                true, true,
                                false, false])

CSV.write("runspecs.csv", runspecs)

for ii in 1:nrow(runspecs)
    rcp = (runspecs.scenario_emt[ii] == "Baseline" ? "RCP4.5" : "RCP1.9")
    println(runspecs.name[ii])
    model = getpage("$(rcp) & $(runspecs.scenario_eco[ii])"; pm25_scenario=runspecs.scenario_emt[ii], config_capital=(runspecs.use_capital[ii] ? "full" : "inferred"), pm25_useekc=runspecs.pm25_useekc[ii], pm25_useext=runspecs.pm25_useext[ii], pm25_gainsmatch=runspecs.pm25_gainsmatch[ii], emissionfeedback=runspecs.emissionfeedback[ii], use_delays=runspecs.use_delays[ii], vsl_calib=:oecd_global, use_gains_ghg=!runspecs.use_pageghg[ii], use_seaice=runspecs.use_tippt[ii], use_permafrost=runspecs.use_tippt[ii])
    update_param!(model, :CarbonPriceInfer, :control_factor, runspecs.gh_control_factor[ii])
    update_param!(model, :SLRDamages_saf_slradaptfrac, 0. * ones(dim_count(model, :time), dim_count(model, :country)))

    run(model)

    df = getdataframe(model, :Population, :pop_population)
    CSV.write("$(outdir)/pop-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :GDP, :gdp)
    CSV.write("$(outdir)/gdp-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :SeaLevelRise, :s_sealevel)
    CSV.write("$(outdir)/slr-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :FaIRGrounds, :rt_g_globaltemperature)
    CSV.write("$(outdir)/gsat-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :RegionTemperature, :rtl_realizedtemperature_absolute)
    CSV.write("$(outdir)/temp-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :PM25Pollution, :pm_total)
    CSV.write("$(outdir)/pm-$(runspecs.name[ii]).csv", df)

    if runspecs.use_capital[ii]
        df = getdataframe(model, :Capital, :gdp_capital)
        CSV.write("$(outdir)/capital-gdp-$(runspecs.name[ii]).csv", df)
    end

    df = getdataframe(model, :co2emissions, :e_countryCO2emissions)
    CSV.write("$(outdir)/e_co2-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :SLRDamages, :d_slr)
    CSV.write("$(outdir)/d_slr-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :MarketDamagesBurke, :isat_per_cap_ImpactperCapinclSaturationandAdaptation)
    CSV.write("$(outdir)/d_market-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :NonMarketDamages, :isat_per_cap_ImpactperCapinclSaturationandAdaptation)
    CSV.write("$(outdir)/d_nonmarket-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :CromarMortality, :excess_death_rate)
    CSV.write("$(outdir)/cromar-mortality-edr-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :CromarMortality, :mortality_costs)
    CSV.write("$(outdir)/cromar-mortality-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :DasguptaLabor, :effective)
    CSV.write("$(outdir)/dasgupta-labor-prod-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :DasguptaLabor, :damages)
    CSV.write("$(outdir)/dasgupta-labor-dmg-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :AbatementCostsCO2, :tc_totalcost_national)
    CSV.write("$(outdir)/abateco2-tc-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :TotalAbatementCosts, :tct_percap_totalcostspercap)
    CSV.write("$(outdir)/abateall-tcpc-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :TotalAbatementCosts, :tct_totalcosts)
    CSV.write("$(outdir)/abateall-tct-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :TotalAdaptationCosts, :act_percap_adaptationcosts)
    CSV.write("$(outdir)/adaptall-acpc-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :Discontinuity, :isat_per_cap_DiscImpactperCapinclSaturation)
    CSV.write("$(outdir)/discontinuity-percap-$(runspecs.name[ii]).csv", df)

    df = getdataframe(model, :PMMarketDamages, :totalchange)
    CSV.write("$(outdir)/pmmarket-percchg-$(runspecs.name[ii]).csv", df)

    for comp in [:MarketDamageAQ_AsthmaERVisits, :MarketDamageAQ_CropLoss, :MarketDamageAQ_LostWorkHours, :MarketDamageAQ_RespiratoryAdmissions]
        df = getdataframe(model, comp, :total_market_damage)
        CSV.write("$(outdir)/$(comp)-$(runspecs.name[ii]).csv", df)
    end

    for var in [:morb_healthcare_new, :morb_productivity_new, :morb_disutility_new, :mort_productivity_new, :mort_disutility_new, :infrastructure_cost]
        df = getdataframe(model, :WBRegionCorrection, var)
        CSV.write("$(outdir)/WBRC-$(var)-$(runspecs.name[ii]).csv", df)
    end

    for comp in [:LaborProductivity]
        df = getdataframe(model, comp, :outcome)
        CSV.write("$(outdir)/CIL-$(comp)-$(runspecs.name[ii]).csv", df)
    end
end
