include("../../src/main_model.jl")

model = getpage("RCP4.5 & SSP2"; pm25_scenario=Symbol("Baseline_CLE"),
                config_capital="full", use_seaice=false,
                use_permafrost=false,
                pm25_useekc=true, pm25_useext=false,
                pm25_gainsmatch=true, emissionfeedback=true,
                vsl_calib=:oecd_global, use_delays=false,
                use_gains_ch4=:Decarb_APC) # only difference
update_param!(model, :SLRDamages_saf_slradaptfrac, 0. * ones(dim_count(model, :time), dim_count(model, :country)))

run(model)

outdir = "mainruns"

    df = getdataframe(model, :Population, :pop_population)
    CSV.write("$(outdir)/pop-methaneonly.csv", df)

    df = getdataframe(model, :GDP, :gdp)
    CSV.write("$(outdir)/gdp-methaneonly.csv", df)

    df = getdataframe(model, :SeaLevelRise, :s_sealevel)
    CSV.write("$(outdir)/slr-methaneonly.csv", df)

    df = getdataframe(model, :FaIRGrounds, :rt_g_globaltemperature)
    CSV.write("$(outdir)/gsat-methaneonly.csv", df)

    df = getdataframe(model, :RegionTemperature, :rtl_realizedtemperature_absolute)
    CSV.write("$(outdir)/temp-methaneonly.csv", df)

df = getdataframe(model, :Capital, :gdp_capital)
CSV.write("$(outdir)/capital-gdp-methaneonly.csv", df)

    df = getdataframe(model, :SLRDamages, :d_slr)
    CSV.write("$(outdir)/d_slr-methaneonly.csv", df)

    df = getdataframe(model, :MarketDamagesBurke, :isat_per_cap_ImpactperCapinclSaturationandAdaptation)
    CSV.write("$(outdir)/d_market-methaneonly.csv", df)

    df = getdataframe(model, :NonMarketDamages, :isat_per_cap_ImpactperCapinclSaturationandAdaptation)
    CSV.write("$(outdir)/d_nonmarket-methaneonly.csv", df)

    df = getdataframe(model, :CromarMortality, :excess_death_rate)
    CSV.write("$(outdir)/cromar-mortality-edr-methaneonly.csv", df)

    df = getdataframe(model, :CromarMortality, :mortality_costs)
    CSV.write("$(outdir)/cromar-mortality-methaneonly.csv", df)

    df = getdataframe(model, :AbatementCostsCO2, :tc_totalcost_national)
    CSV.write("$(outdir)/abateco2-tc-methaneonly.csv", df)

    df = getdataframe(model, :TotalAbatementCosts, :tct_percap_totalcostspercap)
    CSV.write("$(outdir)/abateall-tcpc-methaneonly.csv", df)

    df = getdataframe(model, :TotalAdaptationCosts, :act_percap_adaptationcosts)
    CSV.write("$(outdir)/adaptall-acpc-methaneonly.csv", df)

    df = getdataframe(model, :Discontinuity, :isat_per_cap_DiscImpactperCapinclSaturation)
    CSV.write("$(outdir)/discontinuity-percap-methaneonly.csv", df)

    df = getdataframe(model, :PMMarketDamages, :totalchange)
    CSV.write("$(outdir)/pmmarket-percchg-methaneonly.csv", df)

    for comp in [:MarketDamageAQ_AsthmaERVisits, :MarketDamageAQ_CropLoss, :MarketDamageAQ_LostWorkHours, :MarketDamageAQ_RespiratoryAdmissions]
        df = getdataframe(model, comp, :total_market_damage)
        CSV.write("$(outdir)/$(comp)-methaneonly.csv", df)
    end

    for comp in [:PM25Damage_Healthcare, :PM25Damage_Productivity, :PM25Damage_Disutility, :PM25Damage_Mortality]
        df = getdataframe(model, comp, :cost)
        CSV.write("$(outdir)/$(comp)-methaneonly.csv", df)
    end

    for comp in [:LaborProductivity]
        df = getdataframe(model, comp, :outcome)
        CSV.write("$(outdir)/CIL-$(comp)-methaneonly.csv", df)
    end
