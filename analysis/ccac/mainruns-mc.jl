using Random
import Mimi.add_save!
include("../../src/main_model.jl")
include("../../src/mcs.jl")

output_path = "mainruns-mc"

for name in ["Baseline", "LTCAction", "IntegratedAction", "IntegratedAction_delay"]
    println(name)
    scenario_emt = (name == "Baseline" ? "Baseline" :
                    (name == "LTCAction" ? "Long-term Climate Action" :
                     (name == "IntegratedAction" ? "Integrated Action" : "Delayed Action")))
    rcp = (scenario_emt == "Baseline" ? "RCP4.5" : "RCP2.6")

    model = getpage("$(rcp) & SSP2"; pm25_scenario=scenario_emt, config_capital="full", pm25_useekc=true, pm25_useext=false, pm25_gainsmatch=true, emissionfeedback=true, use_delays=name == "IntegratedAction_delay", vsl_calib=:oecd_global, use_gains_ghg=true, use_seaice=false, use_permafrost=false)

    run(model)

    Random.seed!(20260306)

    mcs = getsim(model, 1000);
    add_save!(mcs, (:GDP, :gdp));
    add_save!(mcs, (:RegionTemperature, :rtl_realizedtemperature_absolute));
    add_save!(mcs, (:PM25Pollution, :pm_total));
    add_save!(mcs, (:Capital, :gdp_capital));
    add_save!(mcs, (:SLRDamages, :d_slr));
    add_save!(mcs, (:MarketDamagesBurke, :isat_per_cap_ImpactperCapinclSaturationandAdaptation));
    add_save!(mcs, (:NonMarketDamages, :isat_per_cap_ImpactperCapinclSaturationandAdaptation));
    add_save!(mcs, (:CromarMortality, :excess_death_rate));
    add_save!(mcs, (:CromarMortality, :mortality_costs));
    add_save!(mcs, (:DasguptaLabor, :loglabor));
    add_save!(mcs, (:DasguptaLabor, :damages));
    add_save!(mcs, (:AbatementCostsCO2, :tc_totalcost_national));
    add_save!(mcs, (:TotalAbatementCosts, :tct_percap_totalcostspercap));
    add_save!(mcs, (:TotalAdaptationCosts, :act_percap_adaptationcosts));
    add_save!(mcs, (:Discontinuity, :isat_per_cap_DiscImpactperCapinclSaturation));
    add_save!(mcs, (:PMMarketDamages, :totalchange));
    add_save!(mcs, (:MarketDamageAQ_AsthmaERVisits, :total_market_damage));
    add_save!(mcs, (:MarketDamageAQ_CropLoss, :total_market_damage));
    add_save!(mcs, (:MarketDamageAQ_LostWorkHours, :total_market_damage));
    add_save!(mcs, (:MarketDamageAQ_RespiratoryAdmissions, :total_market_damage));
    add_save!(mcs, (:WBRegionCorrection, :morb_healthcare_new));
    add_save!(mcs, (:WBRegionCorrection, :morb_productivity_new));
    add_save!(mcs, (:WBRegionCorrection, :morb_disutility_new));
    add_save!(mcs, (:WBRegionCorrection, :mort_productivity_new));
    add_save!(mcs, (:WBRegionCorrection, :mort_disutility_new));
    add_save!(mcs, (:WBRegionCorrection, :infrastructure_cost));
    add_save!(mcs, (:LaborProductivity, :outcome));

    res = run(mcs, model, 1000; trials_output_filename=joinpath(output_path, name, "trialdata.csv"),
              results_output_dir=joinpath(output_path, name))
end
