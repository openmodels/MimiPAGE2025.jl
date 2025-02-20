using DataFrames
using Statistics
using LinearAlgebra
using GLM

function calc_domar_change(TT, labels, isos, dimpact)
    # Match up known impacts to IO countries
    replace!(isos, "SDN" => "SUD")
    replace!(isos, "PSX" => "PSE")
    labels2 = leftjoin(labels, DataFrame(V1=isos, dimpact=dimpact), on=:V1)
    labels2[labels2.V1 .== "ANT", :dimpact] .= labels2[labels2.V1 .== "NLD", :dimpact]

    # Calculate Domar weights
    total_sales = vec(sum(TT, dims=1)) + labels2.FD
    labels2[!, :gdp] = labels2.FD + labels2.VA
    global_gdp = sum(labels2.gdp)
    weights = total_sales / global_gdp

    # Calculate global GDP loss
    dimpact_level = exp.(labels2.dimpact) .- 1
    total_change = sum(weights .* replace(dimpact_level, missing => 0))

    # Extract out the additional
    total_trade_effect = total_change - sum(replace(dimpact_level .* labels2.gdp, missing => 0)) / global_gdp

    return total_trade_effect
end

function calc_domar_distribute_method1(TT, labels, isos, dimpact)
    domar_change = calc_domar_change(TT, labels, isos, dimpact)

    # Distribute domar loss
    dirimpacts = DataFrame(ISO=isos, dimpact=dimpact)
    totimpacts = DataFrame()

    for iso in isos
        comtrade_iso = comtrade[comtrade.ReporterISO .== iso .&& comtrade.PartnerISO .!= "W00", :]
        if nrow(comtrade_iso) == 0
            continue
        end

        results2_iso = results2[results2.ISO .== iso, :]

        maxgrow = max(0, dimpact[findfirst(isequal.(isos), iso)])

        calcdf = leftjoin(comtrade_iso, dirimpacts, on=:PartnerISO => :ISO)

        # Limit any growth to the growth of the country
        calcdf[!, :cif_lost] = calcdf.Cifvalue .* max.(-calcdf.dimpact, -maxgrow)
        calcdf[!, :fob_lost] = calcdf.Fobvalue .* max.(-calcdf.dimpact, -maxgrow)

        # Fill in missing values with preference based on direction
        calcdf[calcdf.FlowDesc .== "Export", :fob_lost] .= coalesce.(
            calcdf[calcdf.FlowDesc .== "Export", :fob_lost],
            calcdf[calcdf.FlowDesc .== "Export", :cif_lost]
        )
        calcdf[calcdf.FlowDesc .== "Export", :Fobvalue] .= coalesce.(
            calcdf[calcdf.FlowDesc .== "Export", :fob_lost],
            calcdf[calcdf.FlowDesc .== "Export", :Cifvalue]
        )
        calcdf[calcdf.FlowDesc .== "Import", :cif_lost] .= coalesce.(
            calcdf[calcdf.FlowDesc .== "Import", :fob_lost],
            calcdf[calcdf.FlowDesc .== "Import", :cif_lost]
        )
        calcdf[calcdf.FlowDesc .== "Import", :Cifvalue] .= coalesce.(
            calcdf[calcdf.FlowDesc .== "Import", :fob_lost],
            calcdf[calcdf.FlowDesc .== "Import", :Fobvalue]
        )

        fracloss_import = sum(skipmissing(calcdf[calcdf.FlowDesc .== "Import", :cif_lost])) /
                        sum(skipmissing(calcdf[calcdf.FlowDesc .== "Import", :Cifvalue]))
        fracloss_export = sum(skipmissing(calcdf[calcdf.FlowDesc .== "Export", :fob_lost])) /
                        sum(skipmissing(calcdf[calcdf.FlowDesc .== "Export", :Fobvalue]))

        append!(totimpacts, DataFrame(ISO=iso, fracloss_import=fracloss_import, fracloss_export=fracloss_export))
    end

    totimpacts2 = leftjoin(totimpacts, df_gdp3, on="ISO" => "Country Code")

    return (
        global=DataFrame(domar_change=domar_change,
                         global_gdp=sum(skipmissing(totimpacts2.GDP_2019_est)),
                         global_loss=sum(replace(totimpacts2.fracloss_export, missing => 0) .* totimpacts2.GDP_2019_est)),
        totimpacts2=totimpacts2
    )
end

function calc_domar_distribute_method2(scaleby, isos, totimpacts2)
    totimpacts2[!, :tradeloss] = totimpacts2.fracloss_export .* scaleby

    domar_loss2 = leftjoin(DataFrame(ISO=isos), totimpacts2)
    return domar_loss2.tradeloss
end
