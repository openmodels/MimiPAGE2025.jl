using DataFrames
using Statistics
using LinearAlgebra
using GLM

trade_comtrade = CSV.read(pagedata("trade/uncomtrade.csv"), DataFrame)
trade_labels = CSV.read(pagedata("trade/labels.csv"), DataFrame)
TTdf = CSV.read(pagedata("trade/TT.csv"), DataFrame)
trade_TT = Matrix(TTdf[!, 2:ncol(TTdf)])

function calc_domar_change(isos::AbstractVector{<:AbstractString}, dimpact::Vector{Float64})
    # Match up known impacts to IO countries
    replace!(isos, "SDN" => "SUD")
    replace!(isos, "PSX" => "PSE")
    labels2 = leftjoin(trade_labels, DataFrame(ISO=isos, dimpact=dimpact), on=:ISO)
    labels2[labels2.ISO .== "ANT", :dimpact] .= labels2[labels2.ISO .== "NLD", :dimpact]

    # Calculate Domar weights
    total_sales = vec(sum(trade_TT, dims=1)) + labels2.FD
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

function calc_domar_distribute_method(isos::AbstractVector{<:AbstractString}, dimpact::Vector{Float64})
    domar_change = calc_domar_change(isos, dimpact)

    # Distribute domar loss
    dirimpacts = DataFrame(ISO=isos, dimpact=dimpact)
    totimpacts = DataFrame(ISO=AbstractString[], fracloss_import=Union{Float64, Missing}[],
                           fracloss_export=Union{Float64, Missing}[])

    for iso in isos
        comtrade_iso = trade_comtrade[trade_comtrade.reporterISO .== iso .&& trade_comtrade.partnerISO .!= "W00", :]
        if nrow(comtrade_iso) == 0
            append!(totimpacts, DataFrame(ISO=iso, fracloss_import=missing, fracloss_export=missing))
            continue
        end

        maxgrow = max(0, dimpact[findfirst(isequal.(isos, iso))])

        calcdf = leftjoin(comtrade_iso, dirimpacts, on=:partnerISO => :ISO)

        # Limit any growth to the growth of the country
        calcdf[!, :cif_lost] = Union{Missing, Float64}[calcdf.cifvalue .* max.(-calcdf.dimpact, -maxgrow)...]
        calcdf[!, :fob_lost] = Union{Missing, Float64}[calcdf.fobvalue .* max.(-calcdf.dimpact, -maxgrow)...]

        # Fill in missing values with preference based on direction
        calcdf[calcdf.flowDesc .== "Export", :fob_lost] .= coalesce.(
            calcdf[calcdf.flowDesc .== "Export", :fob_lost],
            calcdf[calcdf.flowDesc .== "Export", :cif_lost]
        )
        calcdf[calcdf.flowDesc .== "Export", :fobvalue] .= coalesce.(
            calcdf[calcdf.flowDesc .== "Export", :fob_lost],
            calcdf[calcdf.flowDesc .== "Export", :cifvalue]
        )
        calcdf[calcdf.flowDesc .== "Import", :cif_lost] .= coalesce.(
            calcdf[calcdf.flowDesc .== "Import", :fob_lost],
            calcdf[calcdf.flowDesc .== "Import", :cif_lost]
        )
        calcdf[calcdf.flowDesc .== "Import", :cifvalue] .= coalesce.(
            calcdf[calcdf.flowDesc .== "Import", :fob_lost],
            calcdf[calcdf.flowDesc .== "Import", :fobvalue]
        )

        fracloss_import = sum(skipmissing(calcdf[calcdf.flowDesc .== "Import", :cif_lost])) /
                        sum(skipmissing(calcdf[calcdf.flowDesc .== "Import", :cifvalue]))
        fracloss_export = sum(skipmissing(calcdf[calcdf.flowDesc .== "Export", :fob_lost])) /
                        sum(skipmissing(calcdf[calcdf.flowDesc .== "Export", :fobvalue]))

        append!(totimpacts, DataFrame(ISO=iso, fracloss_import=fracloss_import, fracloss_export=fracloss_export))
    end

    return (
        domar_change=domar_change,
        totimpacts2=totimpacts
    )
end

