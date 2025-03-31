using DataFrames
using Statistics
using LinearAlgebra
using GLM
using Memoization

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

function calc_domar_distribute_method(model::Model, dimpact::Vector{Float64})
    isos = dim_keys(model, :country)
    domar_change = calc_domar_change(isos, dimpact)

    # Distribute domar loss
    totimpacts = DataFrame(ISO=AbstractString[], fracloss_import=Union{Float64, Missing}[],
                           fracloss_export=Union{Float64, Missing}[])

    for iso in isos
        maxgrow = max(0, dimpact[findfirst(isequal.(isos, iso))])

        if iso ∈ aggregates
            # aggisos = get_aggregateinfo().ISO[get_aggregateinfo().Aggregate .== iso]
            # fracloss_imports = Union{Float64, Missing}[]
            # fracloss_exports = Union{Float64, Missing}[]
            # for aggiso in aggisos
            #     fracloss_import, fracloss_export = calc_domar_distribute_iso(iso, maxgrow, model, dimpact)
            #     push!(fracloss_imports, fracloss_import)
            #     push!(fracloss_exports, fracloss_export)
            # end
            # fracloss_import = mean(skipmissing(fracloss_imports))
            # fracloss_export = mean(skipmissing(fracloss_exports))

            # None of the aggregated ISOs are currently present in UNComtrade
            fracloss_import = missing
            fracloss_export = missing
        else
            fracloss_import, fracloss_export = calc_domar_distribute_iso(iso, maxgrow, model, dimpact)
        end

        append!(totimpacts, DataFrame(ISO=iso, fracloss_import=fracloss_import, fracloss_export=fracloss_export))
    end

    # Fill in missing entries
    totimpacts.fracloss_import[ismissing.(totimpacts.fracloss_import)] .= mean(skipmissing(totimpacts.fracloss_import))
    totimpacts.fracloss_export[ismissing.(totimpacts.fracloss_export)] .= mean(skipmissing(totimpacts.fracloss_export))

    return (domar_change=domar_change, totimpacts2=totimpacts)
end

@memoize get_comtrade(iso) = trade_comtrade[trade_comtrade.reporterISO .== iso .&& trade_comtrade.partnerISO .!= "W00", :]
@memoize get_comtrade_modelorder(iso, model) = leftjoin(get_comtrade(iso), DataFrame(ISO=dim_keys(model, :country), index=1:dim_count(model, :country)), on=:partnerISO => :ISO)



function calc_domar_distribute_iso(iso::AbstractString, maxgrow::Float64, model::Model, dimpact::Vector{Float64})
    calcdf = get_comtrade_modelorder(iso, model)
    if nrow(calcdf) == 0
        return (missing, missing)
    end

    calcdf[!, :dimpact] = [ismissing(ii) ? missing : dimpact[ii] for ii in calcdf.index]

    # Limit any growth to the growth of the country
    calcdf[!, :cif_lost] = Union{Missing, Float64}[calcdf.cifvalue .* max.(-calcdf.dimpact, -maxgrow)...]
    calcdf[!, :fob_lost] = Union{Missing, Float64}[calcdf.fobvalue .* max.(-calcdf.dimpact, -maxgrow)...]

    # Fill in missing values with preference based on direction
    exportrows = calcdf.flowDesc .== "Export"
    importrows = calcdf.flowDesc .== "Import"
    calcdf[exportrows, :fob_lost] .= coalesce.(
        calcdf[exportrows, :fob_lost],
        calcdf[exportrows, :cif_lost]
    )
    calcdf[exportrows, :fobvalue] .= coalesce.(
        calcdf[exportrows, :fobvalue],
        calcdf[exportrows, :cifvalue]
    )
    calcdf[importrows, :cif_lost] .= coalesce.(
        calcdf[importrows, :cif_lost],
        calcdf[importrows, :fob_lost]
    )
    calcdf[importrows, :cifvalue] .= coalesce.(
        calcdf[importrows, :cifvalue],
        calcdf[importrows, :fobvalue]
    )

    fracloss_import = sum(skipmissing(calcdf[importrows, :cif_lost])) /
        sum(skipmissing(calcdf[importrows, :cifvalue]))
    fracloss_export = sum(skipmissing(calcdf[exportrows, :fob_lost])) /
        sum(skipmissing(calcdf[exportrows, :fobvalue]))

    return (fracloss_import, fracloss_export)
end
