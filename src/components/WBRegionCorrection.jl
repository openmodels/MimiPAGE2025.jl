using Mimi

wbrcr = CSV.read(pagedata("pollution/wbratios.csv"), DataFrame, missingstring="NA")
wbinf = CSV.read(pagedata("pollution/wbinfcosts.csv"), DataFrame, missingstring="NA")
# wbmatch = CSV.read(pagedata("pollution/wbregioncorrections-match.csv", DataFrame, missingstring="NA"))
# wbregions = CSV.read(pagedata("pollution/regiongroups_WB_global_flagship_2024.csv", DataFrame, missingstring="NA"))

@defcomp WBRegionCorrection begin
    country = Index()

    model = Parameter{Model}()
    gdp0_initgdp = Parameter(index=[country], unit="\$M") # GDP in y_year_0
    e_countryCO2emissions = Parameter(index=[time, country], unit="Mtonne/year")

    morb_healthcare_old = Parameter(index=[time, country], unit="\$M/yr")
    morb_productivity_old = Parameter(index=[time, country], unit="\$M/yr")
    morb_disutility_old = Parameter(index=[time, country], unit="\$M/yr")
    mort_disutility_old = Parameter(index=[time, country], unit="\$M/yr")

    morb_healthcare_new = Variable(index=[time, country], unit="\$M/yr")
    morb_productivity_new = Variable(index=[time, country], unit="\$M/yr")
    morb_disutility_new = Variable(index=[time, country], unit="\$M/yr")
    mort_productivity_new = Variable(index=[time, country], unit="\$M/yr")
    mort_disutility_new = Variable(index=[time, country], unit="\$M/yr")

    infrastructure_cost = Variable(index=[time, country], unit="\$M/yr")

    function run_timestep(pp, vv, dd, tt)
        for cc in dd.country
            morbrows = wbrcr[(wbrcr.ISO3 .== dim_keys(pp.model, :country)[cc]) .& (wbrcr.CAT .== "MORB"), :]

            if nrow(morbrows) > 0
                vv.morb_healthcare_new[tt, cc] = pp.morb_healthcare_old[tt, cc] * get_gains_value(morbrows, gettime(tt), "HEALTHCARE_RATIO", 1.)
                vv.morb_productivity_new[tt, cc] = pp.morb_productivity_old[tt, cc] * get_gains_value(morbrows, gettime(tt), "PRODUCTIVITY_RATIO", 1.)
                vv.morb_disutility_new[tt, cc] = pp.morb_disutility_old[tt, cc] * get_gains_value(morbrows, gettime(tt), "DISUTILITY_RATIO", 1.)
            else
                vv.morb_healthcare_new[tt, cc] = pp.morb_healthcare_old[tt, cc]
                vv.morb_productivity_new[tt, cc] = pp.morb_productivity_old[tt, cc]
                vv.morb_disutility_new[tt, cc] = pp.morb_disutility_old[tt, cc]
            end

            mortrows = wbrcr[(wbrcr.ISO3 .== dim_keys(pp.model, :country)[cc]) .& (wbrcr.CAT .== "MORT"), :]

            if nrow(mortrows) > 0
                inter = get_gains_value(mortrows, gettime(tt), "MORTPROD_INTERCPT_COEFF", 0.)
                coef1 = get_gains_value(mortrows, gettime(tt), "MORTPROD_MORBPROD_COEFF", 0.)
                coef2 = get_gains_value(mortrows, gettime(tt), "MORTPROD_MORTDISU_COEFF", 0.)
                if ismissing(coef1) || ismissing(coef2)
                    vv.mort_productivity_new[tt, cc] = 0.
                else
                    vv.mort_productivity_new[tt, cc] = inter + pp.morb_productivity_old[tt, cc] * coef1 + pp.mort_disutility_old[tt, cc] * coef2
                    if vv.mort_productivity_new[tt, cc] < 0.
                        vv.mort_productivity_new[tt, cc] = 0.
                    end
                end
                vv.mort_disutility_new[tt, cc] = pp.mort_disutility_old[tt, cc] * get_gains_value(mortrows, gettime(tt), "DISUTILITY_RATIO", 1.)
            else
                vv.mort_productivity_new[tt, cc] = 0.
                vv.mort_disutility_new[tt, cc] = pp.mort_disutility_old[tt, cc]
            end

            infrow = wbinf[(wbinf.ISO3 .== dim_keys(pp.model, :country)[cc]) .& (wbinf.YEAR .== min(max(2030, gettime(tt)), 2050)), :]

            if nrow(infrow) > 0
                @assert nrow(infrow) == 1
                if ismissing(pp.gdp0_initgdp[cc])
                    vv.infrastructure_cost[tt, cc] = 0.
                else
                    vv.infrastructure_cost[tt, cc] = (infrow.interceptpgdp[1] * pp.gdp0_initgdp[cc] + infrow.slope[1] * pp.e_countryCO2emissions[tt, cc]) * 1.1102 * 81.551 / 97.315
                end
            else
                vv.infrastructure_cost[tt, cc] = 0.
            end
        end
    end
end

function addwbregioncorrection(model::Model)
    wbrc = add_comp!(model, WBRegionCorrection)

    wbrc[:model] = model

    return wbrc
end
