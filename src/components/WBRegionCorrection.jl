using Mimi

wbrcr = CSV.read(pagedata("pollution/wbratios.csv"), DataFrame, missingstring="NA")

@defcomp WBRegionCorrection begin
    country       = Index()

    model = Parameter{Model}()

    morb_healthcare_old = Parameter(index=[time, country], unit="\$M/yr")
    morb_productivity_old = Parameter(index=[time, country], unit="\$M/yr")
    morb_disutility_old = Parameter(index=[time, country], unit="\$M/yr")
    mort_disutility_old = Parameter(index=[time, country], unit="\$M/yr")

    morb_healthcare_new = Variable(index=[time, country], unit="\$M/yr")
    morb_productivity_new = Variable(index=[time, country], unit="\$M/yr")
    morb_disutility_new = Variable(index=[time, country], unit="\$M/yr")
    mort_productivity_new = Variable(index=[time, country], unit="\$M/yr")
    mort_disutility_new = Variable(index=[time, country], unit="\$M/yr")

    function run_timestep(pp, vv, dd, tt)
        for cc in dd.country
            morbrows = wbrcr[(wbrcr.ISO3 .== dim_keys(pp.model, :country)[cc]) .& (wbrcr.CAT .== "MORB"), :]
            mortrows = wbrcr[(wbrcr.ISO3 .== dim_keys(pp.model, :country)[cc]) .& (wbrcr.CAT .== "MORB"), :]

            if nrow(morbrows) > 0
                vv.morb_healthcare_new[tt, cc] = pp.morb_healthcare_old[tt, cc] * get_gains_value(morbrows, gettime(tt), "HEALTHCARE_RATIO", 1.)
                vv.morb_productivity_new[tt, cc] = pp.morb_productivity_old[tt, cc] * get_gains_value(morbrows, gettime(tt), "PRODUCTIVITY_RATIO", 1.)
                vv.morb_disutility_new[tt, cc] = pp.morb_disutility_old[tt, cc] * get_gains_value(morbrows, gettime(tt), "DISUTILITY_RATIO", 1.)
            else
                vv.morb_healthcare_new[tt, cc] = pp.morb_healthcare_old[tt, cc]
                vv.morb_productivity_new[tt, cc] = pp.morb_productivity_old[tt, cc]
                vv.morb_disutility_new[tt, cc] = pp.morb_disutility_old[tt, cc]
            end

            if nrow(mortrows) > 0
                vv.mort_productivity_new[tt, cc] = pp.mort_disutility_old[tt, cc] * get_gains_value(mortrows, gettime(tt), "PRODUCTIVITY_DISUTILITY_RATIO", 0.5)
                vv.mort_disutility_new[tt, cc] = pp.mort_disutility_old[tt, cc] * get_gains_value(mortrows, gettime(tt), "DISUTILITY_RATIO", 1.)
            else
                vv.mort_productivity_new[tt, cc] = pp.mort_disutility_old[tt, cc] * 0.5
                vv.mort_disutility_new[tt, cc] = pp.mort_disutility_old[tt, cc]
            end
        end
    end
end

function addwbregioncorrection(model::Model)
    wbrc = add_comp!(model, WBRegionCorrection)

    wbrc[:model] = model

    return wbrc
end
