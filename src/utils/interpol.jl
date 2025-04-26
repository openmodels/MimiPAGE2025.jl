function find_model_years(yy, y_year_0, y_year)
    if yy <= y_year_0
        return 0, 0
    elseif yy < y_year[ii]
        return 0, 1
    end

    for ii in 1:(length(y_year) - 1)
        if y_year[ii] <= yy && yy <= y_year[ii+1]
            return ii, ii+1
        end
    end

    error("Year outside of model simulation.")
end

function interpolate(yy, y_year_0, vv0, y_year, vv)
    i1, i2 = find_model_years(yy, y_year_0, y_year)
    if i1 == 0 && i2 == 0
        return vv0
    else
        if i1 == 0 && i2 == 1
            y1 = y_year_0
            y2 = y_year[1]
            v1 = vv0
            v2 = vv[1]
        else
            y1, y2 = y_year[i1], y_year[i2]
            v1 = vv[TimestepIndex(i1)]
            v2 = vv[TimestepIndex(i2)]
        end
        return v1 + (v2 - v1) * (yy - y1) / (y2 - y1)
    end
end
