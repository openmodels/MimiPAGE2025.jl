function find_model_years(yy, y_year_0, y_year)
    if yy <= y_year_0
        return 0, 0
    elseif yy <= y_year[TimestepIndex(1)]
        return 0, 1
    end

    for ii in 1:(length(y_year) - 1)
        if y_year[TimestepIndex(ii)] <= yy && yy <= y_year[TimestepIndex(ii+1)]
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
            y2 = y_year[TimestepIndex(1)]
            v1 = vv0
            v2 = vv[1]
        else
            j1, j2 = TimestepIndex(i1), TimestepIndex(i2)
            y1, y2 = y_year[j1], y_year[j2]
            v1 = vv[i1]
            v2 = vv[i2]
        end
        return v1 + (v2 - v1) * (yy - y1) / (y2 - y1)
    end
end
