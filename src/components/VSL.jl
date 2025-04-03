using Mimi

# Calculate the value of a statistical life
# follows equations from FUND 

@defcomp VSL begin

    country       = Index()

    gdp         = Parameter(index=[time, country], unit="\$M")
    population  = Parameter(index=[time, country], unit="million person")

    α             = Parameter(unit = "US\$2005")    # VSL scaling parameter
    ϵ             = Parameter()                     # Income elasticity of the value of a statistical life.
    y₀            = Parameter(unit = "US\$2005")    # Normalization constant.
    pc_gdp        = Variable(index=[time, country], unit = "US\$2005/yr/person") # Country-level per capita GDP ($/person).

    vsl           = Variable(index=[time, country], unit = "US\$2005/yr") # Value of a statistical life ($).
    
    function run_timestep(p, v, d, t)
        for c in d.country
            v.pc_gdp[t, c] = (p.gdp[t, c]) ./ (p.population[t, c])
            v.vsl[t,c] = p.α * (v.pc_gdp[t,c] / p.y₀) ^ p.ϵ
        end
    end
end


function addVSL(m::Model, vsl::Symbol)
    
	compref = add_comp!(m, VSL, :VSL);
    
    # --------------------------------------------------------------------------    
    # VSL
    # --------------------------------------------------------------------------

    if vsl==:fund
	    update_param!(m, :VSL, :α,  4.99252262888626e6 * pricelevel_1995_to_2005)   # convert from FUND USD $1995 to USD $2005
        update_param!(m, :VSL, :y₀, 24_962.6131444313  * pricelevel_1995_to_2005)   # convert from FUND USD $1995 to USD $2005
    elseif vsl==:epa
	    update_param!(m, :VSL, :α,  7.73514707e6)                                   # 2020 EPA VSL in 2005$. See DataExplainer.ipynb for information
        update_param!(m, :VSL, :y₀, 48_726.60)                                      # 2020 U.S. income per capita in 2005$; See DataExplainer.ipynb for information  
    elseif vsl==:uba
	    update_param!(m, :VSL, :α,  5_920_000. / pricelevel_2005_to_2020)           # 2020 UBA VSL in 2005$
        update_param!(m, :VSL, :y₀, 44_646.78)                                      # 2020 German income per capita in 2005$
    else
        error("Invalid vsl argument of $vsl.")
    end

	update_param!(m,  :VSL, :ϵ, 1.0)
	
    compref
end
