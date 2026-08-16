capitalparams = CSV.read(pagedata("other/capital.csv"), DataFrame)

@defcomp MacroParameters begin
    country = Index()

    config_save = Parameter{String}()

    model = Parameter{Model}()
    capital_draw = Parameter{Int64}(default=-1)
    save_constant = Parameter(unit="%", default=15.0) # pp33 PAGE09 documentation

    save_savingsrate = Variable(index=[country], unit="%")

    function init(pp, vv, dd)
        mc = pp.capital_draw == -1 ? nothing : pp.capital_draw
        if mc == nothing && pp.config_save == "constant"
            vv.save_savingsrate[:] .= pp.save_constant
        else
            vv.save_savingsrate[:] = readcountrydata_im(pp.model, capitalparams, :ISO, :mc, mc, "saverate") * 100
            # Don't allow negative savings
            vv.save_savingsrate[vv.save_savingsrate .< 0.] .= 0.
        end
    end

    function run_timestep(pp, vv, dd, tt)
    end
end

function addmacroparameters(model::Model, config_save::String="inferred")
    macroparams = add_comp!(model, MacroParameters)

    macroparams[:model] = model
    macroparams[:capital_draw] = -1
    macroparams[:config_save] = config_save

    return macroparams
end
