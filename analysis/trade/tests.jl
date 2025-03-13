include("../../src/main_model.jl")

for config_capital in ["constant", "full"]
    for use_trade in [false, true]
        # config_capital = "constant"; use_trade = true
        model = getpage(config_capital=config_capital, use_trade=use_trade)
        run(model)
        if config_capital == "full"
            println([config_capital, use_trade, mean(model[:Capital, :fracloss][10, :]), sum(model[:TotalCosts, :total_damages_peryear][10, :]), sum(model[:Discontinuity, :rgdp_per_cap_DiscRemainGDP][10, :])])
        else
            println([config_capital, use_trade, 0., sum(model[:TotalCosts, :total_damages_peryear][10, :]), sum(model[:Discontinuity, :rgdp_per_cap_DiscRemainGDP][10, :])])
        end

    end
end
