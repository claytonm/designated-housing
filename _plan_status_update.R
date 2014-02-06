df = read.dbf("C:/HIA/HUD/data/spatial/assisted_housing/output/phaByPlanStatus.dbf",
         as.is = T)

df$plan_statu = ifelse(df$plan_statu == "active",
                       df$plan_statu,
                       "none")

df$units_disa2 = ifelse(df$plan_statu == "active",
                       df$units_disa,
                       0)

df$units_mix2 = ifelse(df$plan_statu == "active",
                       df$units_mix,
                       0)

df$units_elde2 = ifelse(df$plan_statu == "active",
                       df$units_elde,
                       0)


