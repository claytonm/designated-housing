# folder to write figures to
figuresFolder = "../figsForPew/"
# Do elderly and disabled populations in PHAs reflect demand
# for subsidized housing among E & D?
# plot % elderly in PHA 
#  vs  % elderly li renters w/HP among all li renters w/HP
df = merge(pha, chas5, by.x = "county10", by.y = "county10", suffixes = "")
df$eliRat = 100*(df$T5_est5 + df$T5_est6)/df$T5_est4
df$eliRatRent = 100*(df$T5_est69 + df$T5_est70)/df$T5_est68
df$eliRatTotal = 100*(df$T5_est5 + df$T5_est6 + df$T5_est69 + df$T5_est70)/(df$T5_est4 + df$T5_est68)

EphaEdemand = g.d(data = df,
                  x = "eliRatTotal",
                  y = "PCT_AGE62P",
                  size = "TOTAL_UNIT",
                  alpha = .5,
                  fill.var = "plan_status",
                  fill.val = c("active", "expired"),
                  fill.colors = c("#BFBFBF", "#000000"),#002E5C
                  title_size = 12,
                  axis_title_size = 10,
                  axis_text_size = 8,
                  xlab = "% ELI Households With Housing Problems\nThat Are Elderly",
                  ylab = "% PHA Units That Are Elderly-Occupied"
                  )

rows = df$units_elderly > 0 | df$units_mix > 0
eldOverRepPlan = 100*sum(df$eliRatTotal[rows] < df$PCT_AGE62P[rows], na.rm = T)/sum(rows)

rows = df$units_elderly == 0 & df$units_mix == 0
eldOverRepNone = 100*sum(df$eliRatTotal[rows] < df$PCT_AGE62P[rows], na.rm = T)/sum(rows)


df = merge(df, chas6, by.x = "county10", by.y = "county10", suffixes = "")
df$eliRatDis = 100*df$T6_est55/(df$T5_est4 + df$T5_est68)


DphaDdemand = g.d(data = df,
                  x = "eliRatDis",
                  y = "PCT_DISABL",
                  size = "TOTAL_UNIT",
                  alpha = .5, 
                    fill.var = "plan_status",
                    fill.val = c("active", "expired"),
                    fill.colors = c("#BFBFBF", "#000000"),#002E5C
                    title_size = 12,
                    axis_title_size = 10,
                    axis_text_size = 8,
                  xlab = "% ELI Households With Housing Problems\nThat Are Disabled",
                  ylab = "% PHA Units That Are Disabled-Occupied")

rows = df$units_disabled > 0 | df$units_mix > 0
disOverRepPlan = 100*sum(df$eliRatDis[rows] < df$PCT_DISABL[rows], na.rm = T)/sum(rows)

rows = df$units_disabled == 0 & df$units_mix == 0
disOverRepNone = 100*sum(df$eliRatDis[rows] < df$PCT_DISABL[rows], na.rm = T)/sum(rows)


##########
# Has proportion of designated units for elderly/disabled changed over time?
# make stacked bar charts as described in Section 2 bullet 1
df = ddply(pha_designation,
           .(date_rec_year),
           summarize,
           units_elderly = sum(units_elderly, na.rm = T),
           units_disabled = sum(units_disabled, na.rm = T),
           units_mix = sum(units_mix, na.rm = T))

df = melt(df[,c("date_rec_year",
                             "units_elderly",
                             "units_disabled",
                             "units_mix")],
          id.vars=c("date_rec_year"),
          variable.name = "status",
          value.name="units")

df = ddply(df,
           .(date_rec_year),
           transform,
           units_prop = 100*units/sum(units))

df = df[1:60,]

desByTime = ggplot(df,
                   aes(x = date_rec_year, y = units, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
     theme_minimal(title_size = 12,
                   axis_text_size = 8,
                   axis_title_size = 10) +
     scale_fill_brewer(palette = "Blues") +
     theme(legend.position = "none") +
     labs(x = "",
          y = "Number Designated",
          title = "Designated Units By Type: 1994 to 2013") +
     scale_x_discrete(labels = paste("'",
                                     c(94:99,
                                       paste("0",0:9,sep = ""),
                                       paste("1",0:3, sep = "")),
                                     sep = ""))
                            

desPropByTime = ggplot(df,
                       aes(x = date_rec_year, y = units_prop, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
     theme_minimal(title_size = 12,
                   axis_text_size = 8,
                   axis_title_size = 10) +
     scale_fill_brewer(palette = "Blues") +
     labs(x = "Year",
          y = "% Designated",
          title = "Percent Designated Units by Type: 1994 to 2013") +
     theme(legend.position = "none") +
     scale_x_discrete(labels = paste("'",
                                     c(94:99,
                                       paste("0",0:9,sep = ""),
                                       paste("1",0:3, sep = "")),
                                     sep = ""))
     


# end make stacked bar chart


##########
# bar chart of units designated elderly as percent
# of all elderly residents in all relevant programs
df = merge(allProgramsAge,
           pha,
           by.x = "pha_code",
           by.y="PARTICIPAN",
           suffixes = "")[c("pha_code",
                            "v1",
                            paste("v",seq(2,12,by=2),sep=""),
                            "plan_status",
                            "units_elderly",
                            "units_disabled",
                            "units_mix",
                            "FORMAL_PAR")]

df$elderly = apply(df[,c("v10","v12")], 1, sum)
df$pct_designated_elderly = 100*df$units_elderly/df$elderly
ePctDesUnits = ggplot(data = df[df$plan_status == "active" &
                                     df$units_elderly > 0,],
                      aes(x = pct_designated_elderly)) +
     geom_bar(breaks = seq(from = 0, to = 200, by = 10),
              width = .3,
              fill = "#BFBFBF",
              color = "black")+
     xlim(0,200) +
     ylim(0,20) +
     theme_minimal(title_size = 14,
                   axis_text_size = 10,
                   axis_title_size = 10) +
     theme(text = element_text(family = "twCent")) +
     labs(x = "Units Designated Elderly As\n% of Elderly PHA Residents",
          y = "Number of PHAs",
          title = "Elderly")

# bar chart of units designated disabled 
# as percent of disabled residents in all relevant programs
df = merge(allProgramsDisabled,
           pha,
           by.x = "pha_code",
           by.y ="PARTICIPAN",
           suffixes = "")[c(paste("v",seq(2,16,by=2),sep=""),"plan_status", "units_elderly", "units_disabled", "units_mix", "FORMAL_PAR")]

df$disabled = apply(df[,c("v10","v12","v14","v16")], 1, sum)
df$pct_designated_disabled = 100*df$units_disabled/df$disabled
dPctDesUnits = ggplot(data = df[df$plan_status == "active" &
                                     df$pct_designated_disabled > 0,],
                      aes(x = pct_designated_disabled)) +
     geom_bar(breaks = seq(from = 0, to = 200, by = 10),
              fill = "#BFBFBF",
              color = "black")+
     xlim(0,200) +
     ylim(0,20) +
     theme_minimal(title_size = 14,
                   axis_text_size = 10,
                   axis_title_size = 10) +
     theme(text = element_text(family = "twCent")) +
     labs(x = "Units Designated Disabled As\n% of Disabled PHA Residents",
          y = "Number of PHAs",
          title = "Disabled")

write.csv(df,
          paste(tablesFolder,
                "fig2BottomRow.csv",
                sep = ""),
          row.names = F)

##########
# bar chart of elderly in pha  
# as percent of county-wide demand (CHAS) -
# total county-wide supply from all other phas (from RCR)
df = merge(allProgramsAge,
           pha,
           by.x = "pha_code",
           by.y="PARTICIPAN",
           suffixes = "")[,c("pha_code",
                             "v1",
                             paste("v",seq(2,12,by=2),sep=""),
                             "plan_status",
                             "units_disabled",
                             "units_elderly",
                             "units_mix",
                             "county10")]

df$elderly = df$v10 + df$v12

# summarize elderly pha population by county
df = ddply(df,
           .(county10),
           transform,    # add elderlyCounty to existing df
           elderlyCounty = sum(elderly))

df = merge(df,
           chas16,
           by = "county10",
           suffixes = "")[,c(names(df),"T16_est5","T16_est17","T16_est90","T16_est102")]

df$eld_hp = apply(df[,c("T16_est5","T16_est17","T16_est90","T16_est102")], 1, sum)

df$eld_supply_otherPHAs = df$elderlyCounty - df$elderly
df$eld_resid_demand = df$eld_hp - df$eld_supply_otherPHAs
df$eld_resid_demand_pct_met = 100*df$elderly/df$eld_resid_demand

eldResidDemandNone = g.b(df[df$plan_status != "active",],
                         x = "eld_resid_demand_pct_met",
                         breaks = seq(from = 0, to = 100, by = 10),
                         xmin = 0,
                         xmax = 100,
                         ymax = 0.6,
                         title = "None",
                         xlab = "% Residual Demand Met",
                         ylab = "% PHAs",
                         title_size = 14,
                         axis_text_size = 10,
                         axis_title_size = 10)

eldResidDemandNone.mean = mean(df$eld_resid_demand_pct_met[df$units_elderly == 0])

eldResidDemandEldery = g.b(df[df$plan_status == "active" &
                                   df$units_elderly > 0,],
                            x = "eld_resid_demand_pct_met",
                            breaks = seq(from = 0, to = 100, by = 10),
                            xmin = 0,
                            xmax = 100,
                            ymin = 0,
                            ymax = .6,
                           title = "Elderly",
                           xlab = "",
                            title_size = 14,
                            axis_text_size = 10,
                            axis_title_size = 12)

eldResidDemandElderly.mean = mean(df$eld_resid_demand_pct_met[df$units_elderly > 0])


eldResidDemandMixed = g.b(df[df$plan_status == "active" &
                                  df$units_mix > 0,],
                          "eld_resid_demand_pct_met",
                          breaks = seq(from = 0, to = 100, by = 10),
                          xmin = 0,
                          xmax = 100,
                          ymin = 0,
                          ymax = .6,
                          title = "Mixed",
                          xlab = "",
                          title_size = 14,
                          axis_text_size = 10,
                          axis_title_size = 12)

eldResidDemandMixed.mean = mean(df$eld_resid_demand_pct_met[df$units_mix > 0])

colsNames = c("pha_code",
              "v1",
              "county10",
              "plan_status",
              "units_disabled",
              "units_elderly",
              "units_mix",
              "elderly",
              "elderlyCounty",
              "eld_hp",
              "eld_supply_otherPHAs",
              "eld_resid_demand",
              "eld_resid_demand_pct_met")

write.csv(df[,colsNames],
          paste(tablesFolder,
                "fig2TopRow.csv",
                sep = ""),
          row.names = F)
##########

##########
# bar chart of disabled in pha  
# as percent of county-wide demand (CHAS) -
# total county-wide supply from all other phas (from RCR)
df = merge(allProgramsDisabled,
           pha,
           by.x = "pha_code",
           by.y="PARTICIPAN",
           suffixes = "")[,c(paste("v",seq(10,16,by=2),sep=""),
                             "plan_status",
                             "units_disabled",
                             "units_elderly",
                             "units_mix",
                             "county10")]

df$disabled = df$v10 + df$v12 + df$v14 + df$v16

# summarize disabled pha population by county
df = ddply(df,
           .(county10),
           transform,
           disabledCounty = sum(disabled))

df = merge(df,
           chas6,
           by = "county10",
           suffixes = "")[,c(names(df),"T6_est55")]

df$dis_hp = df$T6_est55

df$dis_supply_otherPHAs = df$disabledCounty - df$disabled
df$dis_resid_demand = df$dis_hp - df$dis_supply_otherPHAs
df$dis_resid_demand_pct_met = 100*df$disabled/df$dis_resid_demand

disResidDemandNone = g.b(df[df$plan_status != "active",],
                            x = "dis_resid_demand_pct_met",
                            breaks = seq(from = 0, to = 100, by = 10),
                            xmin = 0,
                            xmax = 100,
                            ymin = 0,
                         ymax = 0.6,
                         title = "None",
                         xlab = "",
                         ylab = "% PHAs",
                            title_size = 14,
                            axis_text_size = 10,
                            axis_title_size = 12)

disResidDemandNone.mean = mean(df$dis_resid_demand_pct_met[df$units_disabled == 0])

disResidDemandDisabled = g.b(df[df$plan_status == "active" &
                                     df$units_disabled > 0,],
                           x = "dis_resid_demand_pct_met",
                           breaks = seq(from = 0, to = 100, by = 10),
                           xmin = 0,
                           xmax = 100,
                           ymin = 0,
                             ymax = 0.6,
                             title = "Disabled",
                             xlab = "",
                           title_size = 14,
                           axis_text_size = 10,
                           axis_title_size = 12)

disResidDemandDisabeld.mean = mean(df$dis_resid_demand_pct_met[df$units_disabled > 0])

disResidDemandMixed = g.b(df[df$plan_status == "active" & 
                                  df$units_mix > 0,],
                         x = "dis_resid_demand_pct_met",
                         breaks = seq(from = 0, to = 100, by = 10),
                         xmin = 0,
                         xmax = 100,
                         ymin = 0,
                          ymax = 0.6,
                          title = "Mixed",
                          xlab = "",
                         title_size = 14,
                         axis_text_size = 10,
                         axis_title_size = 12)

disResidDemandMixed.mean = mean(df$dis_resid_demand_pct_met[df$units_mix > 0])
##########

##########
# bar chart of Black in pha  
# as percent of county-wide demand (CHAS) -
# total county-wide supply from all other phas (from RCR)
df = merge(allProgramsRace,
           allProgramsDisabled,
           by = "pha_code",
           suffixes = "")

df[,"v3"] = df[,"v3"]*df[,"totalHHs"]/100

df = merge(df,
           pha,
           by.x = "pha_code",
           by.y="PARTICIPAN",
           suffixes = "")[,c("pha_code",
                             "v1",
                             "v3",
                             "plan_status",
                             "county10")]

df$aa = as.numeric(df$v3)

# summarize aa pha population by county
df = ddply(df,
           .(county10),
           transform,
           aaCounty = sum(aa))

df = merge(df,
           chas1,
           by = "county10",
           suffixes = "")[,c(names(df),"eli_aa", "vli_aa")]

df$aa_hp = df$eli_aa#  + df$vli_aa

df$aa_supply_otherPHAs = df$aaCounty - df$aa
df$aa_resid_demand = df$aa_hp - df$aa_supply_otherPHAs
df$aa_resid_demand_pct_met = 100*df$aa/df$aa_resid_demand

aaResidDemandNone = g.b.race(df[df$plan_status == "none" & !is.na(df$aa_resid_demand_pct_met),],
                         x = "aa_resid_demand_pct_met",
                         breaks = seq(from = 0, to = 100, by = 10),
                         xmin = 0,
                         xmax = 100,
                         ymin = 0,
                         ymax = 1,
                         title = "None",
                         xlab = "% Residual Demand Met",
                         ylab = "% PHAs",
                         title_size = 14,
                         axis_text_size = 10,
                         axis_title_size = 12)

aa_resid_demand_pct_metNone.mean = mean(df$aa_resid_demand_pct_met[df$plan_status == "none" & df$aa_resid_demand_pct_met < Inf],
                                        na.rm = T)

aaResidDemandPlan = g.b.race(df[df$plan_status != "none" & !is.na(df$aa_resid_demand_pct_met),],
                             x = "aa_resid_demand_pct_met",
                             breaks = seq(from = 0, to = 100, by = 10),
                             xmin = 0,
                             xmax = 100,
                             ymin = 0,
                             ymax = 1,
                             title = "Active",
                             xlab = "",
                             ylab = "% PHAs",
                             title_size = 14,
                             axis_text_size = 10,
                             axis_title_size = 12)

aa_resid_demand_pct_metPlan.mean = mean(df$aa_resid_demand_pct_met[df$plan_status != "none" & df$aa_resid_demand_pct_met < Inf],
                                        na.rm = T)

colNames = names(df)
write.csv(df[,colNames],
          paste(tablesFolder,
                "fig3TopRow.csv",
                sep = ""),
          row.names = F)

##########

##########
# bar chart of Hispanic in pha  
# as percent of county-wide demand (CHAS) -
# total county-wide supply from all other phas (from RCR)
df = merge(allProgramsEthnicity,
           allProgramsDisabled,
           by = "pha_code",
           suffixes = "")

df[,"v2"] = df[,"v2"]*df[,"totalHHs"]/100


df = merge(allProgramsEthnicity,
           pha,
           by.x = "pha_code",
           by.y="PARTICIPAN",
           suffixes = "")[,c("pha_code",
                             "v1",
                             "v2",
                             "plan_status",
                             "county10")]

df$hisp = as.numeric(df$v2)

# summarize aa pha population by county
df = ddply(df,
           .(county10),
           transform,
           hispCounty = sum(hisp))

df = merge(df,
           chas1,
           by = "county10",
           suffixes = "")[,c(names(df),"eli_hisp")]

df$eli_hisp_hp = df$eli_hisp

df$hisp_supply_otherPHAs = df$hispCounty - df$hisp
df$hisp_resid_demand = df$eli_hisp_hp - df$hisp_supply_otherPHAs
df$hisp_resid_demand_pct_met = 100*df$hisp/df$hisp_resid_demand

hispResidDemandNone = g.b.race(df[df$plan_status == "none" & !is.na(df$hisp_resid_demand_pct_met),],
                             x = "hisp_resid_demand_pct_met",
                             breaks = seq(from = 0, to = 100, by = 10),
                             xmin = 0,
                             xmax = 100,
                             ymin = 0,
                             ymax = 1,
                             title = "None",
                             xlab = "",
                             ylab = "% PHAs",
                             title_size = 14,
                             axis_text_size = 10,
                             axis_title_size = 12)

hisp_resid_demand_pct_metNone.mean = mean(df$hisp_resid_demand_pct_met[df$plan_status == "none" & df$hisp_resid_demand_pct_met < Inf],
                                        na.rm = T)

hispResidDemandPlan = g.b.race(df[df$plan_status != "none" & !is.na(df$hisp_resid_demand_pct_met),],
                             x = "hisp_resid_demand_pct_met",
                             breaks = seq(from = 0, to = 100, by = 10),
                             xmin = 0,
                             xmax = 100,
                             ymin = 0,
                             ymax = 1,
                             title = "Active",
                             xlab = "",
                             ylab = "% PHAs",
                             title_size = 14,
                             axis_text_size = 10,
                             axis_title_size = 12)

hisp_resid_demand_pct_metPlan.mean = mean(df$hisp_resid_demand_pct_met[df$plan_status != "none" & df$hisp_resid_demand_pct_met < Inf],
                                          na.rm = T)

write.csv(df,
          paste(tablesFolder,
                "fig3BottomRow.csv",
                sep = ""),
          row.names = F)
############

############
# compare pha access to transit by designation status
# graph pct phas by distance from BG population centroid 
# to nearest transit stop (sld$D4a)
df = pha
df$bg10 = paste(df$STATE2KX, df$CNTY2KX, df$TRACT2KX, df$BG2KX,
                sep = "")
df = merge(df,
           sld,
           by.x = "bg10",
           by.y = "GEOID10",
           suffixes = "")[,c("bg10", "plan_status", "D4a", "D5br", "D2A_JPHH", "D3apo")]
df$quarterMileBuffer = ifelse(df$D4a < 0, 0, 1)
bg10None = df$bg10[df$plan_status != "active"]
bg10Active = df$bg10[df$plan_status == "active"]
# percent pha's within
phaTransitTable = prop.table(table(df$plan_status, df$quarterMileBuffer), 1)

# 
distToTransitNone = g.b.transit(df[df$plan_status != "active" &
                                df$quarterMileBuffer == 1,],
                         x = "D4a",
                         breaks = seq(from = 0, to = 1300, by = 100),
                         xmin = 0,
                         xmax = 1210,
                         ymax = .3,
                         title = "None",
                         xlab = "Meters",
                         ylab = "% PHAs",
                         title_size = 14,
                         axis_text_size = 10,
                         axis_title_size = 10)

distToTransitActive = g.b.transit(df[df$plan_status == "active" &
                                        df$quarterMileBuffer == 1,],
                                x = "D4a",
                                breaks = seq(from = 0, to = 1300, by = 100),
                                xmin = 0,
                                xmax = 1210,
                                ymax = .3,
                                title = "Active",
                                xlab = "Meters",
                                ylab = "% PHAs",
                                title_size = 14,
                                axis_text_size = 10,
                                axis_title_size = 10)



jobAccessByTransitNone = g.b.transit(df[df$plan_status != "active" &
                                             df$D5br > 0,],
                                x = "D5br",
                                breaks = seq(from = 0, to = 1e5, by = 5e3),
                                xmin = 0,
                                xmax = 1e5,
                                ymax = 0.5,
                                title = "None",
                                xlab = "Jobs",
                                ylab = "% PHAs",
                                title_size = 14,
                                axis_text_size = 10,
                                axis_title_size = 10)

jobAccessByTransitActive = g.b.transit(df[df$plan_status == "active" &
                                               df$D5br > 0,],
                                  x = "D5br",
                                  breaks = seq(from = 0, to = 1e5, by = 5e3),
                                  xmin = 0,
                                  xmax = 1e5,
                                  ymax = 0.5,
                                  title = "Active",
                                  xlab = "Jobs",
                                  ylab = "% PHAs",
                                  title_size = 14,
                                  axis_text_size = 10,
                                  axis_title_size = 10)

# calculate median number of jobs accessible by transit 
# for phas by plan status
jobTransitAccessActive = median(df$D5br[df$D5br > 0 & df$plan_status == "active"])
jobTransitAccessNone = median(df$D5br[df$D5br > 0 & df$plan_status != "active"])

# calculate median number of jobs within bgs that contain phas 
# plan status
jobDensityNone = median(sld$D2A_JPHH[sld$GEOID10 %in% bg10None])
jobDensityActive = median(sld$D2A_JPHH[sld$GEOID10 %in% bg10Active])
# calculate median road density for pedestrian-oriented roads
# in bgs that contain phas 
pedRoadDensityNone = median(sld$D3apo[sld$GEOID10 %in% bg10None])
pedRoadDensityActive = median(sld$D3apo[sld$GEOID10 %in% bg10Active])

## for each pha, count number of community health centers 
## in the county in which pha is located
df = merge(pha,
           chc,
           by.x = "county10",
           by.y =  "State County FIPS Code",
           all.x = T,
           suffixes = "")[,c("PARTICIPAN", "Health Center Site Fact ID", "county10","plan_status")]

noCHCs = df$PARTICIPAN[is.na(df[,"Health Center Site Fact ID"])]


df = ddply(df,
           .(PARTICIPAN,plan_status),
           summarize,
           chc.count = length(PARTICIPAN))

df$chc.count[df$PARTICIPAN %in% noCHCs] = 0

# calculate percent of designated and non-designated PHAs that
# are in counties without CHCs
noCHCNonePct = 100*sum(df$PARTICIPAN %in% noCHCs & df$plan_status != "active", na.rm = T)/sum(df$plan_status != "active", na.rm = T)
noCHCActivePct = 100*sum(df$PARTICIPAN %in% noCHCs & df$plan_status == "active", na.rm = T)/sum(df$plan_status == "active", na.rm = T)


chcCountyLevelNone = g.b.transit(df[df$plan_status != "active",],
                                       x = "chc.count",
                                       breaks = seq(from = 0, to = 400, by = 20),
                                       xmin = 0,
                                       xmax = 400,
                                       ymax = 1,
                                       title = "None",
                                       xlab = "Community Health Centers",
                                       ylab = "% PHAs",
                                       title_size = 14,
                                       axis_text_size = 10,
                                       axis_title_size = 10)
chcCountyLevelActive = g.b.transit(df[df$plan_status == "active",],
                                   x = "chc.count",
                                   breaks = seq(from = 0, to = 400, by = 20),
                                   xmin = 0,
                                   xmax = 400,
                                   ymax = 1,
                                   title = "Active",
                                   xlab = "Community Health Centers",
                                   ylab = "% PHAs",
                                   title_size = 14,
                                   axis_text_size = 10,
                                   axis_title_size = 10)

median.chcCountyLevelActive = median(df$chc.count[df$plan_status == "active"])
median.chcCountyLevelNone = median(df$chc.count[df$plan_status != "active"])

# save work
save.image()

                           











