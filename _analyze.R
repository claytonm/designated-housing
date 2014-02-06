## write tables for Pew here
tablesFolder = paste(getwd(),"/tablesForPew/",sep = "")

##########
# total pha units
# total non-pha units
total.pha = nrow(pha)
units.total.pha = sum(pha$TOTAL_UNIT)
units.total.multi_fam = sum(multi_fam$TOTAL_ASSI)
units.total.lihtc = sum(lihtc$LI_UNITS)
units.pha.pct = 100*units.total.pha/(units.total.pha + units.total.lihtc)

# total pha-designated units
units.designated =  
     as.integer(sum(pha_designation$units_elderly) +
     sum(pha_designation$units_disabled) +
     sum(pha_designation$units_mixed))
units.designated.pct = 100*units.designated/units.total.pha

units.designated.elderly = as.integer(sum(pha_designation$units_elderly))
units.designated.elderly.pct = 100*units.designated.elderly/units.designated                               
units.designated.disabled = as.integer(sum(pha_designation$units_disabled))
units.designated.disabled.pct = 100*units.designated.disabled/units.designated
units.designated.mixed = as.integer(sum(pha_designation$units_mix))
units.designated.mixed.pct = 100*units.designated.mixed/units.designated


# total phas with designated units
phas.designated = nrow(pha_designation)
phas.designated.pct = 100*phas.designated/total.pha

     
##########
# table of phas by size
phaBySize = ddply(pha,
                  .(size),
                  summarize,
                  authorities = length(size),
                  units = sum(TOTAL_UNIT))
phaBySize$authorities_pct = 100*phaBySize$authorities/sum(phaBySize$authorities)
phaBySize$units_pct = 100*phaBySize$units/sum(phaBySize$units)
Total = c(NA, sum(phaBySize$authorities), sum(phaBySize$units),sum(phaBySize$authorities_pct), sum(phaBySize$units_pct))
phaBySize = rbind(phaBySize, Total)
row.names(phaBySize) = c(as.character(phaBySize$size[1:6]),"Total")

write.csv(phaBySize,
          paste(tablesFolder,"table1phaBySize.csv", sep = ""))
##########

##########
# table of designation status and pha size
tPlanStatusBySize = round(100*prop.table(table(pha[,c("size", "plan_status")]),
                                   margin = 1), 1)
tPlanStatusBySize = rbind(tPlanStatusBySize, 100*table(pha[,c("plan_status")])/sum(table(pha[,c("plan_status")])))
tPlanStatusBySize = data.frame(as.matrix(tPlanStatusBySize))

# table of designation status by Total Units 
# designated Elderly, Disabled, and Mixed
unitsDesBySize = ddply(pha[pha$plan_status == "active",],
                       .(size),
                       summarize,
                       elderly = sum(units_elderly),
                       disabled = sum(units_disabled),
                       mixed = sum(units_mix))
row.names(unitsDesBySize) = unitsDesBySize$size
unitsDesBySize = unitsDesBySize[,-1]
unitsDesBySize$elderly_prop = 100*unitsDesBySize$elderly/sum(unitsDesBySize$elderly)
unitsDesBySize$disabled_prop = 100*unitsDesBySize$disabled/sum(unitsDesBySize$disabled)
unitsDesBySize$mixed_prop = 100*unitsDesBySize$mixed/sum(unitsDesBySize$mixed)
totals = apply(unitsDesBySize, 2, sum)
unitsDesBySize = rbind(unitsDesBySize, totals)
row.names(unitsDesBySize)[ncol(unitsDesBySize)] = "Total"

tPlanStatusBySize = cbind(tPlanStatusBySize,
                          unitsDesBySize)

row.names(tPlanStatusBySize)[nrow(tPlanStatusBySize)] = "Total"
##########

##########
# table of race by pha size
df = merge(phaRace,
           pha,
           by.x = "pha_code",
           by.y="PARTICIPAN",
           suffixes = "")[c(names(phaRace),"size","TOTAL_OCCU")]

df = merge(df,
           phaEthnicity,
           by.x = "pha_code",
           by.y = "pha_code",
           suffixes = "")[c(names(df),"Hispanic")]

race.df = round(df[,"TOTAL_OCCU"]*df[,c("WhiteOnly","AfricanAmericanOnly","AsianOnly","AmericanIndianOrAlaskaNativeOnly","Hispanic")]/100,0)
race.df$size = df$size
race.df$total = df$TOTAL_OCCU

race.df2 = ddply(race.df,
                .(size),
                summarize,
                total = sum(total),
                white = sum(WhiteOnly),
                black = sum(AfricanAmericanOnly),
                asian = sum(AsianOnly),
                native = sum(AmericanIndianOrAlaskaNativeOnly),
                hispanic = sum(Hispanic))

row.names(race.df2) = race.df2$size
race.df2 = race.df2[,-1]
total = apply(race.df2[,-1],2,sum)
total = 100*total/sum(race.df2$total)

race.df2 = 100*race.df2[,2:ncol(race.df2)]/race.df2$total
race.df2 = rbind(race.df2,total)
row.names(race.df2)[nrow(race.df2)] = "Total"

write.csv(race.df2,
          paste(tablesFolder, "table2phaByRaceAndSize.csv", sep=""))
##########

##########
# table of Age by pha size
df = merge(phaAge,
           pha,
           by.x = "pha_code",
           by.y="PARTICIPAN",
           suffixes = "")[c(paste("v",seq(2,12,by=2),sep=""),"size")]

df$total = apply(df[,1:6],1,sum)

age.df = ddply(df,
                 .(size),
                 summarize,
                 total = sum(total),
                 age_00_5 = sum(v2),
                age_06_17 = sum(v4),
                age_18_50 = sum(v6),
                age_51_61 = sum(v8),
                age_62_82 = sum(v10),
                age_83 = sum(v12))

row.names(age.df) = age.df$size
age.df = age.df[,-1]
total = apply(age.df[,-1],2,sum)
total = 100*total/sum(age.df$total)
age.df = 100*age.df[,2:ncol(age.df)]/age.df$total
age.df = rbind(age.df,total)
row.names(age.df)[nrow(age.df)] = "Total"
##########

##########
df = merge(allProgramsAge,
           pha,
           by.x = "pha_code",
           by.y="PARTICIPAN",
           suffixes = "")[c(paste("v",seq(2,12,by=2),sep=""),"plan_status", "units_elderly", "units_disabled", "units_mix", "FORMAL_PAR")]

df$elderly = apply(df[,c("v10","v12")], 1, sum)
df$pct_designated_elderly = 100*df$units_elderly/df$elderly



##########
# table of phas by financial status and plan status
phaByFin = ddply(pha,
                  .(PHAS_DESIG),
                  summarize,
                  authorities = length(PHAS_DESIG),
                 units = sum(TOTAL_UNIT),
                 active = sum(plan_status=="active"),
                 none = sum(plan_status=="none"))
phaByFin = phaByFin[c(2,3,4,5,1,6,7),]
phaByFin$PHAS_DESIG[nrow(phaByFin)] = "Not Available"
row.names(phaByFin) = phaByFin$PHAS_DESIG
phaByFin = phaByFin[,-1]
total = apply(phaByFin,2,sum)

# phaByFin[,3:ncol(phaByFin)] = 100*phaByFin[,3:ncol(phaByFin)]/phaByFin$authorities
phaByFin$active = 100*phaByFin$active/(sum(phaByFin$active))
phaByFin$none = 100*phaByFin$none/(sum(phaByFin$none))
total[3:length(total)] = 100
phaByFin = rbind(phaByFin,total)
row.names(phaByFin)[nrow(phaByFin)] = "Total"
##########

# "Describe prevalence of housing problems by 
# designated housing plan status"

# join pha to chas5 and compare housing problems 
# for elderly housing in pha counties with designated
# housing and in counties without designated housing
# df = merge(pha, chas16, by.x = "county10", by.y = "county10", suffixes = "")
# # compare rate of housing problems for 62+ hhs 
# # by income category for pha's based on designation status
# hpEld = ddply(df[df$units_elderly > 0 |
#                     df$units_mix > 0 |
#                     df$plan_status == "none",],
#                .(plan_status),
#                summarize,
#                eliEldhP_p = 100*sum(eliEldhP)/sum(eliEld),
#                vliEldhP_p = 100*sum(vliEldhP)/sum(vliEld),
#                liEldhP_p = 100*sum(liEldhP)/sum(liEld))
# compare rate of housing problems for 75+ hhs 
# by income category for pha's based on designation status
# hp_75T = ddply(df[df$units_elderly > 0 |
#                     df$units_mix > 0 |
#                     df$plan_status == "none",],
#                .(plan_status),
#                summarize,
#                eli75hp_p = 100*sum(T5_est6)/sum(eli_75),
#                vli75hp_p = 100*sum(T5_est10)/sum(vli_75),
#                li75hp_p = 100*sum(T5_est14)/sum(li_75))

# compare rate of housing problems for hhs with 
# person who has independent living limitation
# by income category for pha's based on 
# designation status

eliDisHp_p.active = 100*sum(chas6$T6_est55[chas6$county10 %in% countiesActive])/sum(chas6$T6_est54[chas6$county10 %in% countiesActive])
vliDisHp_p.active = 100*sum(chas6$T6_est59[chas6$county10 %in% countiesActive])/sum(chas6$T6_est58[chas6$county10 %in% countiesActive])
liDisHp_p.active = 100*sum(chas6$T6_est63[chas6$county10 %in% countiesActive])/sum(chas6$T6_est62[chas6$county10 %in% countiesActive])

eliDisHp_p.none = 100*sum(chas6$T6_est55[chas6$county10 %in% countiesNone])/sum(chas6$T6_est54[chas6$county10 %in% countiesNone])
vliDisHp_p.none = 100*sum(chas6$T6_est59[chas6$county10 %in% countiesNone])/sum(chas6$T6_est58[chas6$county10 %in% countiesNone])
liDisHp_p.none = 100*sum(chas6$T6_est63[chas6$county10 %in% countiesNone])/sum(chas6$T6_est62[chas6$county10 %in% countiesNone])

eliEldhP_p.active = 100*sum(chas16$eliEldhP[chas16$county10 %in% countiesActive])/sum(chas16$eliEld[chas16$county10 %in% countiesActive])
vliEldhP_p.active = 100*sum(chas16$vliEldhP[chas16$county10 %in% countiesActive])/sum(chas16$vliEld[chas16$county10 %in% countiesActive])
liEldhP_p.active = 100*sum(chas16$liEldhP[chas16$county10 %in% countiesActive])/sum(chas16$liEld[chas16$county10 %in% countiesActive])

eliEldhP_p.none = 100*sum(chas16$eliEldhP[chas16$county10 %in% countiesNone])/sum(chas16$eliEld[chas16$county10 %in% countiesNone])
vliEldhP_p.none = 100*sum(chas16$vliEldhP[chas16$county10 %in% countiesNone])/sum(chas16$vliEld[chas16$county10 %in% countiesNone])
liEldhP_p.none = 100*sum(chas16$liEldhP[chas16$county10 %in% countiesNone])/sum(chas16$liEld[chas16$county10 %in% countiesNone])

activeRow = c(eliEldhP_p.active,vliEldhP_p.active,liEldhP_p.active,eliDisHp_p.active,vliDisHp_p.active,liDisHp_p.active)
noneRow = c(eliEldhP_p.none,vliEldhP_p.none,liEldhP_p.none,eliDisHp_p.none,vliDisHp_p.none,liDisHp_p.none)

hp_disT = data.frame(rbind(activeRow, noneRow))
# 
# 
# 
# df = merge(pha, chas6, by.x = "county10", by.y = "county10", suffixes = "")
# hp_disT = ddply(df[df$units_disabled > 0 |
#                      df$units_mix > 0 |
#                      df$plan_status == "none",],
#                .(plan_status),
#                summarize,
#                eliDisHp_p = 100*sum(T6_est55)/sum(T6_est54),
#                vliDisHp_p = 100*sum(T6_est59)/sum(T6_est58),
#                liDisHp_p = 100*sum(T6_est63)/sum(T6_est62))
# 
# # combine hpEld and hp_disT
# hpEldDis = cbind(hpEld,hp_disT[,-1])
write.csv(hp_disT,
          paste(tablesFolder,
                "table6prevOfHousingProblemsByIncome.csv",
                sep = ""),
          row.names = F)

# Compare availability of other assisted housing units by availability
# of designated housing units
countiesActive = pha$county10[pha$plan_status == "active"]
countiesNone = pha$county10[pha$plan_status != "active"]

df.1 = ddply(multi_fam[multi_fam$IS_202_811 == "Y", ],
             .(county10),
             summarize,
             TOTAL_ASSI202 = sum(TOTAL_ASSI))

total_202.countiesActive = sum(df.1$TOTAL_ASSI202[df.1$county10 %in% countiesActive])
total_202_mean.countiesActive = total_202.countiesActive/length(countiesActive)
total_202.countiesNone = sum(df.1$TOTAL_ASSI202[df.1$county10 %in% countiesNone])
total_202_mean.countiesNone = total_202.countiesNone/length(countiesNone)



df.2 = ddply(multi_fam,
             .(county10),
             summarize,
             TOTAL_UNITall = sum(TOTAL_UNIT))

df.3 = ddply(hcv,
             .(county00),
             summarize,
             TOTAL_UNITall = sum(LI_UNITS))

multifam.countiesActive = sum(df.2$TOTAL_UNITall[df.2$county10 %in% countiesActive])
multifam_mean.countiesActive = multifam.countiesActive/length(countiesActive)
multifam.countiesNone = sum(df.2$TOTAL_UNITall[df.2$county10 %in% countiesNone])
multifam_mean.countiesNone = multifam.countiesNone/length(countiesNone)
hcv.countiesActive = sum(df.3$TOTAL_UNITall[df.3$county00 %in% countiesActive])
hcv_mean.countiesActive = hcv.countiesActive/length(countiesActive)
hcv.countiesNone = sum(df.3$TOTAL_UNITall[df.3$county00 %in% countiesNone])
hcv_mean.countiesNone = hcv.countiesNone/length(countiesNone)


total.countiesActive = sum(pha$TOTAL_UNIT[pha$plan_status == "active"])
total_mean.countiesActive = total.countiesActive/length(countiesActive)
total.countiesNone = sum(pha$TOTAL_UNIT[pha$plan_status != "active"])
total_mean.countiesNone = total.countiesNone/length(countiesNone)

activeRow = c(total.countiesActive,
              total_mean.countiesActive,
              total_202.countiesActive,
              total_202_mean.countiesActive,
              multifam.countiesActive,
              multifam_mean.countiesActive,
              hcv.countiesActive,
              hcv_mean.countiesActive)

noneRow = c(total.countiesNone,
            total_mean.countiesNone,
            total_202.countiesNone,
            total_202_mean.countiesNone,
            multifam.countiesNone,
            multifam_mean.countiesNone,
            hcv.countiesNone,
            hcv_mean.countiesNone)

dfFinal = as.data.frame(rbind(activeRow, noneRow))

write.csv(dfFinal,
          paste(tablesFolder,
                "table5assistedUnitsFromOtherPrograms.csv",
                sep = ""),
          row.names = F)

##########
# table of pha by plan status and QCT status
phaQCT = table(pha[,c("plan_status","qct_status")])

# table of pha by plan status and rural/urban status
df = merge(pha, pha_csa, by = "PARTICIPAN", suffixes = "")[,c(names(pha),"GEOID")]
df$urban_rural = ifelse(is.na(df$GEOID), "rural", "urban")
table(df[,c("plan_status","urban_rural")])
##########

##########
# calculate median crime violent/property crime rates 
# for counties with and without PHAs with active designation programs
dfActive = crime[crime$county10 %in% countiesActive,]
dfNone = crime[crime$county10 %in% countiesNone,]
activeVlntCrime = median(dfActive$vlntCrimePerCap, na.rm = T) # median violent crime active counties
noneVlntCrime = median(dfNone$vlntCrimePerCap, na.rm = T) # median violent crime none counties
activePrptyCrime = median(dfActive$prptyCrimePerCap, na.rm = T) # median property crime active counties
nonePrptyCrime = median(dfNone$prptyCrimePerCap, na.rm = T) # median property crime none counties


activeVlntCrime.g = g.b(dfActive,
                         x = "vlntCrimePerCap",
                         breaks = seq(from = 0, to = 800, by = 50),
                         xmin = 0,
                         xmax = 800,
                         ymax = 0.3,
                         title = "Active",
                         xlab = "Violent Crime Per Capita",
                         ylab = "% PHAs",
                         title_size = 14,
                         axis_text_size = 10,
                         axis_title_size = 10)

noneVlntCrime.g = g.b(dfNone,
                        x = "vlntCrimePerCap",
                        breaks = seq(from = 0, to = 800, by = 50),
                        xmin = 0,
                        xmax = 800,
                        ymax = 0.3,
                        title = "None",
                        xlab = "Violent Crime Per Capita",
                        ylab = "% PHAs",
                        title_size = 14,
                        axis_text_size = 10,
                        axis_title_size = 10)

activePrptyCrime.g = g.b(dfActive,
                        x = "prptyCrimePerCap",
                        breaks = seq(from = 0, to = 2000, by = 100),
                        xmin = 0,
                        xmax = 2000,
                        ymax = 0.3,
                        title = "Active",
                        xlab = "Violent Crime Per Capita",
                        ylab = "% PHAs",
                        title_size = 14,
                        axis_text_size = 10,
                        axis_title_size = 10)

nonePrptyCrime.g = g.b(dfNone,
                      x = "prptyCrimePerCap",
                      breaks = seq(from = 0, to = 2000, by = 100),
                      xmin = 0,
                      xmax = 2000,
                      ymax = 0.3,
                      title = "None",
                      xlab = "Violent Crime Per Capita",
                      ylab = "% PHAs",
                      title_size = 14,
                      axis_text_size = 10,
                      axis_title_size = 10)
# 
########## End Crime Section

########## Begin Racial/Ethnic Poverty Concentration Section
activeREPovPct = 100*sum(pha$plan_status == "active" & pha$ct10 %in% rEPov)/sum(pha$plan_status == "active")
noneREPovPct = 100*sum(pha$plan_status == "none" & pha$ct10 %in% rEPov)/sum(pha$plan_status == "none")
                                                                                                                                                        
# save work
save.image()








