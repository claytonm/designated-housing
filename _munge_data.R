# Step 1 in Research Plan for HUD HIA

# "Assess the number of PHAs with and without active or 
# explored plans by size and PHA type":
# create a designation status column (plan_status) 
# in df pha to see how designation is related
# to pha attributes like size and financial status

# Note: there are 33 pha codes in pha_designation
# that are not found in pha table; also there are 
# 48 pha codes in pha_designation that appear more
# than once (at most three times)

# begin
# clean pha_code column in pha_designation
pha_designation$pha_code = ifelse(pha_designation$pha_code == "PA01", "PA010",
                                  ifelse(pha_designation$pha_code == "OH12", "OH003",
                                         ifelse(pha_designation$pha_code == "MI28", "MI003",
                                                ifelse(pha_designation$pha_code == "TN035", "TN125",
                                                       ifelse(pha_designation$pha_code == "AZ20", "AZ001",
                                                              ifelse(pha_designation$pha_code == "MO36", "MO13",
                                                                     ifelse(pha_designation$pha_code == "GAA02", "GA002",
                                                                            pha_designation$pha_code)))))))


active_pha_code = unique(pha_designation$pha_code[pha_designation$plan_status == "active"])
pha_designation = pha_designation[pha_designation$plan_status == "active",]
# end

# add plan status ("active","none") to pha
pha$plan_status = ifelse(pha$PARTICIPAN %in% active_pha_code, "active", "none")
# add number of elderly-dedicated units to pha
units_elderly = vector(length = nrow(pha))
for (i in 1:nrow(pha)){
  # for each row in pha, assign value of units_elderly in pha_designation
  # that corresponds to the pha
  units_elderly[i] = pha_designation$units_elderly[pha_designation$pha_code == pha$PARTICIPAN[i]][1]
}
pha$units_elderly = units_elderly
pha$units_elderly = ifelse(is.na(pha$units_elderly), 0, pha$units_elderly)
pha$units_elderly = ifelse(pha$plan_status == "none", 0, pha$units_elderly)
# add number of units that are disabled-dedicated to pha
units_disabled = vector(length = nrow(pha))
for (i in 1:nrow(pha)){
  # for each row in pha, assign value of units_disabled in pha_designation
  # that corresponds to the pha
  units_disabled[i] = pha_designation$units_disabled[pha_designation$pha_code == pha$PARTICIPAN[i]][1]
}
pha$units_disabled = units_disabled
pha$units_disabled = ifelse(is.na(pha$units_disabled), 0, pha$units_disabled)
pha$units_disabled = ifelse(pha$plan_status == "none", 0, pha$units_disabled)
# add number of units that are mixed-dedicated to pha
units_mix = vector(length = nrow(pha))
for (i in 1:nrow(pha)){
  # for each row in pha, assign value of plan_status in pha_designation
  # that corresponds to the pha
  units_mix[i] = pha_designation$units_mix[pha_designation$pha_code == pha$PARTICIPAN[i]][1]
}
pha$units_mix = units_mix
pha$units_mix = ifelse(is.na(pha$units_mix), 0, pha$units_mix)
pha$units_mix = ifelse(pha$plan_status == "none", 0, pha$units_mix)

# write dbf to map PHAs by plan status
write.dbf(pha[pha$plan_status == "active",
              c("PARTICIPAN", "units_disabled", "units_mix", "units_elderly")],
          "../spatial/assisted_housing/output/pha_designation.dbf")

# create variable that indicates pha size
# <100, 100-500, 501-1,000, 1,001-3,000, 3,001-7,500, and 7,500+
pha$size = cut(pha$TOTAL_UNIT,
               breaks = c(0, 100, 500, 1000, 3000, 7500, max(pha$TOTAL_UNIT)),
               labels = c("<100", "101-500", "501-1,000", "1,001-3,000","3,001-7,500","7,501 +"))

# create 2010 Tract ID for pha
pha$ct10 = paste(pha$STATE2KX, pha$CNTY2KX, pha$TRACT2KX, sep = "")

# add qct to to pha (indicator if pha is in a qct)
qct_status = vector(length = nrow(pha)) # status vector
for (i in 1:nrow(pha)){
     # for each row in pha, assign value of plan_status in pha_designation
     # that corresponds to the pha
     qct_status[i] = ifelse(pha$ct10[i] %in% qct$FIPS, "QCT", "Not QCT")
}
pha$qct_status = qct_status

# compare pha by size and race and Ethnicity
# create pha code for phaRace and phaEthnicity
phaRace$pha_code = sapply(strsplit(phaRace$HA,
                                   " - ",),
                          "[",
                          1)
phaEthnicity$pha_code = sapply(strsplit(phaEthnicity$HA,
                                   " - ",),
                          "[",
                          1)
phaAge$pha_code = sapply(strsplit(phaAge$v1,
                                        " - ",),
                               "[",
                               1)
phaDisabled$pha_code = sapply(strsplit(phaDisabled$v1,
                                  " - ",),
                         "[",
                         1)


allProgramsAge$pha_code = sapply(strsplit(allProgramsAge$v1,
                                          " - ",),
                                 "[",
                                 1)
allProgramsDisabled$pha_code = sapply(strsplit(allProgramsDisabled$v1,
                                          " - ",),
                                 "[",
                                 1)
allProgramsRace$pha_code = sapply(strsplit(allProgramsRace$v1,
                                               " - ",),
                                      "[",
                                      1)
allProgramsEthnicity$pha_code = sapply(strsplit(allProgramsEthnicity$v1,
                                           " - ",),
                                  "[",
                                  1)

# "Describe prevalence of housing problems by 
# designated housing plan status"
# create consistent county id field to join pha 
# to chas data
pha$county10 = paste(pha$STATE2KX, pha$CNTY2KX, sep = "")
chas5$county10 = substr(chas5$geoid, start = 8, stop = 12)
chas6$county10 = substr(chas6$geoid, start = 8, stop = 12)
chas16$county10 = substr(chas16$geoid, start = 8, stop = 12)
chas1$county10 = substr(chas1$geoid, start = 8, stop = 12)

# get total eli, vli, and li households with
# occupants between 62-74
cols = paste("T5_est", c("5", "26"), sep = "")
chas5$eli_62 = apply(chas5[,cols], 1, sum)
cols = paste("T5_est", c("9", "30"), sep = "")
chas5$vli_62 = apply(chas5[,cols], 1, sum)
cols = paste("T5_est", c("13", "34"), sep = "")
chas5$li_62 = apply(chas5[,cols], 1, sum)

# get total eli, vli, and li households with
# occupants 75+
cols = paste("T5_est", c("6", "27"), sep = "")
chas5$eli_75 = apply(chas5[,cols], 1, sum)
cols = paste("T5_est", c("10", "31"), sep = "")
chas5$vli_75 = apply(chas5[,cols], 1, sum)
cols = paste("T5_est", c("14", "35"), sep = "")
chas5$li_75 = apply(chas5[,cols], 1, sum)

# get total elderly househoolds that are eli, vli, and li
cols = paste("T16_est",c("4","16","89","101"),sep = "")
chas16$eliEld = apply(chas16[,cols], 1, sum)
cols = paste("T16_est",c("25","41","110","122"),sep = "")
chas16$vliEld = apply(chas16[,cols], 1, sum)
cols = paste("T16_est",c("46","62","131","143"),sep = "")
chas16$liEld = apply(chas16[,cols], 1, sum)

# get total elderly househoolds that are eli, vli, and li
# and have a housing problem
cols = paste("T16_est", 1 + c(4, 16, 89, 101),sep = "")
chas16$eliEldhP = apply(chas16[,cols], 1, sum)
cols = paste("T16_est", 1 + c(25, 41, 110, 122),sep = "")
chas16$vliEldhP = apply(chas16[,cols], 1, sum)
cols = paste("T16_est", 1 + c(46, 62, 131, 143),sep = "")
chas16$liEldhP = apply(chas16[,cols], 1, sum)

## get total Black and Hispanic households that are eli with hp
cols = paste("T1_est", c(6, 130), sep = "")
chas1$eli_aa = apply(chas1[,cols], 1, sum)
cols = paste("T1_est", 4 + c(6, 130), sep = "")
chas1$eli_hisp = apply(chas1[,cols], 1, sum)
## get total Black and Hispanic households that are vli with hp
cols = paste("T1_est", 8 + c(6, 130), sep = "")
chas1$vli_aa = apply(chas1[,cols], 1, sum)
cols = paste("T1_est", 8 + 4 + c(6, 130), sep = "")
chas1$vli_hisp = apply(chas1[,cols], 1, sum)


# "Calculate number and proportion of units designated for elderly,
# disabled or mixed over time"

# replace NA in pha_designation columns units_elderly, units_disabled,
# and units_mix with 0s
pha_designation$units_elderly = ifelse(is.na(pha_designation$units_elderly),
                                       0,
                                       pha_designation$units_elderly)
pha_designation$units_disabled = ifelse(is.na(pha_designation$units_disabled),
                                        0,
                                        pha_designation$units_disabled)
pha_designation$units_mix = ifelse(is.na(pha_designation$units_mix),
                                   0,
                                   pha_designation$units_mix)

# To investigate trend in designated units over time:
# convert date_rec into date_rec_year
# conver date_ren into date_ren_year
pha_designation$date_rec_year = sapply(strsplit(pha_designation$date_rec,
                                         "/",),
                                       "[",
                                       3)
pha_designation$date_ren_year = sapply(strsplit(pha_designation$date_ren,
                                                "/",),
                                       "[",
                                       3)

# compare availability of other assisted housing units by
# availability of designated housing units
multi_fam$county10 = paste(multi_fam$STATE2KX,
                           multi_fam$CNTY2KX,
                           sep = "")
lihtc$county10 = lihtc$GEOID

hcv$county00 = paste(hcv$STATE2K,
                     hcv$CNTY2K,
                     sep = "")

## get total households in All Relevant Programs
cols = paste("v",seq(from = 2, to = 16, by = 2), sep = "")
allProgramsDisabled$totalHHs = apply(allProgramsDisabled[,cols], 1, sum)
phaDisabled$totalHHs = apply(phaDisabled[,cols], 1, sum)
cols = paste("v",seq(from = 4, to = 16, by = 4), sep = "")
allProgramsDisabled$hhWithChildren = apply(allProgramsDisabled[,cols], 1, sum)


###########
# compare elderly cost burden in 2000 with 2006 - 2010
# at state-level
# calc total elderly households
cols = paste("A2AC",c(1,4),sep = "")
chas2_2000$eldCbEli = apply(chas2_2000[,cols], 1, sum)
cols = c(1,4,6,9,11,14,16,19,21,24)
cols = c(cols, 25 + cols)
cols = paste("A2AC", cols, sep = "")
chas2_2000$eld = apply(chas2_2000[,cols], 1, sum)
chas2_2000$eldCbEli_pct = 100*chas2_2000$eldCbEli/chas2_2000$eld

# prepare 2006 - 2010 elderly household data
chas16_state$state_code = substr(chas16_state$geoid,
                                 start = 8,
                                 stop = 9)
cols = c(4, 16, 25, 37, 46, 58, 67, 79)
cols = c(cols, 85 + cols)
cols = paste("T16_est", cols, sep = "")
chas16_state$eld = apply(chas16_state[,cols], 1, sum)
cols = c(5, 17)
cols = c(cols, 85 + cols)
cols = paste("T16_est", cols, sep = "")
chas16_state$eldCbEli = apply(chas16_state[,cols], 1, sum)
chas16_state$eldCbEli_pct = 100*chas16_state$eldCbEli/chas16_state$eld

## join chas16_state and chas2_2000 and export for map
df = merge(chas16_state,
           chas2_2000,
           by.x = "state_code",
           by.y = "SUM040",
           suffixes = c("2010", "2000"))[,c("state_code",
                                            "eldCbEli_pct2010",
                                            "eldCbEli_pct2000")]

df$pctChange = 100*(df$eldCbEli_pct2010 - df$eldCbEli_pct2000)/df$eldCbEli_pct2000

##########
###########
# compare disabled housing problems in 2000 with 2008 - 2010
# at state-level
chas6_state$state_code = substr(chas6_state$geoid,
                                 start = 8,
                                stop = 9)

chas6_state$dis = chas6_state$T6_est53
chas6_state$disCbEli = chas6_state$T6_est55
chas6_state$disEliCb_pct = 100*chas6_state$disCbEli/chas6_state$dis

cols = 1:30
cols = paste("A7AC", cols,sep = "")
chas7_2000$dis = apply(chas7_2000[,cols],
                       1, sum)
cols = c(1, 21)
cols = paste("A7AC", cols,sep = "")
chas7_2000$disEliCb = apply(chas7_2000[,cols],
                            1, sum)
chas7_2000$disEliCb_pct = 100*chas7_2000$disEliCb/chas7_2000$dis

df = merge(chas6_state,
           chas7_2000,
           by.x = "state_code",
           by.y = "SUM040",
           suffixes = c("2010", "2000"))[,c("state_code",
                                            "disEliCb_pct2010",
                                            "disEliCb_pct2000")]
df$pctChange = 100*(df$disEliCb_pct2010 - df$disEliCb_pct2000)/df$disEliCb_pct2000

##### crime
# create valid county id field for crime df
df = crime
df$FIPS_ST = as.character(df$FIPS_ST)
df$FIPS_CTY = as.character(df$FIPS_CTY)
df$FIPS_ST = leftPad(df$FIPS_ST, l = 2, pad = "0")
df$FIPS_CTY = leftPad(df$FIPS_CTY, l = 3, pad = "0")
df$county10 = paste(df$FIPS_ST, df$FIPS_CTY, sep = "")
crime = df
# calculate total crime incidennts per 100,000 capita
crime$crimePerCap = 1e5*crime$P1TOT/crime$CPOPARST
crime$vlntCrimePerCap = 1e5*crime$P1VLNT/crime$CPOPARST
crime$prptyCrimePerCap = 1e5*crime$P1PRPTY/crime$CPOPARST
#####

#####
# Race/Ethnic Concentration of Poverty
df = rEPov
names(df)[1] = "ct10"
df = df[grep("tract_id:", df$ct10),]
df = substr(df,
            start = nchar(df) - 10,
            stop = nchar(df))
rEPov = df
write.csv(rEPov, "../tabular/raceEthnicConcentrationPoverty/output/raceEthnicConcentrationPoverty.csv",
          row.names = F)










        