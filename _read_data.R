library(foreign) # import data from less common formats (like dbf)

# import data on pha by type (ie size and financial status)
pha = read.dbf("../spatial/assisted_housing/output/pha.dbf",
               as.is = T) # don't convert character variables to R factors

# import data on pha by location in Combined Statistical Area (csa)
pha_csa = read.dbf("../spatial/assisted_housing/raw/pha_csa.dbf",
                  as.is = T)

# import Qualified Census Tracts
qct = read.dbf("../spatial/assisted_housing/raw/QCT2013dbf/QCT2013.DBF",
               as.is = T)

# import data on pha designation status
pha_designation = read.csv("../tabular/pha_designation_status/output/pha_designation_status.csv",
                           as.is = T)

# import rcr data on pha population by race and ethnicity
phaRace = read.csv("../tabular/rcr/Public housing/Public housing. final spreadsheets/10.csv",
                   as.is = T)
phaEthnicity = read.csv("../tabular/rcr/Public housing/Public housing. final spreadsheets/11.csv",
                        as.is = T)
phaAge = read.csv("../data/tabular/rcr/Public housing/Public housing. final spreadsheets/12.csv",
                  as.is = T)

phaDisabled = read.csv("../tabular/rcr/Public housing/Public housing. final spreadsheets/8.csv",
                  as.is = T)

allProgramsAge = read.csv("../tabular/rcr/All relevant programs/All relevant programs. final spreadsheets/12.csv",
                      as.is = T)

allProgramsDisabled = read.csv("../tabular/rcr/All relevant programs/All relevant programs. final spreadsheets/8.csv",
                               as.is = T)

allProgramsRace = read.csv("../tabular/rcr/All relevant programs/All relevant programs. final spreadsheets/10.csv",
                               as.is = T)

allProgramsEthnicity = read.csv("../tabular/rcr/All relevant programs/All relevant programs. final spreadsheets/11.csv",
                           as.is = T)

# import data on multifamily_housing
multi_fam = read.dbf("../spatial/assisted_housing/output/multifamily_properties.dbf",
                     as.is = T)

# import data on lihtc
lihtc = read.dbf("../spatial/assisted_housing/output/lihtc_county.dbf",
                     as.is = T)

# import data on hcv's
hcv = read.dbf("../spatial/assisted_housing/output/lihtc_county.dbf",
               as.is = T)

# import chas 2006 - 2010 data on housing problems for 
# elderly households 
chas5 = read.csv("../tabular/chas/2006_2010/counties/Table5.csv",
                 as.is = T)

# import chas 2008 - 2010 data on disabled households
chas6 = read.csv("../tabular/chas/2008_2010/counties/Table6.csv",
                 as.is = T)

# import chas 2006 - 2010 data on elderly households 
chas16 = read.csv("../tabular/chas/2006_2010/counties/Table16.csv",
                  as.is = T)

# import chas 2006 - 2010 data on households by race
chas1 = read.csv("../tabular/chas/2006_2010/counties/Table1.csv",
                 as.is = T)

# import chas 2000 data on elderly households at state-level
chas2_2000 = read.dbf("../tabular/chas/2000/states/A2A/A2A040r.dbf",
                      as.is = T)

# import chas 2006 - 2010 data on elderly households at state-level
chas16_state = read.csv("../tabular/chas/2006_2010/states/Table16.csv",
                        as.is = T)

# import chas 2008 - 2010 data on disabled households at state-level
chas6_state = read.csv("../tabular/chas/2008_2010/states/Table6.csv",
                        as.is = T)

# import chas 2000 data on disabled households at state-level
chas7_2000 = read.dbf("../tabular/chas/2000/states/A7A/A7A040r.dbf",
                      as.is = T)

# import Smart Location Data (SLD)
sld = read.dbf("../spatial/SmartLocationDb/SLD_shapefile/SmartLocationDb.dbf",
               as.is = T)

# import federally-qualified community health center data
chc = read.csv("../tabular/HCCs_and_Lookalikes/HCCs.txt",
               sep = "|",
               header = F,
               as.is = T)

namesCHC = read.csv("../tabular/HCCs_and_Lookalikes/HCC_Download_Template_Names.csv",
                    sep = ",",
                    header = F,
                    as.is = T)
names(chc) = namesCHC[,1]
rm(namesCHC)
names(chc) = gsub("^\\s+|\\s+$", "", names(chc))

# import crime 
crime = read.table("../tabular/crime/ICPSR_34582/ICPSR_34582/DS0001/34582-0001-Data.tsv",
                   sep = "\t",
                   header = T)

# import race/ethnic concentrations of poverty
rEPov = read.csv("../tabular/raceEthnicConcentrationPoverty/raceEthnicConcentrationPoverty.csv",
                 as.is = T,
                 header = F)




