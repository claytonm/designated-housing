library(Hmisc)      # typesetting table 
library(plyr)
library(ggplot2)    # drawing graphs
library(extrafont)  # custom fonts
library(knitr)      # combine (knit) R and LaTex 
library(gridExtra)  # arrange graphs in grids
library(reshape2)   # converting data between wide/long format

# load system fonts
Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.10/bin/gswin64c.exe")

source("_analyze.R")  # run analysis
source("_graphs.R")   # make graphs
# set text width in final document to 5 inches  
textwidth = 5

# compile .Rnw to .pdf file; 
# creates knit.pdf in working director
knit2pdf("knit.Rnw", compiler = "lualatex")