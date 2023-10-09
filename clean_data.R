library(tidyverse)
library(scico)
library(reshape2)

setwd("~/03_Outreach/dataviz/konstanz")

#this script contains processing steps to bring the raw data in the format for
# the analysis. THIS ANALYSIS ALSO INCLUDED MANUALLY ADDITION OF OF DATA, SO BE
#SURE TO THINK VERY CAREFULLY ABOUT DELITING FILES, E.G.names_clean.csv


df = read.csv("data/raw/raw_data.csv", sep = ";", check.names = F)

long_names = names(df)
write.csv(long_names, "data/processed/column_names.csv")


#lookup table was created manually

lookup_names = read.csv("data/processed/names_clean.csv", header = TRUE)

names(df) = lookup_names$kurz

write.csv(df, "data/processed/data_renamed.csv")
