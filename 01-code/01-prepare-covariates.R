####################
### INTRODUCTION ###
####################
# This script prepares all the input data needed to run the regression model
# Requires packages: haven, joyn, wbstats, lubridate, readr, dplyr, fastverse, readxl, countrycode, zoo
library(dplyr)
rm(list=ls())

####################################
### PREPARE REGION/INCGROUP DATA ###
####################################
# Start with the class file, downloaded from here: https://github.com/GPID-WB/Class/blob/master/OutputData/CLASS.dta
cls <- haven::read_dta("02-inputdata/CLASS.dta") |>
# Keep relevant columns 
select(code,year_data,region,incgroup_historical) |>
# Rename columns
rename(year = year_data,incgroup = incgroup_historical) |>
# Convert into format needed for imputation
mutate(eca = if_else(region=="Europe & Central Asia",1,0),
       lac = if_else(region=="Latin America & Caribbean",1,0),
       ssa = if_else(region=="Sub-Saharan Africa",1,0),
       lmc = if_else(incgroup=="Lower middle income",1,0),
       umc = if_else(incgroup=="Upper middle income",1,0),
       hic = if_else(incgroup=="High income",1,0),
# Indicator for whether we want to predict income or consumption distributions. Assume we want the former only for HICs
       inc = if_else(incgroup=="High income",1,0)) |>
select(-region,-incgroup)

#########################
### GDP DATA FROM WDI ###
#########################
# Get data from WDI
wdi <- wbstats::wb_data(indicator = c("NY.GDP.PCAP.PP.KD","NY.GDP.PCAP.KD"), lang = "en",country="countries_only") |>
# Rename columns
rename(code=iso3c,year=date,gdp=NY.GDP.PCAP.PP.KD,gdp_usd = NY.GDP.PCAP.KD) |>
# Keep relevant columns
select(code,year,gdp,gdp_usd) |>
# Keep relevant years
filter(between(year,1981, lubridate::year(Sys.Date()))) 

########################################
### GDP NOWCASTS FROM PIP REPOSITORY ###
########################################
# File from here: A newer version may not be public yet, reah out to the person responsible for the repository to be sure.
# https://github.com/PIP-Technical-Team/aux_nan/
gdp <- readr::read_csv("02-inputdata/nan.csv") |>
       filter(gdp_data_level=="national") |>
       select(country_code,year,gdppc_growth) |>
       rename(code=country_code) |>
       group_by(code) |>
       arrange(year) |>
       # Keep relevant years
       filter(between(year,1981, lubridate::year(Sys.Date()))) 

##############################
### CREATE FULL GDP SERIES ###
##############################
gdp <- joyn::joyn(wdi,gdp,match_type="1:1",by=c("code","year"),reportvar=FALSE) |>
       group_by(code) |>
       arrange(year) |>
       # The GDP growth values from the PIP repository are at t-1 (for example the 2020 COVID shock in India is in the 2019 cell). Move them up once:
       mutate(gdppc_growth = lag(gdppc_growth)) |>
       # Add more growth values when missing in PIP repository but present in WDI
       mutate(gdppc_growth = if_else(is.na(gdppc_growth),gdp_usd/lag(gdp_usd)-1,gdppc_growth))  |>
       ungroup() |>
       # Remove cases where we don't have any GDP data
       filter(!(is.na(gdp) & is.na(gdp_usd) & is.na(gdppc_growth))) |>
       arrange(code, year) %>%
       group_by(code) |>
       
       # This entire segment tries to change the growth column onto the series in PPP terms
       mutate(# Convert growth rate to growth factor
              growth_factor = 1 + gdppc_growth,
              # Calculate cumulative growth factor
              cum_growth = cumprod(tidyr::replace_na(growth_factor, 1)),
              # Identify first and last known gdp and their years
              gdp_first        = dplyr::first(na.omit(gdp)),
              year_first       = min(year[!is.na(gdp)], na.rm = TRUE),
              gdp_last         = dplyr::last(na.omit(gdp)),
              year_last        = max(year[!is.na(gdp)], na.rm = TRUE),
              # Calculate cumulative growth from first and last GDP observation 
              cum_growth_last  = dplyr::last(cum_growth[!is.na(gdp)]),
              cum_growth_first = dplyr::first(cum_growth[!is.na(gdp)]),
              # Forward extrapolation (after last known GDP)
              gdp_fwd = if_else(year > year_last,gdp_last * cum_growth / cum_growth_last,NA_real_),
              # Backward extrapolation (before first known GDP)
              gdp_bwd = if_else(year < year_first,gdp_first / (cum_growth_first / cum_growth),NA_real_ ),
              # Final combined series
              gdp2021 = coalesce(gdp, gdp_fwd, gdp_bwd)) |>
          
  ungroup() |>
  select(code,year,gdp2021)

###############################
### CONVERT TO 2017 PPP GDP ###
###############################
deltaratio <- readxl::read_excel("02-inputdata/delta_ratio_gdp.xlsx") |>
              select(country_code,delta) |>
              rename(code = country_code)

gdp <- joyn::joyn(gdp,deltaratio,by="code",match_type="m:1",reportvar=FALSE,keep="left") |>
       mutate(gdp2017 = gdp2021/delta) |>
       select(-delta)

rm(deltaratio)

###################################################
### DATA FROM WORLD BANK POPULATION PROJECTIONS ###
###################################################
# Get rural population share data (rps) from the World Bank Population Projections
# Data retrieved from https://databank.worldbank.org/source/population-estimates-and-projections#advancedDownloadOptions
# Select all countries
# Select series: "Rural population (% of total population)"
# Select time: all years
# Download options -> advanced options -> CSV -> Data format: list -> download
# Save file in 02-inputdata folder, rename file as ruralpopulationshare.csv
# Load data
wbp <- readr::read_csv("02-inputdata/ruralpopulationshare.csv")|> 
# Rename columns.
rename(code = "Country Code", year = Time, rps = Value) |>
# Keep relevant columns 
select(code,year,rps) |>
# Keep relevant years
filter(between(year,1981, lubridate::year(Sys.Date()))) |>
# Turn variable numeric
mutate(rps = as.numeric(rps))
         
###############################################
### DATA FROM UN WORLD POPULATION PROSPECTS ###
###############################################
# Get Under-5 mortality rates (u5m) and life expectancy at birth (leb) from UN WPP
# Download data from here:
#https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used
# Select the first file "Compact (most used: estimates and medium projections) (XLSX)"
# Save as wpp.xlsx in 02-inputdata folder
# Historical data from the UN
wpp_historical <- readxl::read_excel("02-inputdata/wpp.xlsx", sheet = "Estimates", skip = 16) 
# Projectiosn from the UN
wpp_projections <- readxl::read_excel("02-inputdata/wpp.xlsx", sheet = "Medium variant", skip = 16) 
# Combine
wpp <- rbind(wpp_historical,wpp_projections) |>
  # Rename relevant variables
  rename("year" = "Year", "iso3n" = "Location code","economy" = "Region, subregion, country or area *",
         "leb" = "Life Expectancy at Birth, both sexes (years)",
         "u5m" = "Under-Five Mortality (deaths under age 5 per 1,000 live births)",
         "pop" = "Total Population, as of 1 July (thousands)") |>
  select(iso3n,economy,year,leb,u5m,pop) |>
  mutate(code = countrycode::countrycode(iso3n, origin = 'iso3n', destination = 'iso3c')) |>
  # Check all got converted, Kosovo did not       
  mutate(code = if_else(iso3n==412,"XKX",code)) |>
  filter(!is.na(code)) |>
  # Turn variables numeric
  mutate(leb = as.numeric(leb),
         u5m = as.numeric(u5m),
         pop = as.numeric(pop)) |>
  filter(between(year,1981, lubridate::year(Sys.Date()))) |>
  #In WPP, Channel Islands appears as Jersey and Guernsey. Take the weighted average 
  mutate(code = if_else(code %in% c("JEY","GGY"),"CHI",code)) |>
  group_by(code,year) |>
  summarize(u5m = weighted.mean(u5m,pop),
            leb = weighted.mean(leb,pop)) |>
  ungroup() |>
  select(code,year,leb,u5m) 
rm(wpp_historical,wpp_projections)

#############
### MERGE ###
#############
covariates <- expand.grid(year=seq(1981,lubridate::year(Sys.Date()),1),code=unique(cls$code)) |>
        joyn::joyn(cls,match_type="1:1",by=c("code","year"),reportvar=FALSE,keep="left") |>
        joyn::joyn(gdp,match_type="1:1",by=c("code","year"),reportvar=FALSE,keep="left") |>
        joyn::joyn(wbp,match_type="1:1",by=c("code","year"),reportvar=FALSE,keep="left") |>
        joyn::joyn(wpp,match_type="1:1",by=c("code","year"),reportvar=FALSE,keep="left")

# Fill missings
covariates <- covariates |>
        group_by(code) |>
        arrange(year) |>
        # Assume inc groups don't change before first value and after last value. Same for regions, which obviously are time-invariant. 
        mutate(across(c(eca,lac,ssa,lmc,umc,hic,inc),~ zoo::na.approx(.x,maxgap = Inf, rule = 2))) |>
        ungroup() |>
        # TWN, XKX, and MAF miss rural population shares. Use neighboring countries as back up
        mutate(newcode = code,
               newcode = if_else(code %in% c("TWN","CHN"),"TWN_CHN",newcode),
               newcode = if_else(code %in% c("XKX","SRB"),"XKX_SRB",newcode),
               newcode = if_else(code %in% c("MAF","SXM"),"MAF_SXM",newcode)) |>
        group_by(newcode,year) |>
        mutate(rps = coalesce(rps,mean(rps,na.rm=TRUE))) |>
        ungroup() |>
        select(-newcode)

# Save final file
save(covariates,file="03-intermediatedata/covariates.Rda")

rm(cls,gdp,wbp,wdi,wpp)
