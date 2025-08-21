####################
### INTRODUCTION ###
####################
# This script runs the regression that estimates the parameters needed to predict full distributions

library(dplyr)
rm(list=ls())

####################
### PREPARE DATA ###
####################
# Load most recent 100-binned survey data in 2021 PPPs. 
welfare2021 <- haven::read_dta("P:/03.pip/estimates/percentiles/20250401_2021_01_02_PROD/world_100bin.dta") |>
               filter(reporting_level==1 | country_code=="ARG") |>
               rename(code = country_code) |>
               # Rework the percentile value to it fits with the method of the paper
               mutate(pct = (percentile-0.5)/100,
                       pct = log(pct/(1-pct))) |>
               select(year,code,avg_welfare,welfare_type,pct) 
 
# Load most 100-binned survey data in 2021 PPPs
welfare2017 <- haven::read_dta("P:/03.pip/estimates/percentiles/20250401_2017_01_02_PROD/world_100bin.dta") |>
               filter(reporting_level==1 | country_code=="ARG") |>
               rename(code = country_code) |>
               # Rework the percentile value to it fits with the method of the paper
               mutate(pct = (percentile-0.5)/100,
                      pct = log(pct/(1-pct))) |>
               select(year,code,avg_welfare,welfare_type,pct) 

# Add welfare time which isn't in the binned data but is needed to properly merge on the covariates
welfare_time <- pipr::get_stats() |>
                filter(reporting_level=="national" | country_code=="ARG") |>
                select(country_code,year,welfare_time,welfare_type) |>
                rename(code=country_code)

# For some reason NGA 2022 is in the binned data, it shouldn't be, removing it by selecting keep="inner".
welfare2017 <- joyn::joyn(welfare2017,welfare_time, by=c("code","year","welfare_type"), match_type="m:1",keep="inner",reportvar=FALSE)
welfare2021 <- joyn::joyn(welfare2021,welfare_time, by=c("code","year","welfare_type"), match_type="m:1",keep="inner",reportvar=FALSE)


# Calculate covariates at survey years
load("03-intermediatedata/covariates.Rda")
covariates_svy <- welfare_time |>
                  select(-welfare_type) |>
                  distinct() |>
                  rename(reporting_year = year, year = welfare_time) |>
                  joyn::joyn(covariates,match_type="1:1",by=c("code","year"),reportvar=FALSE) |>
                  group_by(code) |>
                  arrange(year) |>
                  mutate(
                         # For gdp, rps, leb, u5m, take the weighted average using of the preceding and proceding estimates
                         gdp2021 = coalesce(gdp2021,lag(gdp2021)*(lead(year)-year)+lead(gdp2021)*(year-lag(year))),
                         rps = coalesce(rps,lag(rps)*(lead(year)-year)+lead(rps)*(year-lag(year))),
                         leb = coalesce(leb,lag(leb)*(lead(year)-year)+lead(leb)*(year-lag(year))),
                         u5m = coalesce(u5m,lag(u5m)*(lead(year)-year)+lead(u5m)*(year-lag(year))),
                         # For regions, no change, just take the preceeding value
                         eca = coalesce(eca,lag(eca)),
                         lac = coalesce(lac,lag(lac)),
                         ssa = coalesce(ssa,lag(ssa)),
                         # For income group, take the value closest to the welfare time
                         lmc = coalesce(lmc,if_else(year-reporting_year<=0.5,lag(lmc),lead(lmc))),
                         umc = coalesce(umc,if_else(year-reporting_year<=0.5,lag(umc),lead(umc))),
                         hic = coalesce(hic,if_else(year-reporting_year<=0.5,lag(hic),lead(hic))),
                         inc = coalesce(inc,if_else(year-reporting_year<=0.5,lag(inc),lead(inc)))) |>
                 ungroup() |>
                 filter(!is.na(reporting_year)) |>
                 rename(welfare_time = year, year = reporting_year)

  
# Merge welfare data to create final data for regressions
data2021 <- joyn::joyn(welfare2021,covariates_svy,match_type="m:1",by=c("code","welfare_time"),reportvar=FALSE) |>
                  filter(year>=1981) |>
            # Create weights for regressions, each country gets the same influence
            group_by(code) |>
            mutate(weight1 = 100/length(gdp2021),
                   weight2 = 100/length(na.omit(gdp2021)),
                   weight2 = if_else(weight2>1,0,weight2)) |>
            ungroup()

data2017 <- joyn::joyn(welfare2017,covariates_svy,match_type="m:1",by=c("code","welfare_time"),reportvar=FALSE) |>
            filter(year>=1981) |>
            # Create weights for regressions, each country gets the same influence
            group_by(code) |>
            mutate(weight1 = 100/length(gdp2017),
                   weight2 = 100/length(na.omit(gdp2017)),
                   weight2 = if_else(weight2>1,0,weight2)) |>
            ungroup()

rm(covariates,covariates_svy,welfare2017,welfare2021,welfare_time)


#######################
### RUN REGRESSIONS ###
#######################
tier1_2021ppp <- quantreg::rq(log(avg_welfare) ~ log(u5m)+leb+rps+inc*log(gdp2021)+            pct+pct:eca+pct:lac+pct:ssa, weight=weight1, data=data2021)
tier2_2021ppp <- quantreg::rq(log(avg_welfare) ~ log(u5m)+leb+rps+                 lmc+umc+hic+pct+pct:eca+pct:lac+pct:ssa, weight=weight2, data=data2021)
tier1_2017ppp <- quantreg::rq(log(avg_welfare) ~ log(u5m)+leb+rps+inc*log(gdp2017)+            pct+pct:eca+pct:lac+pct:ssa, weight=weight1, data=data2017)
tier2_2017ppp <- quantreg::rq(log(avg_welfare) ~ log(u5m)+leb+rps+                 lmc+umc+hic+pct+pct:eca+pct:lac+pct:ssa, weight=weight2, data=data2017)

# Calculate smearing factor
tier1_2021ppp_smearing <- exp(mean(tier1_2021ppp$residuals))
tier2_2021ppp_smearing <- exp(mean(tier2_2021ppp$residuals))
tier1_2017ppp_smearing <- exp(mean(tier1_2017ppp$residuals))
tier2_2017ppp_smearing <- exp(mean(tier2_2017ppp$residuals))

# Store results
tier1_2021ppp <- as.data.frame(tier1_2021ppp$coefficients)
tier2_2021ppp <- as.data.frame(tier2_2021ppp$coefficients)
tier1_2017ppp <- as.data.frame(tier1_2017ppp$coefficients)
tier2_2017ppp <- as.data.frame(tier2_2017ppp$coefficients)

# Add smearing factor to stored results
tier1_2021ppp <- rbind(tier1_2021ppp, tier1_2021ppp_smearing)
tier2_2021ppp <- rbind(tier2_2021ppp, tier2_2021ppp_smearing)
tier1_2017ppp <- rbind(tier1_2017ppp, tier1_2017ppp_smearing)
tier2_2017ppp <- rbind(tier2_2017ppp, tier2_2017ppp_smearing)
rownames(tier2_2021ppp)[nrow(tier2_2021ppp)] <- "smearing"
rownames(tier1_2021ppp)[nrow(tier1_2021ppp)] <- "smearing"
rownames(tier2_2017ppp)[nrow(tier2_2017ppp)] <- "smearing"
rownames(tier1_2017ppp)[nrow(tier1_2017ppp)] <- "smearing"

##########################
### SAVE FINAL RESULTS ###
##########################
save(tier1_2021ppp,file="03-intermediatedata/tier1_2021ppp.Rda")
save(tier2_2021ppp,file="03-intermediatedata/tier2_2021ppp.Rda")
save(tier1_2017ppp,file="03-intermediatedata/tier1_2017ppp.Rda")
save(tier2_2017ppp,file="03-intermediatedata/tier2_2017ppp.Rda")