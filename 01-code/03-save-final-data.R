####################
### INTRODUCTION ###
####################
# This script saves the parameter estimates in the format needed for further processing in the pipeline
library(dplyr)
rm(list=ls())

load("03-intermediatedata/covariates.Rda")
load("03-intermediatedata/tier1_2021ppp.Rda")
load("03-intermediatedata/tier2_2021ppp.Rda")
load("03-intermediatedata/tier1_2017ppp.Rda")
load("03-intermediatedata/tier2_2017ppp.Rda")

##############################
### PREPARE WITH 2021 PPPs ###
##############################
CF21 <- data.table::as.data.table(covariates)

CF21[, `:=`(
  
  # Tier 1 component 1
  t1_comp1 = tier1_2021ppp["(Intercept)",] + 
             tier1_2021ppp["log(gdp2021)",]*log(gdp2021) + 
             tier1_2021ppp["log(u5m)",]*log(u5m) + 
             tier1_2021ppp["rps",]*rps +
             tier1_2021ppp["leb",]*leb + 
             tier1_2021ppp["inc",]*inc +
             tier1_2021ppp["inc:log(gdp2021)",]*inc*log(gdp2021),
  
  # Tier 1 quantile factor
  t1_qf = tier1_2021ppp["pct",] + 
          tier1_2021ppp["pct:eca",]*eca+
          tier1_2021ppp["pct:lac",]*lac+
          tier1_2021ppp["pct:ssa",]*ssa,
  
  # Tier 2 component 1
  t2_comp1 = tier2_2021ppp["(Intercept)",] + 
             tier2_2021ppp["log(u5m)",] * log(u5m) + 
             tier2_2021ppp["rps",] * rps + 
             tier2_2021ppp["leb",] *leb + 
             tier2_2021ppp["lmc",] * lmc + 
             tier2_2021ppp["umc",] * umc + 
             tier2_2021ppp["hic",] * hic,
  
  # Tier 2 quantile factor
  t2_qf = tier2_2021ppp["pct",] + 
          tier2_2021ppp["pct:eca",]*eca+
          tier2_2021ppp["pct:lac",]*lac+
          tier2_2021ppp["pct:ssa",]*ssa
  
)]

CF21 <- CF21[, .(code, year, t1_comp1, t1_qf, t2_comp1, t2_qf)]



##############################
### PREPARE WITH 2017 PPPs ###
##############################
CF17 <- data.table::as.data.table(covariates)

CF17[, `:=`(
  
  # Tier 1 component 1
  t1_comp1 = tier1_2017ppp["(Intercept)",] + 
    tier1_2017ppp["log(gdp2017)",]*log(gdp2017) + 
    tier1_2017ppp["log(u5m)",]*log(u5m) + 
    tier1_2017ppp["rps",]*rps +
    tier1_2017ppp["leb",]*leb + 
    tier1_2017ppp["inc",]*inc +
    tier1_2017ppp["inc:log(gdp2017)",]*inc*log(gdp2017),
  
  # Tier 1 quantile factor
  t1_qf = tier1_2017ppp["pct",] + 
    tier1_2017ppp["pct:eca",]*eca+
    tier1_2017ppp["pct:lac",]*lac+
    tier1_2017ppp["pct:ssa",]*ssa,
  
  # Tier 2 component 1
  t2_comp1 = tier2_2017ppp["(Intercept)",] + 
    tier2_2017ppp["log(u5m)",] * log(u5m) + 
    tier2_2017ppp["rps",] * rps + 
    tier2_2017ppp["leb",] *leb + 
    tier2_2017ppp["lmc",] * lmc + 
    tier2_2017ppp["umc",] * umc + 
    tier2_2017ppp["hic",] * hic,
  
  # Tier 2 quantile factor
  t2_qf = tier2_2017ppp["pct",] + 
    tier2_2017ppp["pct:eca",]*eca+
    tier2_2017ppp["pct:lac",]*lac+
    tier2_2017ppp["pct:ssa",]*ssa
  
)]

CF17 <- CF17[, .(code, year, t1_comp1, t1_qf, t2_comp1, t2_qf)]

#######################
### SAVE FINAL DATA ###
#######################
CF <- list(ppp2021 = CF21, ppp2017 = CF17)

qs::qsave(CF, file="04-outputdata/cmd_coeff.qs")

####################################
### CHECK EVERYTHING MAKES SENSE ###
####################################
# Sample prediction for Ethiopia 2021
eth2021 <- data.frame(quantile = seq(1,1000,1)/1000-0.0005,code="ETH",year=2021) |>
  joyn::joyn(CF21,match_type="m:1",by=c("code","year"), keep="left",reportvar=FALSE) |>
  mutate(y_tier1_21 = exp(t1_comp1+t1_qf*log(quantile/(1-quantile))),
         y_tier2_21 = exp(t2_comp1+t2_qf*log(quantile/(1-quantile)))) |>
  select(code,year,y_tier1_21,y_tier2_21,quantile)  |>
  joyn::joyn(CF17,match_type="m:1",by=c("code","year"), keep="left",reportvar=FALSE) |>
  mutate(y_tier1_17 = exp(t1_comp1+t1_qf*log(quantile/(1-quantile))),
         y_tier2_17 = exp(t2_comp1+t2_qf*log(quantile/(1-quantile)))) |>
  select(starts_with("y_tier"),quantile) |>
  tidyr::pivot_longer(-quantile,names_to="type",values_to="y")

library(ggplot2)
ggplot(data=eth2021) + geom_density(aes(x=y),color="blue") + facet_wrap(~type) + xlim(0,15)

# Predict extreme poverty rates to make sure they are in the unit interval and broadly make sense 
expr21 <- CF21 |>
        mutate(tier1_21 = 100*(1+(exp(t1_comp1)/3)^(1/t1_qf))^(-1),
               tier2_21 = 100*(1+(exp(t2_comp1)/3)^(1/t2_qf))^(-1)) |>
        select(code,year,tier1_21,tier2_21)

expr <- CF17 |>
  mutate(tier1_17 = 100*(1+(exp(t1_comp1)/3)^(1/t1_qf))^(-1),
         tier2_17 = 100*(1+(exp(t2_comp1)/3)^(1/t2_qf))^(-1)) |>
  select(code,year,tier1_17,tier2_17) |>
  joyn::joyn(expr21,by=c("code","year"),match_type="1:1",reportvar=FALSE)

ggplot(expr,aes(x=tier1_21,y=tier2_21)) + geom_point()
ggplot(expr,aes(x=tier1_17,y=tier2_17)) + geom_point()
ggplot(expr,aes(x=tier1_21,y=tier1_17)) + geom_point()
ggplot(expr,aes(x=tier2_21,y=tier2_17)) + geom_point()
