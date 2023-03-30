
#
# Packages ----
#

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)


#
# Checkng stations and parameters for 2020 assessment
#

df_indicator_2020 <- readRDS("Data/13_df_indicator_NIVA_only (2020).rds")

tab1 <- xtabs(~STATION_CODE + LATIN_NAME, df_indicator_2020)
tab2 <- xtabs(~PARAM + LATIN_NAME, df_indicator_2020)
tab1
# STATION_CODE Gadus morhua Mytilus edulis
# 10A2            0              6
# 10B             6              0
# 11X             0              6
# 19B             5              0
# 43B2            5              0
# 45B2            4              0
# 98A2            0              5
# 98B1            7              0

tab2
rownames(tab2)
# "CD"    "DDEPP" "HCB"   "HG"    "PB" 

#
# Latest NIVA data ----
#

# dat_all <- readRDS("../Milkys2_pc/Files_from_Jupyterhub_2021/Raw_data/101_data_updated_2022-09-01.rds") %>%
#   select(MYEAR:VALUE_WW)
# saveRDS(dat_all, "Input_data/NIVA_data_2022-09-01.rds")

dat_all <- readRDS("Input_data/NIVA_data_2022-09-01.rds")

dat_1a <- dat_all %>%
  filter(
    STATION_CODE %in% rownames(tab1),
    PARAM %in% c("CD", "DDEPP", "HCB", "HG", "PB")
  )

xtabs(~PARAM + STATION_CODE, dat_1a)

#
# NOTE: ADD station 20B as well
#

#
# Calculate upper/lower bounds of PCB7 ----
#

dat_1b <- dat_all %>%
  filter(
    STATION_CODE %in% rownames(tab1),
    PARAM %in% c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")
  ) %>%
  mutate(
    VALUE_lo = ifelse(is.na(FLAG1), VALUE_WW, 0),
    VALUE_up = VALUE_WW
  ) %>%
  group_by(STATION_CODE, SAMPLE_NO2, LATIN_NAME, MYEAR) %>%
  summarise(
    CB7_lo = sum(VALUE_lo),
    CB7_up = sum(VALUE_up)
  )

#
# Get sumPCB7 data for one station  
#

#
# . Individual data ----
#
dat_test1 <- dat_1b %>%
  filter(STATION_CODE == "10A2")

ggplot(dat_test1, aes(x = MYEAR)) +
  geom_point(aes(y = CB7_up), color = "red") +
  geom_point(aes(y = CB7_lo), color = "blue")

#
# . Median data ----
#
dat_test2 <- dat_test1 %>%
  group_by(STATION_CODE, LATIN_NAME, MYEAR) %>%
  summarise(
    CB7_lo = median(CB7_lo),
    CB7_up = median(CB7_up)
  )


ggplot(dat_test2, aes(x = MYEAR)) +
  geom_point(aes(y = CB7_up), color = "red") +
  geom_point(aes(y = CB7_lo), color = "blue")



#
# Interval regression using 'icenReg' ----   
#
# - NOTE: will not be used
# 

# install.packages("icenReg")
library(icenReg)
library(foreach)
library(doParallel)

# using individual datat  
mle_fit <- ic_par(cbind(CB7_lo, CB7_up) ~ MYEAR,
                  model = "ph",
                  dist = "lnorm",
                  data = dat_test1)
summary(mle_fit)


#
# Interval regression using JAGS function ----   
#

library(R2jags)
library(rjags)
source("interval_lm_qi.R")  # function 'int_linear_qi'  

#
# . all data (until 2021) ----      
#


# Model
mod <- int_linear_qi(data = dat_test2, x = "MYEAR", y_lo = "CB7_lo", y_up = "CB7_up")

# str(mod,1)

cat("Estimate of slope (with uncertainty) \n")
mod$slope

if (mod$slope[["2.5%"]] > 0){
  # If lower range (2.5 percentile) of the slope estimate is above zero....
  cat("Significant increase over time")
} else if (mod$slope[["97.5%"]] < 0){
  # If upper range (97.5 percentile) of the slope estimate is below zero....
  cat("Significant decrease over time")
} else {
  cat("No significant change over time")
}

# Test plot  
ggplot(dat_test2, aes(x = MYEAR)) +
  geom_point(aes(y = CB7_up), color = "red") +
  geom_point(aes(y = CB7_lo), color = "blue") +
  geom_line(data = mod$plot_data, aes(x, y)) +
  geom_line(data = mod$plot_data, aes(x, y_lo), linetype = "dashed") +
  geom_line(data = mod$plot_data, aes(x, y_hi), linetype = "dashed")



#
# . shorter time series (until 2014) ----      
#

dat_test2b <- dat_test2 %>% filter(MYEAR <= 2016)

# Model
mod <- int_linear_qi(data = dat_test2b, x = "MYEAR", y_lo = "CB7_lo", y_up = "CB7_up")

# str(mod,1)

cat("Estimate of slope (with uncertainty) \n")
mod$slope


# Test plot  
ggplot(dat_test2b, aes(x = MYEAR)) +
  geom_point(aes(y = CB7_up), color = "red") +
  geom_point(aes(y = CB7_lo), color = "blue") +
  geom_line(data = mod$plot_data, aes(x, y)) +
  geom_line(data = mod$plot_data, aes(x, y_lo), linetype = "dashed") +
  geom_line(data = mod$plot_data, aes(x, y_hi), linetype = "dashed")



