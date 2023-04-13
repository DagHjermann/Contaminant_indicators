
#
# Packages ----
#

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(purrr)

# for JAGS
library(R2jags)
library(rjags)
source("interval_lm_qi.R")  # function 'int_linear_qi'  


#
# Checking stations and parameters for 2020 assessment
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

# In addition to the stations we had in the last assessment, we now have
# a new station, 20B (Longyearbyen)

# The NIVA stations are then: 
#   c("10A2", "10B", "11X", "19B", "20B", "43B2", "45B2", "98A2", "98B1")
# where the ones with 'B' are cod stations and the others are blue mussel stations

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
    STATION_CODE %in% c("10A2", "10B", "11X", "19B", "43B2", "45B2", "98A2", "98B1", "20B"),
    PARAM %in% c("CD", "DDEPP", "HCB", "HG", "PB")
  )

xtabs(~PARAM + STATION_CODE, dat_1a)

#
# NOTE: added station 20B as well
#

xtabs(~PARAM + is.na(FLAG1), dat_1a)


#
# Calculate upper/lower bounds of PCB7 ----
#

dat_1b <- dat_all %>%
  filter(
    STATION_CODE %in% c("10A2", "10B", "11X", "19B", "43B2", "45B2", "98A2", "98B1", "20B"),
    PARAM %in% c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")  # for BDE, replace here 
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

# Medians of lower and upper bound
dat_1b_median <- dat_1b %>%
  group_by(STATION_CODE, LATIN_NAME, MYEAR) %>%
  summarise(
    CB7_lo = median(CB7_lo),
    CB7_up = median(CB7_up)
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
dat_test2 <- dat_1b_median %>%
  filter(STATION_CODE == "10A2")

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


#
# . all data (until 2021) ----      
#
# This includes the later years with huge difference between min and max numbers,
#   so not unsurprisingly we get no signficicant time trend (because uncertainty is
#   so large)

# Model
mod <- int_linear_qi(data = dat_test2, x = "MYEAR", y_lo = "CB7_lo", y_up = "CB7_up")

# str(mod,1)

cat("Estimate of slope (with uncertainty) \n")
mod$slope
# The 95% "confidence interval" is defined by '2.5%' and '97.5%'

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
# This avoids the later years with large uncertainty, so we would expect to pick up the
#   downward time trend that we see. 
# If the downward time trend was significant we would get both '2.5%' and '97.5%' below zero.
# However '97.5%' is slightly above zero, so we cannot say that the negative trend
#   is statistically significant

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


#
# Interval regression for several time series ----   
#

#
# . data ----
#
# Standardize on names VALUE_lo and VALUE_up
#

dat_individual_param <- dat_all %>%
  filter(
    STATION_CODE %in% c("10A2", "10B", "11X", "19B", "43B2", "45B2", "98A2", "98B1", "20B"),
    PARAM %in% c("CD", "DDEPP", "HCB", "HG", "PB")  
  ) %>%
  mutate(
    VALUE_lo = ifelse(is.na(FLAG1), VALUE_WW, 0),
    VALUE_up = VALUE_WW
  ) %>%
  select(STATION_CODE, SAMPLE_NO2, MYEAR, PARAM, VALUE_lo, VALUE_up)


# data for PCB sum  
# Almost identical to 'dat_1b' code above, but setting names to VALUE_lo and VALUE_up,
# and adding PARAM = "CB_S7"

dat_CB <- dat_all %>%
  filter(
    STATION_CODE %in% c("10A2", "10B", "11X", "19B", "43B2", "45B2", "98A2", "98B1", "20B"),
    PARAM %in% c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")  # for BDE, replace here 
  ) %>%
  mutate(
    VALUE_lo = ifelse(is.na(FLAG1), VALUE_WW, 0),
    VALUE_up = VALUE_WW
  ) %>%
  group_by(STATION_CODE, SAMPLE_NO2, LATIN_NAME, MYEAR) %>%
  summarise(
    VALUE_lo = sum(VALUE_lo),
    VALUE_up = sum(VALUE_up), .groups = "drop",
  ) %>%
  mutate(PARAM = "CB_S7") %>%
  select(STATION_CODE, SAMPLE_NO2, MYEAR, PARAM, VALUE_lo, VALUE_up)


# NOTE: for BDE, make a similar 'dat_BDE' where PARAM = BDE6S
#   and the sum is defined like this:
# BDE6S = Sum of BDE congener numbers 28 (tri), 47 (tetra), 99 (penta), 
#   100 (penta), 153 (hexa) and 154 (hexa)
# Names: BDE28, BDE47 etc.

dat_test3_individual <- bind_rows(
  dat_individual_param,
  dat_CB,                      # dat_BDE should also be added here 
  dat_BDE
)

dat_test3_medians <- dat_test3_individual %>%
  group_by(PARAM, STATION_CODE, MYEAR) %>%
  summarise(
    VALUE_lo = median(VALUE_lo),
    VALUE_up = median(VALUE_up)
  )



# Here, two time series for CB7: stations 45B2 and 98A2


# Make function that returns a data frame with lower and upper limits of the slope
#   for a given function and for a given data set
# (Can modify this so it also works for a specific parameter, but in this case we usi)
get_trendslope <- function(parameter, stationcode, data){
  dat_selected <- data %>%
    filter(PARAM %in% parameter & STATION_CODE %in% stationcode)
  mod <- int_linear_qi(data = dat_selected, x = "MYEAR", y_lo = "VALUE_lo", y_up = "VALUE_up")
  data.frame(
    PARAM = parameter,
    STATION_CODE = stationcode,
    slope_lo = mod$slope[["2.5%"]],
    slope = mod$slope[["50%"]],
    slope_up = mod$slope[["97.5%"]])
}




# test for one parameter / station
X <- get_trendslope("CB_S7", "10A2", dat_test3_medians)
# X

# Make "safe" version (doen't stop if there is an error)
get_trendslope_s <- safely(get_trendslope)

X2 <- get_trendslope_s("CB_S7", "10A2", dat_test3_medians)

result1 <- map2(c("CB_S7","CB_S7", "PB"), c("10A2","43B2", "10A2"), get_trendslope_s, data = dat_test3_medians)

# Get all combinations:
df_combinations <- dat_test3_medians %>%
  distinct(PARAM, STATION_CODE)
# result1 <- map2(df_combinations$PARAM, df_combinations$STATION_CODE, get_trendslope_s, data = dat_test3_medians)

# example
result1[[3]]$result

str(result1, 1)
str(result1, 2)

# collecting the results  
result2 <- purrr::transpose(result1)
result_is_ok <- map_lgl(result2$error, is.null) 
result3 <- result2$result[result_is_ok]

result4 <- bind_rows(result3)

result4 <- result4 %>%
  mutate(
    trend = case_when(
      slope_lo > 0 ~ 2,
      slope_up < 0 ~ 3,
      TRUE ~ 1),
    trend_text = case_when(
      slope_lo > 0 ~ "Trend_up",
      slope_up < 0 ~ "Trend_down",
      TRUE ~ "No trend")
  )
