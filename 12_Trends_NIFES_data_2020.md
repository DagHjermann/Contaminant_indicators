---
title: "NIFES data, calculate trends"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    
---

Based on code from '04_NIVA_data_with_NIFES_3.R' ("H:/Documents/seksjon 212/Indikator 2018/Analyse")  


## 1. Packages + functions  

```r
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(mgcv)         # for gam()
library(AICcmodavg)   # for AICc()

source("00_functions.R")
source("16_Trend_functions.R")
```

## 2. Data

```r
df_median <- readRDS("Data/11_df_median (2020).rds") %>%
  rename(TISSUE_NAME = Organ) %>%   # avoid a bunch of annoying error messages...
  as.data.frame() 
```

## 3. Function for performing regression    
From '04_NIVA_data_with_NIFES_3.R', but added 'areas' as input ('Uttaksområde')  

```r
#
# Get regression for all
#
get_regression_statistics <- function(par, tissue, position){
  df_median_sel <- df_median  %>% 
    filter(Parameter %in% par & Posisjon %in% position & TISSUE_NAME %in% tissue & !is.na(Conc)) %>%
    rename(YEAR = Year, Median = Conc)
  df_med_object <- df_median_sel %>% select_for_regr_a() %>% select_for_regr_b()
  modelresults <- try(calc_models_gam(df_med_object, log = TRUE))
  if (class(modelresults)[[1]] != "try-error"){
    res <- statistics_for_excel(df_med_object, modelresults, gam = TRUE)
  } else {
    res <- statistics_for_excel_empty(df_median_sel)
    res$N <- df_med_object$N
    res$Nplus <- df_med_object$Nplus
  }
  data.frame(Posisjon = position, Parameter = par, TISSUE_NAME = tissue, res, stringsAsFactors = FALSE)
}

get_tissue <- function(par){
  ifelse(par %in% "HG", "Muskel", "Lever")
}

# Test
# get_regression_statistics("HG", "Muskel", c("71,60N 21,30E")) 
```


### Table of areas and years  

```r
xtabs(~Year + Posisjon, df_median)
```

```
##       Posisjon
## Year   71,60N 21,30E 72,80N 32,92E 74,53N 19,93E
##   1994             1             1             0
##   1998             1             1             0
##   2001             1             1             0
##   2002             1             1             0
##   2006            12            12             0
##   2007             1            13             4
##   2008            19            19            19
##   2009            19            12            12
##   2010            12            12             0
##   2012            19            19             0
##   2013            19            19             0
##   2014            19            19            19
##   2015            19            19            19
##   2016            19            19            19
##   2017             0            19            19
##   2018            12            12             0
##   2019             0            12            12
```

```r
# xtabs(~Organ, df_median)

xtabs(~Year + TISSUE_NAME + Art, df_median)
```

```
## , , Art = TORSK
## 
##       TISSUE_NAME
## Year   Lever Muskel
##   1994     0      2
##   1998     0      2
##   2001     0      2
##   2002     0      2
##   2006    22      2
##   2007    15      3
##   2008    54      3
##   2009    40      3
##   2010    22      2
##   2012    36      2
##   2013    36      2
##   2014    54      3
##   2015    54      3
##   2016    54      3
##   2017    36      2
##   2018    22      2
##   2019    22      2
```

## 4. Regression on 'df_median'  

### Make table for regression  

```r
# Doing regression for all
sel_param <- c("CD", "HG", "PB", "HCB", "DDEPP", "CB_S7", "DDEPP")

timeseries <- df_median %>%
  filter(Parameter %in% sel_param) %>%
  count(Posisjon, Parameter) %>%
  mutate(TISSUE_NAME = get_tissue(Parameter))

timeseries
```

```
## # A tibble: 18 x 4
##    Posisjon      Parameter     n TISSUE_NAME
##    <chr>         <chr>     <int> <chr>      
##  1 71,60N 21,30E CB_S7        10 Lever      
##  2 71,60N 21,30E CD           10 Lever      
##  3 71,60N 21,30E DDEPP         7 Lever      
##  4 71,60N 21,30E HCB           7 Lever      
##  5 71,60N 21,30E HG           15 Muskel     
##  6 71,60N 21,30E PB           10 Lever      
##  7 72,80N 32,92E CB_S7        13 Lever      
##  8 72,80N 32,92E CD           13 Lever      
##  9 72,80N 32,92E DDEPP         7 Lever      
## 10 72,80N 32,92E HCB           8 Lever      
## 11 72,80N 32,92E HG           17 Muskel     
## 12 72,80N 32,92E PB           13 Lever      
## 13 74,53N 19,93E CB_S7         7 Lever      
## 14 74,53N 19,93E CD            8 Lever      
## 15 74,53N 19,93E DDEPP         5 Lever      
## 16 74,53N 19,93E HCB           6 Lever      
## 17 74,53N 19,93E HG            8 Muskel     
## 18 74,53N 19,93E PB            8 Lever
```

### Perform all regressions

```r
# Perform all regressions
nifes_regression <- 1:nrow(timeseries) %>% purrr::map_dfr(
  ~get_regression_statistics(
    timeseries$Parameter[.], 
    timeseries$TISSUE_NAME[.], 
    timeseries$Posisjon[.])
  )

# Add 'trend_text'
nifes_regression <- nifes_regression %>%
  mutate(trend_text = case_when(
    N < 5 ~ "Too little data",
    N >= 5 & Nplus < 5 ~ "Too few data above LOQ",
    Model_used %in% c("Linear", "Nonlinear") & Dir_change == "Up" ~ "Trend up",
    Model_used %in% c("Linear", "Nonlinear") & Dir_change == "Down" ~ "Trend down",
    Model_used %in% c("Linear", "Nonlinear") & Dir_change == "" ~ "No trend",
    ))


# Adding trend symbol
# Based on "18_Time_series_write_to_Excel_functions.R"
df_symbol_to_trend <- tibble(
  trend_symbol = c("§", "«", "¢", "é", "ê"),
  trend_text = c("Too little data", "Too few data above LOQ", "No trend", "Trend up", "Trend down"),
  trend = c(0,0,1,2,3)
  )

nifes_regression <- nifes_regression %>%
  left_join(df_symbol_to_trend[,c("trend_text", "trend")]) %>% # adding trend symbol
  rename(PARAM = Parameter)   # To fit with NIVA data
```

```
## Joining, by = "trend_text"
```

### Check

```r
nifes_regression %>% select(Posisjon, PARAM, TISSUE_NAME, Nplus, Model_used, Dir_change, trend_text)
```

```
##         Posisjon PARAM TISSUE_NAME Nplus Model_used Dir_change trend_text
## 1  71,60N 21,30E CB_S7       Lever    10  Nonlinear              No trend
## 2  71,60N 21,30E    CD       Lever    10     Linear       Down Trend down
## 3  71,60N 21,30E DDEPP       Lever     7     Linear              No trend
## 4  71,60N 21,30E   HCB       Lever     7     Linear              No trend
## 5  71,60N 21,30E    HG      Muskel    15     Linear              No trend
## 6  71,60N 21,30E    PB       Lever     7  Nonlinear       Down Trend down
## 7  72,80N 32,92E CB_S7       Lever    13  Nonlinear       Down Trend down
## 8  72,80N 32,92E    CD       Lever    13     Linear         Up   Trend up
## 9  72,80N 32,92E DDEPP       Lever     7     Linear              No trend
## 10 72,80N 32,92E   HCB       Lever     8     Linear              No trend
## 11 72,80N 32,92E    HG      Muskel    17     Linear              No trend
## 12 72,80N 32,92E    PB       Lever    10     Linear              No trend
## 13 74,53N 19,93E CB_S7       Lever     7     Linear              No trend
## 14 74,53N 19,93E    CD       Lever     8     Linear         Up   Trend up
## 15 74,53N 19,93E DDEPP       Lever     5     Linear              No trend
## 16 74,53N 19,93E   HCB       Lever     6     Linear              No trend
## 17 74,53N 19,93E    HG      Muskel     8     Linear              No trend
## 18 74,53N 19,93E    PB       Lever     6     Linear              No trend
```


## 5. Save

```r
overwrite <- FALSE

if (overwrite){
  saveRDS(nifes_regression, "Data/12_nifes_regression (2020).rds")
  write.csv(nifes_regression, "Data/12_nifes_regression (2020).csv", quote = FALSE, row.names = FALSE)
}
```



