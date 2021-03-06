---
title: "Get NIVA trends, 2020"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    
---

Actually the script should be called get NIVA _trends_ because that is what we actually do  

## Packages + functions  

```r
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
```

## Data
### Read main NIVA data  
Only like 10 seconds (3-4 minutes if column types is not given)  
__NOTE hard-coded ranges for excel sheet (both no. of columns and rows); 2 places__

```r
# Define column types for faster reading + fewer (or none) errors

# Column types, until column DP only
coltypes = c(rep("text",18),                  # PARAM - Backgr_stations
             rep("numeric", 4),               # Backgr_Nstat - Q95
             rep(c("numeric","text"),39),     # Data 1980 - 2018 (2 columns per year) [W/X - CU/CV]
             rep(c("text","numeric","numeric","numeric","numeric"),2),  # Ant.prøver - EQS threshold, 2016+2017 [CW-DF]
             "text",                                          # OC 
             rep("numeric",10), "text"                        # Trend p(long) - Trends, 2017 only [DH-DR]
             )

# write.csv2(coltypes, "clipboard", row.names = FALSE, col.names = FALSE)

fn <- "C:/Data/seksjon 212/Milkys/Big_excel_table/Data_xl_lessthans_ver12.xlsm"
data_xl <- read_excel(fn,
                      range = "A1:DR19305",    # NOTE HARD-CODED RANGE  
                      col_types = coltypes)

# Read the single less-than value from 2018
data_xl_lessthans <- read_excel(fn,
                      range = "HI1:HI19305",    # NOTE HARD-CODED RANGE  
                      col_types = "logical")


# dim(data_xl)
# [1]  29546   216
```

### 'data_xl_sel' = selected rows of excel table

```r
species <- "Gadus morhua"
#
# Muscle data (Hg)
#
tissue <- "Muscle"
sel_param <- c("HG")
sel_records1 <- with(data_xl, PARAM %in% sel_param & 
                              TISSUE_NAME %in% tissue & 
                              LATIN_NAME %in% species & 
                              Basis == "WW" & !is.na(Klasse.2017))

#
# Liver data (the rest)
#
tissue <- "Liver"
sel_param <- c("CD", "HG", "PB", "BDE6S", "HCB", "DDEPP", "CB_S7")
sel_records2 <- with(data_xl, PARAM %in% sel_param & 
                              TISSUE_NAME %in% tissue & 
                              LATIN_NAME %in% species & 
                              Basis == "WW" & !is.na(Klasse.2017))

# Combine muscel and liver data
sel_records <- sel_records1 | sel_records2
sum(sel_records1)  # 17
```

```
## [1] 17
```

```r
sum(sel_records2)  # 75
```

```
## [1] 75
```

```r
sum(sel_records)   # 92
```

```
## [1] 92
```

```r
# Select rows
data_xl_sel <- data_xl[sel_records,]

# Add less-thans
data_xl_sel <- bind_cols(data_xl_sel, data_xl_lessthans[sel_records,])

# remove data_xl
# rm(data_xl)
```


### Station data  
Shortened from script 98 iin 'Milkys'   

```r
data_stations <- readxl::read_excel("../Milkys/Input_data/Kartbase.xlsx") %>%
  select(stasjonskode, `catch LAT__1`, `catch LONG__1`, stasjonsnavn) %>%
  rename(STATION_CODE = stasjonskode,
         LATITUDE = `catch LAT__1`, LONGITUDE = `catch LONG__1`, 
         STATION_NAME = stasjonsnavn) %>%
  filter(!is.na(STATION_CODE)) %>%
  # One duplicate (different names), we just remove one of them
  filter(!STATION_NAME %in% "Risøy, Østerfjord")

### Pick by area and species  
data_stations <- data_stations %>% 
  filter(LATITUDE > 68) %>%
  # no blue mussel, flatfish or snail stations
  filter(!(grepl("A", STATION_CODE) | grepl("X", STATION_CODE) | grepl("F", STATION_CODE) | grepl("G", STATION_CODE)))

data_stations
```

```
## # A tibble: 6 x 4
##   STATION_CODE LATITUDE LONGITUDE STATION_NAME                    
##   <chr>           <dbl>     <dbl> <chr>                           
## 1 10B              69.8      29.8 Kjøfjord, Outer Varangerfjord   
## 2 43B2             69.7      19.0 Tromsø harbour area             
## 3 45B2             70.6      23.6 Hammerfest harbour area         
## 4 98B1             68.2      14.7 Austnesfjord, Lofoten           
## 5 19B              78.2      13.5 Isfjorden, Svalbard             
## 6 19N              79.0      12.1 Breøyane, Kongsfjorden, Svalbard
```


### Limits

```r
# Limits for EQS and Mattrygghet (food safety)
df_limits <- read_excel("Input_data/Grenseverdier_fra_Sylvia.xlsx")
```


## Add Conc value for check  
- Concnetration value of last year  
- If less-than, it is set slightly lower than value (in case the limit is exacly the same)
- To remove before export

```r
value_lastyear <- data_xl_sel$Yr_2018    # HARD_CODED YEAR

# Less-thans are set a tad lower, to be put in the lower class
sel2 <- !is.na(data_xl_sel$Lt_2018) & data_xl_sel$Lt_2018
value_lastyear[sel2] <- value_lastyear[sel2] - 0.00001

data_xl_sel$Conc_for_check <- value_lastyear
```


## 'df_indicator'  

### Make 'df_indicator'  

```r
df_indicator <- tibble(
  PROJECT_ID = 3699,
  LATIN_NAME = data_xl_sel[["LATIN_NAME"]],
  STATION_CODE = data_xl_sel[["STATION_CODE"]],
  SPECIES_ID = 17,                  # HARD-CODED - check above that we only have cod stations
  TISSUE_NAME = data_xl_sel[["TISSUE_NAME"]],
  PARAM = data_xl_sel[["PARAM"]],
  Conc = data_xl_sel[["Conc_for_check"]],
  trend_symbol = substr(data_xl_sel[["Trends.2018"]], 3, 3),     # 3rd symbol, e.g. the "¢" in ê/¢ - FOR 10-YEAR TREND
  N = NA
  )

#
# Add station position + name
#
nrow(df_indicator)  # 92
```

```
## [1] 92
```

```r
df_indicator <- df_indicator %>% 
  inner_join(data_stations, by = "STATION_CODE")
nrow(df_indicator)  # 27
```

```
## [1] 27
```


### Add NIVA_CODE

```r
df_tissue_code <- tibble(
  TISSUE_NAME = c("Whole soft body", "Muscle", "Liver"),
  NIVA_CODE = c("SB","MU","LI")
  )

nrow(df_indicator)
```

```
## [1] 27
```

```r
df_indicator <- left_join(df_indicator, df_tissue_code, by = "TISSUE_NAME")
nrow(df_indicator)
```

```
## [1] 27
```

### Add trend text

```r
# Check 'set_symbol' function: 
# source("18_Time_series_write_to_Excel_functions.R")
# set_symbol
df_symbol_to_trend <- tibble(
  trend_symbol = c("§", "«", "¢", "é", "ê"),
  trend_text = c("Too little data", "Too few data above LOQ", "No trend", "Trend up", "Trend down"),
  trend = c(0,0,1,2,3)
  )

# Add trend symbol  
nrow(df_indicator)
```

```
## [1] 27
```

```r
df_indicator <- left_join(df_indicator, df_symbol_to_trend, by = "trend_symbol")
nrow(df_indicator)
```

```
## [1] 27
```

```r
# Check result
table(addNA(df_indicator$trend))
```

```
## 
##    0    1    2    3 <NA> 
##    7   15    3    2    0
```

```r
df_indicator$trend_symbol <- NULL
df_indicator$trend_text <- NULL

head(df_indicator, 3)
```

```
## # A tibble: 3 x 13
##   PROJECT_ID LATIN_NAME STATION_CODE SPECIES_ID TISSUE_NAME PARAM  Conc N    
##        <dbl> <chr>      <chr>             <dbl> <chr>       <chr> <dbl> <lgl>
## 1       3699 Gadus mor~ 19B                  17 Liver       BDE6S 0.977 NA   
## 2       3699 Gadus mor~ 43B2                 17 Liver       BDE6S 1.67  NA   
## 3       3699 Gadus mor~ 98B1                 17 Liver       BDE6S 7.70  NA   
## # ... with 5 more variables: LATITUDE <dbl>, LONGITUDE <dbl>,
## #   STATION_NAME <chr>, NIVA_CODE <chr>, trend <dbl>
```

## Save  

```r
overwrite <- FALSE

if (overwrite){
  # Save indicator for NIVA data
  saveRDS(df_indicator, "Data/13_df_indicator_NIVA_only (2020).rds")
  write.csv(df_indicator, "Data/13_df_indicator_NIVA_only (2020).csv", quote = FALSE, row.names = FALSE)
  
  # Save selected excel data
  saveRDS(data_xl_sel, "Data/13_data_xl_sel (2020).rds")
  
}
```

