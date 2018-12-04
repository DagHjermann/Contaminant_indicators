---
title: "01_Read_fix_NIFES_data"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    code_folding: show
    df_print: paged
---

Based on code from '04_NIVA_data_with_NIFES_3.R' ("H:/Documents/seksjon 212/Indikator 2018/Analyse")  

## 1. Packages  


## 2. Data  

```r
dat1 <- read_excel("Input_data/Data kysttorsk NH nov. 2018.xlsx", sheet = "Metaller i torskefilet")
dat2 <- read_excel("Input_data/Data kysttorsk NH nov. 2018.xlsx", sheet = "Metaller og POPs i torskelever", skip = 2)

colnames(dat1)
```

```
##  [1] "Jnr"                 "Merking"             "År"                 
##  [4] "Uttaksdato"          "Mottaksdato"         "Uttakssted"         
##  [7] "Uttaksområde"        "Oppdragsgiver"       "Art"                
## [10] "Organ"               "Engelsk beskrivelse" "Latinsk navn"       
## [13] "Produkt beskrivelse" "Prosjekt notat"      "Prøve status"       
## [16] "Prosjekt Nr."        "Tørrstoff %"         "Enhet"              
## [19] "Ag"                  "Enhet__1"            "As"                 
## [22] "Enhet__2"            "Ba"                  "Enhet__3"           
## [25] "Cd"                  "Enhet__4"            "Co"                 
## [28] "Enhet__5"            "Cu"                  "Enhet__6"           
## [31] "Fe"                  "Enhet__7"            "Hg"                 
## [34] "Enhet__8"            "Mn"                  "Enhet__9"           
## [37] "Mo"                  "Enhet__10"           "Pb"                 
## [40] "Enhet__11"           "Se"                  "Enhet__12"          
## [43] "Sn"                  "Enhet__13"           "Sr"                 
## [46] "Enhet__14"           "V"                   "Enhet__15"          
## [49] "Zn"                  "Enhet__16"           "Alder"              
## [52] "Enhet__17"           "Hel fisk lengde"     "Enhet__18"          
## [55] "Hel fisk vekt"       "Enhet__19"           "Kjønn"              
## [58] "Lever vekt"          "Enhet__20"           "Cr"                 
## [61] "Enhet__21"           "Ni"                  "Enhet__22"
```

```r
# cat("\n")
# colnames(dat2)

# Check uniqueness of Jnr
nrow(dat1) == length(unique(dat1$Jnr))
```

```
## [1] TRUE
```

```r
nrow(dat2) == length(unique(dat2$Jnr))
```

```
## [1] TRUE
```

## 3. Put dat1 on tidy ('tall') format
### Get column numbers

```r
# colnames(dat1) %>% dput()

colnumber_unit <- which(substr(colnames(dat1),1,5) %in% "Enhet")
colnumber_conc <- colnumber_unit - 1
colnumber_meta <- 1:(colnumber_conc[1]-1)
```

### Make concentration data

```r
dat1_conc <- dat1[c(colnumber_meta, colnumber_conc)] %>%
  gather("Parameter", "Conc_chr", `Tørrstoff %`:Ni) %>%                            # Note hard-coded column names
  as.data.frame()

# Check first and second character of value. The "<" is always in the first
table(substr(dat1_conc$Conc_chr, 1, 1))
```

```
## 
##    <    0    1    2    3    4    5    6    7    8    9 
## 2265 1972 1128  535  585  332  254  256  243  123   87
```

```r
table(substr(dat1_conc$Conc_chr, 2, 2))
```

```
## 
##              .    0    1    2    3    4    5    6    7    8    9 
##  347 1836 3622  593  126  152  122  116  130  107  173  233  223
```

```r
# Remove less-than sign and make Conc variabøe
dat1_conc$Conc_chr2 <- sub("<", "", dat1_conc$Conc_chr, fixed = TRUE)
dat1_conc$Conc <- as.numeric(dat1_conc$Conc_chr2)

# Find less-thans and make Flag variable
dat1_conc$Flag <- ifelse(substr(dat1_conc$Conc_chr, 1, 1) %in% "<", "<", "")

nrow(dat1_conc)
```

```
## [1] 8671
```

### Make unit data

```r
dat1_unit <- dat1[c(1, colnumber_unit)] %>%
  as.data.frame()

# Set column names (which now are "Enhet__1" etc.) to the corresponding actual name
colnames(dat1_unit)[-1] <- colnames(dat1)[colnumber_conc]

dat1_unit <- dat1_unit %>%
  gather("Parameter", "Unit", `Tørrstoff %`:Ni) %>%                               # Note hard-coded column names
  as.data.frame()

nrow(dat1_unit)
```

```
## [1] 8671
```

### Put dat1_conc and dat1_unit together

```r
check <- sum(dat1_conc$Jnr != dat1_unit$Jnr)
cat("Number of Jnr not equal:", sum(check), "\n")
```

```
## Number of Jnr not equal: 0
```

```r
if (!is.na(check) & check == 0){
  dat1_tall <- cbind(dat1_conc, Unit = dat1_unit$Unit, stringsAsFactors = FALSE)
}
```


## 4. Put dat2 on tidy ('tall') format
### Get column numbers

```r
# colnames(dat2) %>% dput()

colnumber_unit <- which(substr(colnames(dat2),1,5) %in% "Enhet")
colnumber_conc <- colnumber_unit - 1
colnumber_meta <- 1:(colnumber_conc[1]-1)

# colnames(dat2)[colnumber_meta]
# colnames(dat2)[colnumber_conc]
# colnames(dat2)[colnumber_unit]
```

### Make concentration data

```r
dat2_conc <- dat2[c(colnumber_meta, colnumber_conc)] %>%
  gather("Parameter", "Conc_chr", `Totalfett Etylacetat`:`Sum PBDE 7`) %>%                             # Note hard-coded column names
  as.data.frame()

# Check first and second character of value. The "<" is always in the first
table(substr(dat2_conc$Conc_chr, 1, 1))
```

```
## 
##    <    0    1    2    3    4    5    6    7    8    9 
## 5048 6529 4858 2463 1710 1515 1247 1082  927  810  708
```

```r
table(substr(dat2_conc$Conc_chr, 2, 2))
```

```
## 
##                 .     0     1     2     3     4     5     6     7     8 
##    75  4683 17543   912   514   510   453   434   408   361   376   339 
##     9 
##   289
```

```r
# Remove less-than sign and make Conc variabøe
dat2_conc$Conc_chr2 <- sub("<", "", dat2_conc$Conc_chr, fixed = TRUE)
dat2_conc$Conc <- as.numeric(dat2_conc$Conc_chr2)

# Find less-thans and make Flag variable
dat2_conc$Flag <- ifelse(substr(dat2_conc$Conc_chr, 1, 1) %in% "<", "<", "")

nrow(dat2_conc)
```

```
## [1] 27898
```

### Make unit data

```r
dat2_unit <- dat2[c(1, colnumber_unit)] %>%
  as.data.frame()

# Set column names (which now are "Enhet__1" etc.) to the corresponding actual name
colnames(dat2_unit)[-1] <- colnames(dat2)[colnumber_conc]

dat2_unit <- dat2_unit %>%
  gather("Parameter", "Unit", `Totalfett Etylacetat`:`Sum PBDE 7`) %>%                             # Note hard-coded column names
  as.data.frame()

nrow(dat2_unit)
```

```
## [1] 27898
```

### Put dat2_conc and dat2_unit together

```r
check <- sum(dat2_conc$Jnr != dat2_unit$Jnr)
cat("Number of Jnr not equal:", sum(check), "\n")
```

```
## Number of Jnr not equal: 0
```

```r
if (!is.na(check) & check == 0){
  dat2_tall <- cbind(dat2_conc, Unit = dat2_unit$Unit, stringsAsFactors = FALSE)
}
```

## 5. Put dat1_tall and dat2_tall together

```r
df_nifes_cod <- bind_rows(dat1_tall, dat2_tall)
nrow(df_nifes_cod)  
```

```
## [1] 36569
```

## 6. Check data

```r
# Missing Conc values
sel <- is.na(df_nifes_cod$Conc); sum(sel)
```

```
## [1] 1892
```

```r
df_nifes_cod[sel,] %>% select(Parameter, Conc_chr, Conc_chr2, Conc, Flag) %>% head()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Parameter"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Conc_chr"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Conc_chr2"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Conc"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Flag"],"name":[5],"type":["chr"],"align":["left"]}],"data":[{"1":"Ba","2":"NA","3":"NA","4":"NA","5":"","_rn_":"1426"},{"1":"Ba","2":"NA","3":"NA","4":"NA","5":"","_rn_":"1427"},{"1":"Ba","2":"NA","3":"NA","4":"NA","5":"","_rn_":"1428"},{"1":"Ba","2":"NA","3":"NA","4":"NA","5":"","_rn_":"1429"},{"1":"Ba","2":"NA","3":"NA","4":"NA","5":"","_rn_":"1430"},{"1":"Ba","2":"NA","3":"NA","4":"NA","5":"","_rn_":"1431"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
# Missing Conc but unmissing Conc_chr values
sel <- is.na(df_nifes_cod$Conc) & !is.na(df_nifes_cod$Conc_chr); sum(sel)
```

```
## [1] 0
```

```r
# Check one example where second character = " "
tab <- table(substr(dat2_conc$Conc_chr, 2, 2))
names(tab)
```

```
##  [1] ""  " " "." "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
```

```r
sel <- substr(df_nifes_cod$Conc_chr,2,2) %in% " "; sum(sel)
```

```
## [1] 6519
```

```r
df_nifes_cod[sel,] %>% select(Parameter, Conc_chr, Conc_chr2, Conc, Flag) %>% head()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Parameter"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Conc_chr"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Conc_chr2"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Conc"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Flag"],"name":[5],"type":["chr"],"align":["left"]}],"data":[{"1":"Ag","2":"< .002","3":".002","4":"0.002","5":"<","_rn_":"403"},{"1":"Ag","2":"< .002","3":".002","4":"0.002","5":"<","_rn_":"404"},{"1":"Ag","2":"< .002","3":".002","4":"0.002","5":"<","_rn_":"405"},{"1":"Ag","2":"< .002","3":".002","4":"0.002","5":"<","_rn_":"406"},{"1":"Ag","2":"< .002","3":".002","4":"0.002","5":"<","_rn_":"407"},{"1":"Ag","2":"< .002","3":".002","4":"0.002","5":"<","_rn_":"408"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
# Check one example where second character = ""
sel <- substr(df_nifes_cod$Conc_chr,2,2) %in% ""; sum(sel)
```

```
## [1] 422
```

```r
df_nifes_cod[sel,] %>% select(Parameter, Conc_chr, Conc_chr2, Conc, Flag) %>% head()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Parameter"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Conc_chr"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Conc_chr2"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Conc"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Flag"],"name":[5],"type":["chr"],"align":["left"]}],"data":[{"1":"Alder","2":"6","3":"6","4":"6","5":"","_rn_":"6410"},{"1":"Alder","2":"5","3":"5","4":"5","5":"","_rn_":"6411"},{"1":"Alder","2":"6","3":"6","4":"6","5":"","_rn_":"6412"},{"1":"Alder","2":"7","3":"7","4":"7","5":"","_rn_":"6413"},{"1":"Alder","2":"5","3":"5","4":"5","5":"","_rn_":"6414"},{"1":"Alder","2":"7","3":"7","4":"7","5":"","_rn_":"6415"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
# Check one example where exponent value is used (3.3911910439999998E-3)
df_nifes_cod %>% filter(Jnr %in% "2014-1753/21") %>% select(Parameter, Conc_chr, Conc)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Parameter"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Conc_chr"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Conc"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Tørrstoff %","2":"17.288146964","3":"1.728815e+01"},{"1":"Ag","2":"< .001","3":"1.000000e-03"},{"1":"As","2":"11.147931539","3":"1.114793e+01"},{"1":"Ba","2":"9.1502704250000007E-3","3":"9.150270e-03"},{"1":"Cd","2":"< .0007","3":"7.000000e-04"},{"1":"Co","2":"3.3911910439999998E-3","3":"3.391191e-03"},{"1":"Cu","2":"0.1502575781","3":"1.502576e-01"},{"1":"Fe","2":"0.8606585691","3":"8.606586e-01"},{"1":"Hg","2":"0.1615712873","3":"1.615713e-01"},{"1":"Mn","2":"0.06771782725","3":"6.771783e-02"},{"1":"Mo","2":"< .07","3":"7.000000e-02"},{"1":"Pb","2":"< .004","3":"4.000000e-03"},{"1":"Se","2":"0.3561030837","3":"3.561031e-01"},{"1":"Sn","2":"NA","3":"NA"},{"1":"Sr","2":"1.4701623903","3":"1.470162e+00"},{"1":"V","2":"1.3234768029999999E-3","3":"1.323477e-03"},{"1":"Zn","2":"3.4284897716","3":"3.428490e+00"},{"1":"Alder","2":"8","3":"8.000000e+00"},{"1":"Hel fisk lengde","2":"62","3":"6.200000e+01"},{"1":"Hel fisk vekt","2":"2945","3":"2.945000e+03"},{"1":"Lever vekt","2":"67.32","3":"6.732000e+01"},{"1":"Cr","2":"NA","3":"NA"},{"1":"Ni","2":"NA","3":"NA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## 7. Create df_nifes_cod2
### Add Lat, Long

```r
# df_nifes_cod %>% head(3)

df_nifes_cod <- df_nifes_cod %>%
  separate(Uttakssted, c("Lat_string","Lon_string"), sep = "[:blank:]", remove = FALSE) 

df_nifes_cod %>% select(Jnr, Uttakssted:Lon_string) %>% head(3)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Jnr"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Uttakssted"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Lat_string"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Lon_string"],"name":[4],"type":["chr"],"align":["left"]}],"data":[{"1":"2010-313/1","2":"68.13N 14.0E","3":"68.13N","4":"14.0E","_rn_":"1"},{"1":"2010-313/2","2":"68.13N 14.0E","3":"68.13N","4":"14.0E","_rn_":"2"},{"1":"2010-313/3","2":"68.13N 14.0E","3":"68.13N","4":"14.0E","_rn_":"3"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
# Some (ca 20%) have "N" and "E" as a part of Lat_string/Lon_string
x <- stringr::str_extract(df_nifes_cod$Lat_string, "[a-z,A-Z]")
table(addNA(x))
```

```
## 
##     N  <NA> 
##  7337 29232
```

```r
x <- stringr::str_extract(df_nifes_cod$Lon_string, "[a-z,A-Z]")
table(addNA(x))
```

```
## 
##     E  <NA> 
##  7337 29232
```

```r
# Remove letters and create LATITUDE, LONGITUDE
df_nifes_cod <- df_nifes_cod %>%
  mutate(Lat_string = sub("[a-z,A-Z]", "", Lat_string),
         Lon_string = sub("[a-z,A-Z]", "", Lon_string)) %>%
  mutate(LATITUDE = as.numeric(Lat_string),
         LONGITUDE = as.numeric(Lon_string))
```

### Select/order variables

```r
# Column names, replace space with underscore 
colnames(df_nifes_cod) <- gsub(" ", "_", colnames(df_nifes_cod), fixed = TRUE)

df_nifes_cod <- df_nifes_cod %>%
  select(Jnr,Merking,År,Uttaksdato,Uttakssted,LATITUDE,LONGITUDE,Uttaksområde,Art,Organ,Latinsk_navn,
         Parameter, Conc, Flag, Unit)
```

### Add sum PBDE with 6 congeners (they have only calculated "Sum PBDE 7")  
Creates df_nifes_cod2  

```r
df_nifes_cod_PBDE6 <- df_nifes_cod %>% 
  filter(Parameter %in% c("PBDE 28", "PBDE 47", "PBDE 99", "PBDE 100", "PBDE 126", "PBDE 153")) %>%
  group_by(Jnr,Merking,År,Uttaksdato,Uttakssted,LATITUDE,LONGITUDE,Uttaksområde,Art,Organ,Latinsk_navn,Unit) %>%
  summarise(Conc = sum(Conc, na.rm = TRUE), n = n(), BelowLOQ = sum(Flag %in% "<"), BelowLOQ_fraction = BelowLOQ/n)
summary(df_nifes_cod_PBDE6$BelowLOQ_fraction)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   0.200   0.200   0.252   0.400   0.600
```

```r
df_nifes_cod_PBDE6 <- df_nifes_cod_PBDE6 %>%
  mutate(Flag = ifelse(BelowLOQ_fraction >= 0.5, "<", ""))  # Set Flag to "<" if >=50% of congenerers are below LOQ
         
#
# Add "Parameter" and sort columns in same order
#
df_nifes_cod_PBDE6$Parameter <- "BDE6S"   # same name as in NIVA data
df_nifes_cod_PBDE6 <- df_nifes_cod_PBDE6[,colnames(df_nifes_cod)] %>% as.data.frame()

#
# Add to dataset, create 'df_nifes_cod2'
#
df_nifes_cod2 <- rbind(df_nifes_cod, df_nifes_cod_PBDE6, stringsAsFactors = FALSE)
nrow(df_nifes_cod)  # 36569
```

```
## [1] 36569
```

```r
nrow(df_nifes_cod2) # 36946
```

```
## [1] 36946
```


### Measurement year = 1.april to 31. March

```r
df_nifes_cod2$Time <- lubridate::dmy(df_nifes_cod2$Uttaksdato)
df_nifes_cod2$Month <- lubridate::month(df_nifes_cod2$Time)

# Lacking Year: none; lacking date: 2450
xtabs(~addNA(År), df_nifes_cod2)
```

```
## addNA(År)
##  2010  2011  2012  2013  2014  2015  2016  2018  <NA> 
## 14602  2450  7154  2450  2156  2450  3234  2450     0
```

```r
xtabs(~addNA(Month), df_nifes_cod2)
```

```
## addNA(Month)
##    1    2    3    4    6    9   10   11   12 <NA> 
## 2450 2450 7252 2450 2450 7350 4410 4900  784 2450
```

```r
df_nifes_cod2 %>% filter(is.na(Month)) %>% select(År, Uttaksdato, Time, Month) %>% head(4)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["År"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Uttaksdato"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Time"],"name":[3],"type":["date"],"align":["right"]},{"label":["Month"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"2010","2":"NA","3":"<NA>","4":"NA","_rn_":"1"},{"1":"2010","2":"NA","3":"<NA>","4":"NA","_rn_":"2"},{"1":"2010","2":"NA","3":"<NA>","4":"NA","_rn_":"3"},{"1":"2010","2":"NA","3":"<NA>","4":"NA","_rn_":"4"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
# Set measurement year (32% are in January-March, all in År = 2010,2011,2018)
yr <- df_nifes_cod2$År
sel <- with(df_nifes_cod2, Month <= 3 & !is.na(Month))
sum(sel, na.rm = TRUE); mean(sel, na.rm = TRUE)   # 9234 = 8.2%
```

```
## [1] 12152
```

```
## [1] 0.3289125
```

```r
yr[sel] <- yr[sel]-1

# We REDEFINE Year as measurement year
df_nifes_cod2$Year <- yr
```

## 8. Some tables

```r
# Calender year (År) vs month
xtabs(~addNA(År) + addNA(Month), df_nifes_cod2)
```

```
##          addNA(Month)
## addNA(År)    1    2    3    4    6    9   10   11   12 <NA>
##      2010    0    0 7252 2450 2450    0    0    0    0 2450
##      2011    0 2450    0    0    0    0    0    0    0    0
##      2012    0    0    0    0    0 4900 2254    0    0    0
##      2013    0    0    0    0    0 2450    0    0    0    0
##      2014    0    0    0    0    0    0 2156    0    0    0
##      2015    0    0    0    0    0    0    0 2450    0    0
##      2016    0    0    0    0    0    0    0 2450  784    0
##      2018 2450    0    0    0    0    0    0    0    0    0
##      <NA>    0    0    0    0    0    0    0    0    0    0
```

```r
# Calender year (År) vs measurement year (Year)
xtabs(~addNA(År) + addNA(Year), df_nifes_cod2)   
```

```
##          addNA(Year)
## addNA(År) 2009 2010 2012 2013 2014 2015 2016 2017 <NA>
##      2010 7252 7350    0    0    0    0    0    0    0
##      2011    0 2450    0    0    0    0    0    0    0
##      2012    0    0 7154    0    0    0    0    0    0
##      2013    0    0    0 2450    0    0    0    0    0
##      2014    0    0    0    0 2156    0    0    0    0
##      2015    0    0    0    0    0 2450    0    0    0
##      2016    0    0    0    0    0    0 3234    0    0
##      2018    0    0    0    0    0    0    0 2450    0
##      <NA>    0    0    0    0    0    0    0    0    0
```

```r
# All 2010,2018 data and much 2011 data are in the measurement year before

# Areas
xtabs(~addNA(Uttaksområde) + addNA(Year), df_nifes_cod2)   
```

```
##                    addNA(Year)
## addNA(Uttaksområde) 2009 2010 2012 2013 2014 2015 2016 2017 <NA>
##         Møre           0 4900 2254    0    0    0  784    0    0
##         Træna          0    0    0    0    0    0    0 2450    0
##         Vestfjorden 2450 2450 2450 2450    0    0    0    0    0
##         Vikna       4802 2450 2450    0 2156 2450 2450    0    0
##         <NA>           0    0    0    0    0    0    0    0    0
```

## 9. Create df_median and set parameter names  
Note that we use Flag = "" for data over LOQ (not ">" as in the original script)  

```r
df_median <- df_nifes_cod2 %>% 
  group_by(Year, Uttaksområde, Organ, Parameter) %>%
  summarise(Conc = median(Conc, na.rm = TRUE), N = n(), Over_LOQ = sum(Flag %in% ""))
head(df_median, 3)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Year"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Uttaksområde"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Organ"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Parameter"],"name":[4],"type":["chr"],"align":["left"]},{"label":["Conc"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["N"],"name":[6],"type":["int"],"align":["right"]},{"label":["Over_LOQ"],"name":[7],"type":["int"],"align":["right"]}],"data":[{"1":"2009","2":"Vestfjorden","3":"Filet","4":"Ag","5":"0.001900","6":"25","7":"3"},{"1":"2009","2":"Vestfjorden","3":"Filet","4":"Alder","5":"6.000000","6":"25","7":"25"},{"1":"2009","2":"Vestfjorden","3":"Filet","4":"As","5":"3.445494","6":"25","7":"25"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
# Change parameter names (only those needed for indicators)
sel <- df_median$Parameter %in% "Cd"; sum(sel)
```

```
## [1] 28
```

```r
df_median$Parameter[sel] <- "CD"
sel <- df_median$Parameter %in% "Hg"; sum(sel)
```

```
## [1] 28
```

```r
df_median$Parameter[sel] <- "HG"
sel <- df_median$Parameter %in% "Pb"; sum(sel)
```

```
## [1] 28
```

```r
df_median$Parameter[sel] <- "PB"
sel <- df_median$Parameter %in% "HCB"; sum(sel)    # None
```

```
## [1] 0
```

```r
sel <- df_median$Parameter %in% "BDE6S"; sum(sel)  # OK, doesn't need to be changed
```

```
## [1] 14
```

```r
sel <- df_median$Parameter %in% "pp-DDE"; sum(sel)  # None 
```

```
## [1] 0
```

```r
df_median$Parameter[sel] <- "DDEPP"
sel <- df_median$Parameter %in% "Sum PCB 7"; sum(sel)   # 8
```

```
## [1] 14
```

```r
df_median$Parameter[sel] <- "CB_S7"

# HCB, pp-DDE
cat("Parameters:\n", table(df_median$Parameter) %>% names() %>% paste(collapse = "; "))
```

```
## Parameters:
##  1234678-HpCDD; 1234678-HpCDF; 123478-HxCDD; 123478-HxCDF; 1234789-HpCDF; 123678-HxCDD; 123678-HxCDF; 12378-PeCDD; 12378-PeCDF; 123789-HxCDD; 123789-HxCDF; 234678-HxCDF; 23478-PeCDF; 2378-TCDD; 2378-TCDF; Ag; Alder; As; Ba; BDE6S; CB_S7; CD; Co; Cr; Cu; Fe; Hel fisk lengde; Hel fisk vekt; HG; Lever vekt; Mn; Mo; Ni; OCDD; OCDF; PB; PBDE 100; PBDE 119; PBDE 138; PBDE 153; PBDE 154; PBDE 183; PBDE 28; PBDE 47; PBDE 66; PBDE 99; PCB-101; PCB-105; PCB-114; PCB-118; PCB-123; PCB-126; PCB-138; PCB-153; PCB-156; PCB-157; PCB-167; PCB-169; PCB-180; PCB-189; PCB-28; PCB-52; PCB-77; PCB-81; Se; Sn; Sr; Sum dioksiner/furaner og dl-PCB; Sum mono-ortho PCB; Sum moPCB+noPCB; Sum non-ortho PCB; Sum PBDE 7; Sum PCB 6; Sum PCDD; Sum PCDD+PCDF; Sum PCDF; Totalfett Etylacetat; Tørrstoff %; V; Zn
```

## 10. Save 

```r
# dir.create("Data")

if (save_data){
  saveRDS(df_nifes_cod2, "Data/01_df_nifes_cod2.rds")
  saveRDS(df_median, "Data/01_df_median.rds")
}
```


