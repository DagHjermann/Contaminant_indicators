#
# Conditions for functions to work:
# - Column no 1 is called "Jnr"
# - Column no 2 is called "Prøvenr."
# - Then there are x more columns (position, species etc.)
# - Then concentrations, given in column pairs for each substance, where 
#    + column 1 has concentration
#    + column 2 is called "Enhet", "Enhet__1" or similar (starting with "Enhet") 
#
# All columns should be in text format
#

#
# Put concentrations on long format
# Old version. Problem: doesn't include data columns that are not followed by a "Unit" column
# - such as the second CB118 column  
#
nifes_standardize_conc_OLD <- function(data){
  # Next 3 lines is identical in nifes_standardize_conc() and nifes_standardize_unit()
  colnumber_unit <- grep("^Enhet", colnames(data))
  colnumber_conc <- colnumber_unit - 1
  colnumber_meta <- 1:(colnumber_conc[1]-1)
  # Reformat
  data[c(colnumber_meta, colnumber_conc)] %>%
    pivot_longer(-(Jnr:`Produkt beskrivelse`), names_to = "Parameter", values_to = "Conc_chr")
}

#
# Put concentrations on long format
# New version - includes all concentration columns, also those not followed by a "Unit" column
#   (unless the first concentration column is not followed by a "Unit" column, see 'column_conc_1')
#
nifes_standardize_conc <- function(data){
  # Next 3 lines is identical in nifes_standardize_conc() and nifes_standardize_unit()
  colnumber_unit <- grep("^Enhet", colnames(data))
  column_conc_1 <- colnumber_unit[1]-1    # define first colun as the first column followed by a "Unit" column
  column_conc_2 <- ncol(data)
  colnumber_conc <- seq(column_conc_1, column_conc_2)
  colnumber_conc <- colnumber_conc[!colnumber_conc %in% colnumber_unit]
  colnumber_meta <- 1:(colnumber_conc[1]-1)
  # Reformat
  data[c(colnumber_meta, colnumber_conc)] %>%
    pivot_longer(-(Jnr:`Produkt beskrivelse`), names_to = "Parameter", values_to = "Conc_chr")
}
# nifes_standardize_conc(dat1) %>% select(Jnr, Prøvenr., Parameter, Conc_chr)


# nifes_standardize_conc(dat1) %>% select(Jnr, Prøvenr., Parameter, Conc_chr)

#
# Put units on long format
#
nifes_standardize_unit <- function(data){
  # Next 3 lines is identical in nifes_standardize_conc() and nifes_standardize_unit()
  colnumber_unit <- grep("^Enhet", colnames(data))
  colnumber_conc <- colnumber_unit - 1
  colnumber_meta <- 1:(colnumber_conc[1]-1)
  # Pick columns
  data_unit <- data[c(1:2, colnumber_unit)]
  # Set column names (which now are "Enhet__1" etc.) to the corresponding actual name
  colnames(data_unit)[-c(1,2)] <- colnames(data)[colnumber_conc]
  # Reformat
  data_unit %>%
    pivot_longer(-c(Jnr,Prøvenr.), names_to = "Parameter", values_to = "Unit")
}
# nifes_standardize_unit(dat3) %>% select(Jnr, Prøvenr., Parameter, Unit)

nifes_standardize_data <- function(data){
  data_conc <- nifes_standardize_conc(data)
  data_unit <- nifes_standardize_unit(data)
  data <- left_join(data_conc, data_unit, by = c("Jnr","Prøvenr.","Parameter"))
  # Remove less-than sign and make Conc variable
  data <- data %>%
    mutate(
      Conc = sub("<", "", Conc_chr, fixed = TRUE) %>% 
        sub("*", "", ., fixed = TRUE) %>%
        as.numeric(),
      # Find less-thans and make Flag variable
      Flag = ifelse(grepl("<", Conc_chr), "<", ""),
      Comment = ifelse(grepl("*", Conc_chr, fixed = TRUE), "*", "")
    )
  data
}

# test <- nifes_standardize_data(dat3) %>% select(Jnr, Parameter, Conc_chr, Conc, Unit, Flag)
# test

