
# setwd("H:/Documents/seksjon 212/Milkys 2017/Analyse")

#
# Functions
#

#
# Factor to numeric
#
fact2num <- function(x) as.numeric(levels(x)[as.numeric(x)])

#
# Factor to character
#
fact2char <- function(x) levels(x)[as.numeric(x)]

#
# Change all factor variables to characters
#
fact2char_df <- function(df){
  for (i in 1:length(df)){
    if (class(df[[i]])[1] %in% "factor")
      df[[i]] <- fact2char(df[[i]])
    }
  df
  }




#
# Select 'txt' in table where 'param' is one of multiple 'param_values'
# 'txt' can be e.g., 'count(*)' or '*', or a field name 
#
sqlMulti <- function(txt, table, param, param_values){
  qry <- paste0("select ", txt, " from NIVADATABASE.", table, " where ", param, " in (", paste0(param_values, collapse=','), ")")
  sqlQuery(con, qry)
  }

# EXAMPLE
# sqlMulti("count(*)", "WC_PARAMETERS_METHODS", "PARAMETER_ID", c(13, 2915))
# is equivalent to
# sqlQuery(con, "select count(*) from NIVADATABASE.WC_PARAMETERS_METHODS where PARAMETER_ID in (13, 2915)")

#
# As sqlMulti(), but selecting from two columns
# http://stackoverflow.com/questions/13027708/sql-multiple-columns-in-in-clause
# sqlMulti2 DIDN'T WORK
#
sqlMulti2 <- function(txt, table, param1, param_values1, param2, param_values2){
  qry <- paste0("select ", txt, " from NIVADATABASE.", table, 
                " where (", param1, ",", param2, ") in ((", 
                paste0(param_values1, collapse=','), "),(",
                paste0(param_values2, collapse=','), "))"
                )
  # qry
  sqlQuery(con, qry)
  }

# EXAMPLE
# sqlMulti2("*", "BEGALG_PARAMETER_VALUES", "SAMPLE_ID", df_begalg$SAMPLE_ID, "PARAMETER_ID", selpar_begalg)

