#
# FUNCTIONS FOR TREND ANALYSIS
# - copied from https://github.com/DagHjermann/Milkys2/blob/master/001_Add_trends_functions.R
#   but cleaned up by deleting functions not in use
# - Also
#     - added 'check_data' (used in model_from_medians)
#     - fixed error in model_from_medians when asking for plot with ggplot = FALSE
#
# 'Main 'Master' function is 'model_from_medians'  
# - takes data from file of annual medians (df_med), not raw data
# - gam = TRUE by default 
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# OVERVIEW ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# model_from_medians
# - subsets data
# - sets log_transform = TRUE or FALSE
# - runs calc_models_one_station2
#     - runs 'select_for_regr_a()' and 'select_for_regr_b'
#     - runs 'calc_models_gam' (or 'calc_models', if gam = FALSE)
#     - runs 'add_model_fits'
#     - runs 'statistics_for_excel'
# - the result is 'result_object' containing 'statistics_for_file' and  'data_object'
#     - if calc_models_one_station2 fails, 'statistics_for_excel_empty' is run to result in 'result_stat'
# - picks 'statistics_for_file' from this (calls it 'result_stat'), adds PARAM, LATIN_NAME etc. and returns this as a one-line dataframe



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 'model_from_medians' with helper functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Takes data from file of annual medians (df_med), not raw data
# Note: here gam = TRUE by default (in contrast to model_from_leveldata)
#
# Variable names:
# Hard-coded variable names that must be in the file of medians: 
#   PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, MYEAR
#   'Over_LOQ', 'N', 'SD'
#     ('SD' is used in 'statistics_for_excel_empty' only)
# Variable for median concentration can be chosen using 'varname' (default = 'Value')
#
model_from_medians <- function(param, species, tissue, station, basis, yrs, data_medians, varname = "Value", plotname = "", ggplot = FALSE, gam = TRUE){
  check_data(data_medians, varname)
  df_med <- subset(data_medians, 
                   PARAM %in% param & 
                     LATIN_NAME %in% species &
                     TISSUE_NAME %in% tissue &
                     STATION_CODE %in% station &
                     Basis %in% basis &
                     MYEAR %in% yrs)
  # Change name of response variable to "Median"
  colnames(df_med) <- sub(varname, "Median", colnames(df_med))
  # If we want to excape from the  hard-coded variable names, put varname_n_over_loq as a function argument and do this:
  # colnames(df_med) <- sub(varname_n_over_loq, "Over_LOQ", colnames(df_med))
  if (!param %in% c("VDSI", "VDSI/Intersex", "Intersex", "BAP3O", "Delta13C", "Delta15N")){      # these have median numbers <= 0
    log_transform <- TRUE
  } else {
    log_transform <- FALSE
  }
  result_object <- try(calc_models_one_station2(df_med, gam = gam, log = log_transform))
  # str(result_object, 1)
  if (class(result_object)[1] == "try-error"){
    result_stat <- statistics_for_excel_empty(df_med)
  } else {
    # Result
    result_stat <- result_object$statistics_for_file
    # Plot
    if (plotname != ""){
      if (plotname != "window"){
        fn <- paste0(plotname, "_", param, "_", substr(species, 1, 3), "_", substr(tissue, 1, 2), "_",
                     station, "_", basis, "_", head(yrs,1), "-", tail(yrs,1), ".png")
        png2(fn, width = 7, height = 5)
      }
      tit <- paste0(param, ", ", species, ", ", tissue, ", ",
                    station, ", ", basis, ", ", head(yrs,1), "-", tail(yrs,1))
      if (!ggplot){
        plot_models_one_station(result_object, title = tit, logdata = log_transform)
      } else {
        print(
          plot_models_one_station_gg(result_object, result_stat, title = tit, logdata = log_transform)
        )
      }
      if (plotname != "window")
        dev.off()
    }
  }
  result_stat <- result_stat %>%
    select(-Status, Status)   # Put "Status" as last column
  list(result_object = result_object,
       statistics = 
         cbind(
           data.frame(PARAM = param, 
                      LATIN_NAME = species, 
                      TISSUE_NAME = tissue, 
                      STATION_CODE = station, 
                      Basis = basis, stringsAsFactors = FALSE),
           result_stat,
           N_data = sum(df_med$MYEAR %in% yrs),                # this is the total number of years with data, regardless of data are over LOQ
           log_transformed = log_transform
         )
  )
}




check_data <- function(data, varname){
  needed_vars <- strsplit("PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, MYEAR, Over_LOQ, N, SD", ", ")[[1]]
  found <- needed_vars %in% names(data)
  if (mean(found) < 1){
    cat("Data lacks the following variables: \n")
    cat(needed_vars[!found])
    cat("\n")
    stop("Fix the data and try again")
  }
  found <- varname %in% names(data)
  if (!found){
    stop("'varname' of concetration variable is given as ", varname, "; this variable wasn't found. PLease change 'varname'")
  }
  invisible(NULL)
}


#
# statistics_for_excel_empty
#
# Used to make an empty one-line data frame if there is no rsult from 'statistics_for_excel'
#
statistics_for_excel_empty <- function(df_med){
  X <- data.frame(matrix(NA, 1, 20))
  colnames(X) <- c("Year1", "Year2", "N", "Nplus", "Mean", "p_linear", "p_nonlinear", 
                   "AICc_lin", "AICc_nonlin", "Lin_slope", "Lin_yr1", "Lin_yr2", 
                   "Nonlin_yr1", "Nonlin_yr2", "Over_LOQ_yr2", "Status", "Model_used", "P_change", "Dir_change", "SD_last")
  # if (nrow(df_med) >= 1)
  #   X$SD_last <- tail(df_med$SD, 1)
  # X
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 'calc_models_one_station2' with helper functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# calc_models_one_station2
#
# Differs from "calc_models_one_station" that this one takes "medians_one_station" as input, while
#   "calc_models_one_station" takes "medians_all_stations" as input
#   "calc_models_one_station2" takes "medians_one_station" as input
# The input data should have the variables MYEAR, Median and Over_LOQ
# Also,
#  "calc_models_one_station2" has options gam (= FALSE by default), log (works only if gam = TRUE, is TRUE by default)
#

calc_models_one_station2 <- function(medians_one_station, gam = FALSE, log = TRUE){
  # df_med_regr_20yr <- get_medians_for_regression(medians_all_stations, station = station, yrs = yrs)
  df_med_object <- medians_one_station %>% select_for_regr_a() %>% select_for_regr_b()
  if (!gam){
    modelresults <- calc_models(df_med_object)
  } else {
    modelresults <- calc_models_gam(df_med_object, log = log)
  }
  df_med_object <- add_model_fits(df_med_object, modelresults, gam = gam)
  # df1 <- data_for_excel(medians_all_stations, station=station)
  df2 <- statistics_for_excel(df_med_object, modelresults, gam = gam)
  list(statistics_for_file = df2, data_object = df_med_object, modelresults = modelresults)
}



#
# The data frame must have the variable "Over_LOQ" which equals the number of measurements over LOQ
# Returns a list including the original data and "sel_ts" which is the observations picked for regression analysis
#
select_for_regr_a <- function(df_med_st){
  sel_ts <- rep(TRUE, nrow(df_med_st))
  N <- sum(sel_ts)
  Nplus <- sum(df_med_st$Over_LOQ[sel_ts] > 0)
  # cat(which(sel_ts), "N =", N, "Nplus = ", Nplus, "\n")
  i <- 1
  while (Nplus < (N/2)){
    sel_ts[i] <- FALSE
    N <- sum(sel_ts)
    Nplus <- sum(df_med_st$Over_LOQ[sel_ts] > 0)
    i <- i+1
  }
  # cat(which(sel_ts), "N =", N, "Nplus = ", Nplus, "\n")
  while (Nplus >= 5 & df_med_st$Over_LOQ[sel_ts][1] == 0){
    sel_ts[i] <- FALSE
    i <- i+1
  }
  # cat(which(sel_ts), "N =", N, "Nplus = ", Nplus, "\n")
  list(df_med_st = df_med_st, sel_ts = sel_ts, N = N, Nplus = Nplus)
}

#
# The data frame must have the variable "MYEAR"
# Returns same type of list as select_for_regr_a
#
select_for_regr_b <- function(sel_object){
  df_med_st <- sel_object$df_med_st
  sel_ts <- sel_object$sel_ts
  # Rule 3: If yr_last is the last year with over-LOQ data, the years after are set to yr_last
  # We let MYEARS stay unchanged and change MYEAR_regr
  df_med_st$MYEAR_regr <- df_med_st$MYEAR
  df_med_st[sel_ts,]
  index_last_over_LOQ <- tail(which(df_med_st$Over_LOQ > 0), 1)
  # for testing
  # index_last_over_LOQ <- 8
  index_after_last_over_LOQ <- which(sel_ts)[which(sel_ts) > index_last_over_LOQ] 
  index_after_last_over_LOQ
  df_med_st$MYEAR_regr[index_after_last_over_LOQ] <- df_med_st$MYEAR[index_last_over_LOQ] 
  list(df_med_st = df_med_st, sel_ts = sel_ts, N = sel_object$N, Nplus = sel_object$Nplus)
}


#
# Adds model fits to the output of 'calc_models_gam' (or to be more precise, to the 'df_med_st' part of it)
#
add_model_fits <- function(obj, regr_results, gam = FALSE){
  df <- obj$df_med_st
  df$Fit_lin <- NA
  df$Fit_nonlin <- NA
  median_exists <- is.finite(obj$df_med_st$Median)
  if (!is.null(regr_results$mod_lin$fitted))
    # df$Fit_lin[obj$sel_ts][median_exists] <- regr_results$mod_lin$fitted    # the !is.na selection weeds out NAs in the middle of the time series
    df$Fit_lin[obj$sel_ts & median_exists] <- regr_results$mod_lin$fitted    # the !is.na selection weeds out NAs in the middle of the time series
  if (!is.null(regr_results$mod_nonlin) & !gam)
    df$Fit_nonlin[obj$sel_ts] <- regr_results$mod_nonlin$yFit$Estimate
  if (!is.null(regr_results$mod_nonlin) & gam & regr_results$status == "GAM OK")
    # df$Fit_nonlin[obj$sel_ts][median_exists] <- regr_results$mod_nonlin$yFit$Estimate
    df$Fit_nonlin[obj$sel_ts & median_exists] <- regr_results$mod_nonlin$yFit$Estimate
  updated_obj <- obj
  updated_obj$df_med_st <- as.data.frame(df)
  updated_obj
}

# df_med_object <- add_model_fits(df_med_object, modelresults_20yr)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 'calc_models_gam' with helper functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Calculate gam models
# Based on 'models_yr', but it uses the entire input data for regression
calc_models_gam <- function(obj, var_x = "MYEAR_regr", var_y = "Median", gam = FALSE, log = TRUE){
  data <- obj$df_med_st[obj$sel_ts,]
  data_pick <- data[, c(var_x, var_y)]
  if (log)
    data_pick[,2] <- log(data_pick[,2])
  colnames(data_pick) <- c("x", "y")
  if (sum(is.finite(data_pick$y)) > 0){
    mod_gam <- try(GAM_trend_analysis(dataset = data_pick))
    mod_lin <- lm(y~x, data = data_pick)
    pr <- predict(mod_lin, se = TRUE)
    dY <- -qt(0.025, nrow(data_pick))
    mod_lin_yFit <- data.frame(Year = data_pick$x[!is.na(data_pick$y)], Estimate = pr$fit, SE = pr$se.fit, LowLimit = pr$fit - dY*pr$se.fit, HighLimit = pr$fit + dY*pr$se.fit)  
    if (class(mod_gam)[1] != "try-error"){
      result <- 
        list(AIC = c(nonlinear = AIC(mod_gam$model),
                     linear = AIC(mod_lin)),
             AICc = c(nonlinear = AICc(mod_gam$model),
                      linear = AICc(mod_lin)),
             mod_nonlin = mod_gam,
             mod_lin = mod_lin,
             mod_lin_yFit = mod_lin_yFit,
             status = "GAM OK",
             data = data_pick,
             stringsAsFactors = FALSE)
    } else {
      result <- 
        list(AIC = c(nonlinear = NA,
                     linear = AIC(mod_lin)),
             AICc = c(nonlinear = NA,
                      linear = AICc(mod_lin)),
             mod_nonlin = NA,
             mod_lin = mod_lin,
             mod_lin_yFit = mod_lin_yFit,
             status = "GAM failed",
             data = data_pick,
             stringsAsFactors = FALSE)
    }
  } else {
    result <- list(AIC = c(nonlinear = NA,
                           linear = NA),
                   AICc = c(nonlinear = NA,
                            linear = NA),
                   mod_nonlin = NULL,
                   mod_lin = NULL,
                   mod_lin_yFit = NULL,
                   mod_gam = NULL,
                   status = "No qualified data",
                   data = data_pick,
                   stringsAsFactors = FALSE)
  }  
  result
}



GAM_trend_analysis <-  function(dataset){
  sel <- is.finite(dataset$x) & is.finite(dataset$y)  
  max_df <- ifelse(sum(sel) <= 14, 3, 4)
  mgam <- mgcv::gam(y ~ s(x, k = max_df + 1), data = dataset[sel,])
  pr <- predict(mgam, se.fit = TRUE)
  dY <- -qt(0.025, nrow(dataset))
  yFit <- data.frame(Year = dataset$x[sel], Estimate = pr$fit, SE = pr$se.fit, LowLimit = pr$fit - dY*pr$se.fit, HighLimit = pr$fit + dY*pr$se.fit)  
  list(model = mgam, yFit = yFit)
}




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 'statistics_for_excel' with helper functions----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Called from 'calc_models_one_station2'  
# Uses the medians (obj) + the results from calc_models_gam (regr_results)  
#
statistics_for_excel <- function(obj, regr_results, gam = FALSE){
  if (sum(obj$sel_ts) > 0 & !(gam & regr_results$status == "GAM OK")){
    df_stat <- data.frame(
      Year1 = min(obj$df_med_st$MYEAR[obj$sel_ts]),
      Year2 = max(obj$df_med_st$MYEAR[obj$sel_ts]),
      N = obj$N,
      Nplus = obj$Nplus,
      Mean = mean(obj$df_med_st$Median, na.rm = TRUE),
      p_linear = p_linear(regr_results$mod_lin),
      p_nonlinear = NA,  # p_nonlinear(regr_results$mod_nonlin),
      AICc_lin = regr_results$AICc[["linear"]],
      AICc_nonlin = NA, # regr_results$AICc[["nonlinear"]],
      Lin_slope = coef(regr_results$mod_lin)[2],
      Lin_yr1 = head(regr_results$mod_lin$fitted, 1),
      Lin_yr2 = tail(regr_results$mod_lin$fitted, 1),
      Nonlin_yr1 = NA, # head(regr_results$mod_nonlin$yFit$Estimate, 1),
      Nonlin_yr2 = NA, # tail(regr_results$mod_nonlin$yFit$Estimate, 1),
      Over_LOQ_yr2 = tail(obj$df_med_st[obj$sel_ts, "Over_LOQ"], 1),
      Status = regr_results$status,
      stringsAsFactors = FALSE
    )
  } else if (sum(obj$sel_ts) > 0 & gam & regr_results$status == "GAM OK"){
    # Calculate p-value for GAM model (gam_p)
    x1 <- head(regr_results$mod_nonlin$yFit$Estimate,1)
    x2 <- tail(regr_results$mod_nonlin$yFit$Estimate,1)
    SE1 <- head(regr_results$mod_nonlin$yFit$SE,1)
    SE2 <- tail(regr_results$mod_nonlin$yFit$SE,1)
    mean_SE <- sqrt(SE1^2 + SE2^2)
    n_fit <- nrow(regr_results$mod_nonlin$yFit)
    gam_p <- 2*(1 - pt(abs(x2-x1)/(1.5*mean_SE), n_fit))
    df_stat <- data.frame(
      Year1 = min(obj$df_med_st$MYEAR[obj$sel_ts]),
      Year2 = max(obj$df_med_st$MYEAR[obj$sel_ts]),
      N = obj$N,
      Nplus = obj$Nplus,
      Mean = mean(obj$df_med_st$Median, na.rm = TRUE),
      p_linear = p_linear(regr_results$mod_lin),
      p_nonlinear = gam_p,
      AICc_lin = regr_results$AICc[["linear"]],
      AICc_nonlin = AICc(regr_results$mod_nonlin$model),
      Lin_slope = coef(regr_results$mod_lin)[2],
      Lin_yr1 = head(regr_results$mod_lin$fitted, 1),
      Lin_yr2 = tail(regr_results$mod_lin$fitted, 1),
      Nonlin_yr1 = head(regr_results$mod_nonlin$yFit$Estimate, 1),
      Nonlin_yr2 = tail(regr_results$mod_nonlin$yFit$Estimate, 1),
      Over_LOQ_yr2 = tail(obj$df_med_st[obj$sel_ts, "Over_LOQ"], 1),
      Status = regr_results$status,
      stringsAsFactors = FALSE
    )
    if (SE1 < 0.00001 & SE2 < 0.00001)
      df_stat$Status <- "No variation in data"
  } else {
    df_stat <- data.frame(
      Year1 = NA,
      Year2 = NA,
      N = obj$N,
      Nplus = obj$Nplus,
      Mean = mean(obj$df_med_st$Median, na.rm = TRUE),
      p_linear = NA,
      p_nonlinear = NA,
      AICc_lin = NA,
      AICc_nonlin = NA,
      Lin_slope = NA,
      Lin_yr1 = NA,
      Lin_yr2 = NA,
      Nonlin_yr1 = NA,
      Nonlin_yr2 = NA,
      Over_LOQ_yr2 = NA,
      Status = regr_results$status,
      stringsAsFactors = FALSE
    )
  }
  df_stat$Model_used <- select_model(df_stat)
  if (df_stat$Model_used == "Linear"){
    df_stat$P_change <- df_stat$p_linear
  } else if (df_stat$Model_used == "Nonlinear"){
    df_stat$P_change <- df_stat$p_nonlinear
  } else {
    df_stat$P_change <- NA
  }
  if (df_stat$Status == "No variation in data"){
    df_stat$Dir_change <- ""
  } else if (df_stat$P_change < 0.05 & df_stat$Model_used == "Linear"){
    df_stat$Dir_change <- ifelse(df_stat$Lin_slope > 0, "Up", "Down")
  } else if (df_stat$P_change < 0.05 & df_stat$Model_used == "Nonlinear"){
    df_stat$Dir_change <- ifelse(df_stat$Nonlin_yr2 > df_stat$Nonlin_yr1, "Up", "Down")
  } else {
    df_stat$Dir_change <- ""
  }
  if (is.na(df_stat$p_linear) & is.na(df_stat$p_nonlinear) & df_stat$Status != "No variation in data")
    df_stat$Status <- "Linear regr. and GAM failed"
  df_stat
}

#
# Returns p-value for increase using non-linear model (ie., last minus first point) 
#
p_nonlinear <- function(model){
  nlin_pred <- model$yFit
  first_point <- head(nlin_pred, 1)
  last_point <- tail(nlin_pred, 1)
  diff <- last_point$Estimate - first_point$Estimate
  s_comb <- sqrt(first_point$Variance + last_point$Variance)
  (1-pt(diff/s_comb, 1))/2
}

#
# Returns p-value for increase using linear model (ordinary p-value) 
#
p_linear <- function(model){
  summary(model)$coef[2,4]
}


# Select 
select_model <- function(df_statistics, nonlin_penalty = 0.05){
  if (df_statistics$Nplus <= 1){
    model <- "None"
  } else if (df_statistics$Nplus == 2 & df_statistics$N == 2){
    model <- "None"
  } else if (df_statistics$Nplus %in% 2:4 & df_statistics$N >= 3){
    model <- "Mean"
  } else if (df_statistics$Nplus %in% 5:6){
    model <- "Linear"
  } else if (df_statistics$Nplus >= 7 & !is.na(df_statistics$AICc_nonlin)){
    # We select the model with the lowest AIC after giving the non-linear model a little penalty
    # The main reason for that penalty is to select the linear model if AICs are the same
    #   (which is the case if the GAM odel in reality is linear)
    if (df_statistics$AICc_lin <= (df_statistics$AICc_nonlin + nonlin_penalty)){
      model <- "Linear"
    } else {
      model <- "Nonlinear"
    }
  } else if (df_statistics$Nplus >= 7 & is.na(df_statistics$AICc_nonlin)){
    model <- "Linear"
  } else {
    model <- "Error"
  }
  model
}






#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Plot functions----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Plot data (medians)
#   input =  output from 'calc_models_one_station2 '
#

#
# NOTE
#
plot_models_one_station <- function(result_object, logdata = FALSE, logscale = TRUE, title = "",
                                    colors = c("red", "blue")){
  if (logdata){
    plot(log(Median)~MYEAR, result_object$data_object$df_med_st, type = "n", main = title)
    points(log(Median)~MYEAR, result_object$data_object$df_med_st[result_object$data_object$sel_ts,], pch = 21, bg = "blue3")
    points(log(Median)~MYEAR, result_object$data_object$df_med_st[!result_object$data_object$sel_ts,], pch = 21, bg = "indianred1")
    lines(Fit_lin~MYEAR, result_object$data_object$df_med_st, lty = "dashed", col = colors[1])
    lines(Fit_nonlin~MYEAR, result_object$data_object$df_med_st, lty = "dotted", col = colors[2])
  } else if (!logdata){
    if (!logscale){
      plot(Median~MYEAR, result_object$data_object$df_med_st, main = title)
    } else {
      plot(Median~MYEAR, result_object$data_object$df_med_st, main = title, log = "y")
    }
    points(Median~MYEAR, result_object$data_object$df_med_st[result_object$data_object$sel_ts,], pch = 21, bg = "blue3")
    points(Median~MYEAR, result_object$data_object$df_med_st[!result_object$data_object$sel_ts,], pch = 21, bg = "indianred1")
    lines(exp(Fit_lin)~MYEAR, result_object$data_object$df_med_st, lty = "dashed", col = colors[1])
    lines(exp(Fit_nonlin)~MYEAR, result_object$data_object$df_med_st, lty = "dotted", col = colors[2])
  }
}



plot_models_one_station_gg <- function(result_object, result_stat, title = "", logdata = TRUE){
  df <- result_object$data_object$df_med_st
  df$Included_in_trend <- "Included"
  df$Included_in_trend[!result_object$data_object$sel_ts] <- "Not included"
  subtitle1 = paste0(
    "Number of years: ", result_stat$N, 
    " (years with enough data over LOQ: ", result_stat$Nplus, ")")
  subtitle2a = paste0(
    "Linear model: p =", round_p(result_stat$p_linear, stars = TRUE))
  subtitle2b = paste0(
    "Non-lin model: p =", round_p(result_stat$p_nonlinear, stars = TRUE))
  trendresult <- case_when(
    result_stat$Dir_change == "Up" ~ "Upward trend",
    result_stat$Dir_change == "Down" ~ "Downward trend",
    result_stat$Dir_change == "" & result_stat$Status == "GAM OK" ~ "No time trend",
    result_stat$Status != "GAM OK" ~ result_stat$Status,
  )
  subtitle3 = paste0(
    "Best model: ", result_stat$Model_used, 
    " (Difference in AIC: ", round(abs(result_stat$AICc_lin - result_stat$AICc_nonlin), 2), ")"
  )
  subtitle4 = paste0(
    "Time trend result: ", trendresult, 
    " (P for trend: ", round_p(result_stat$P_change, stars = TRUE), ")"
  )
  if (log){
    gg <- ggplot(result_object$data_object$df_med_st, aes(MYEAR, log(Median)))
  } else {  
    gg <- ggplot(result_object$data_object$df_med_st, aes(MYEAR, Median))
  }
  gg <- gg + 
    geom_point(aes(color = Over_LOQ)) + 
    # geom_smooth(se = FALSE) +
    geom_line(aes(y = Fit_lin), color = "darkgreen", size = 2) +
    geom_line(aes(y = Fit_nonlin), color = "darkred", size = 2) +
    geom_line(data = result_object$modelresults$mod_nonlin$yFit, aes(x = Year, y = LowLimit), color = "darkred", linetype = 2) +
    geom_line(data = result_object$modelresults$mod_nonlin$yFit, aes(x = Year, y = HighLimit), color = "darkred", linetype = 2) +
    # geom_line(data = result_object$modelresults$mod_gam$yFit, aes(x = Year, y = Estimate), color = "blue", size = 2) +
    labs(title = title, 
         subtitle = paste0(subtitle1, "\n", subtitle2a, "\n", subtitle2b, "\n", subtitle3, "\n", subtitle4))
  gg
}

#
# Unitily functions
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Utility functions----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# "Round" p-values (returns text) - COPIED FROM https://github.com/DagHjermann/Milkys2/blob/master/002_Utility_functions.R
#
round_p <- function(x, stars = FALSE){
  text <- case_when(
    x >= 0.2 ~ sprintf("%.1f", x),
    x >= 0.06 ~ sprintf("%.2f", x),
    x >= 0.001 ~ sprintf("%.3f", x),
    x >= 0.0001 ~ sprintf("%.4f", x),
    x < 0.0001 ~ "<0.0001"
  )
  if (stars){
    text <- case_when(
      text <= 0.001 ~ paste(text, "***"),
      text <= 0.01 ~ paste(text, "**"),
      text <= 0.05 ~ paste(text, "*"),
      text <= 0.1 ~ paste(text, "(*)"),
      text > 0.1 ~ text
    ) 
  }
  text
}

# TEST
if (FALSE){
  round_p(0.121232)
  round_p(0.0121232)
  round_p(0.00121232)
  round_p(0.000121232)
  round_p(0.121232, stars = TRUE)
  round_p(0.0121232, stars = TRUE)
  round_p(0.00121232, stars = TRUE)
  round_p(0.000121232, stars = TRUE)
}


