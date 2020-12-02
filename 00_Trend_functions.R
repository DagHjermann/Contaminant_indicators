





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



# Calculate GAM models
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

# Result for 10-year series, 2019:
#                              addNA(Dir_change)
# addNA(Status)                      Down   Up <NA>
#   GAM failed                    73    0    0    0
#   GAM OK                      5153  857  297    0
#   Linear regr. and GAM failed 1216    0    0    0
#   No variation in data         186    0    0    0
#   <NA>                           0    0    0    0


