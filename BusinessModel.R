# Script to calculate game performance from KPI inputs

# simulate at a player level, not at a cohort level.

# convert marketing investment into installs for a unit of time


# month_to_day <- 
# 
# day_to_month <- 
#   
# day_to_year <- 

time_converter <- function(x) {
  quarterly <- rep(x*3,8)
  daily <- rep(x/30,730)
  monthly <- rep(x, 24)
  yearly <- rep(x*12,2)
  return(list(daily=daily, monthly=monthly, quarterly=quarterly, yearly=yearly))
}

get_installs <- function(investment, cpm, ipm) {
  set.seed(123432)
  ipm <- min(1000,ipm)
  
  impressions <-  (1000 * investment) / cpm
  installs <- sum(sample(c(1,0), impressions, replace = T, prob = c(ipm/1000,1-ipm/1000)))
  
  return(installs)
  
}


acquire_ngu <- function(days, investment, cpm, ipm) {
  
  ngu <- rep(NA, days)
  
  for(i in 1:days) {
    ngu[i] <- get_installs(investment, cpm, ipm)
  }
  return(ngu)
}

apply_kfactor <- function(kfactor, mkg_ngu) {
  
  total_ngu <- mkg_ngu*kfactor
  return(total_ngu)
}

inst <- acquire_ngu(730, 100, 11, 15)


weights <- read.csv("weight_values.csv")
ltv_weights <- weights$ltv
retention_weights <- weights$retention



retention_input <- c(50, 20, 7, 3, 1, 0.5)
names(retention_input) <- c("D1", "D7", "D30", "D180", "D365", "D730")


get_retention_curve <- function(inputs, weights) {
  
  curve_limit <- 730
  retention_curve <- rep(NA, curve_limit)
  input_days <- c(1,7,30,180,365,730)
  
  for(i in 1:730) {
    if(i %in% input_days) {
        retention_curve[i] <- inputs[match(i, input_days)]
      } else {

        last_input_day <- max(input_days[input_days<i])
        last_input <- inputs[match(last_input_day, input_days)]
        next_input_day <- min(input_days[input_days>i])
        next_input <- inputs[match(next_input_day, input_days)]
        last_retention_value <- retention_curve[i-1]
        current_weight <- weights[i]
        sum_weights_tranche <- sum(weights[(last_input_day+1):next_input_day])
      
      retention_curve[i] <- last_retention_value - (last_input - next_input) * (current_weight / sum_weights_tranche)
    }
  }
  
  return(retention_curve)
}

# prov <- get_retention_curve(retention_input, retention_weights)

get_dau <- function(installs, retention) {
  
  days <- length(installs)
  daus <- rep(NA, days)
  retention <- c(100, retention)/100
  
  for(i in 1:days) {
    daus[i] <- sum(rev(installs[1:i])*retention[1:i])
  }
  return(daus)
}

get_dau_r <- function(installs, retention) {
  
  days <- length(installs)
  tenure <- 731
  daus <- matrix(rep(0,days*days), days, days)
  
  cohort_quality <- rnorm(days,0,0.07)
  retention_matrix <- (t(retention*matrix(rep(1,730*730), 730, 730)))*(1+cohort_quality)
  retention_adj <- cbind( rep(100,730), retention_matrix)/100
  
  for(i in 1:days) {
    for(j in 1:(days-i+1)){
      daus[i,j+i-1] <- round(installs[i]*retention_adj[i,j])
    }
  }
  return(daus)
}

# d <- get_dau(inst, prov)
# e <- get_dau_r(inst, prov)


ltv_input <- c(0.2, 0.4, 0.55, 0.65, 0.75, 0.8, 0.85)
names(ltv_input) <- c("D1", "D7", "D30", "D60", "D180", "D365", "D730")


get_ltv_curve <- function(inputs, weights) {
  
  curve_limit <- 730
  ltv_curve <- rep(NA, curve_limit)
  input_days <- c(1,7,30,180,365,730)
  
  for(i in 1:730) {
    if(i %in% input_days) {
      ltv_curve[i] <- inputs[match(i, input_days)]
    } else {
      
      last_input_day <- max(input_days[input_days<i])
      last_input <- inputs[match(last_input_day, input_days)]
      next_input_day <- min(input_days[input_days>i])
      next_input <- inputs[match(next_input_day, input_days)]
      last_ltv_value <- ltv_curve[i-1]
      current_weight <- weights[i]
      sum_weights_tranche <- sum(weights[(last_input_day+1):next_input_day])
      
      ltv_curve[i] <- last_ltv_value + (next_input - last_input) * (current_weight / sum_weights_tranche)
    }
  }
  return(ltv_curve)
}

# cheque <- get_ltv_curve(ltv_input, retention_weights)

get_revenue <- function(installs, ltv) {
  
  days <- length(installs)
  revenue <- matrix(rep(0,days*days), days, days)
  
  cohort_quality <- abs(rnorm(days,0,0.2))
  ltv_matrix <- (t(ltv*matrix(rep(1,730*730), 730, 730)))*(1+cohort_quality)
  ltv_adj <- cbind( 0.7*ltv_matrix[,1], ltv_matrix)
  
  for(i in 1:days) {
    for(j in 1:(days-i+1)){
      revenue[i,j+i-1] <- round(installs[i]*ltv_adj[i,j])
    }
  }
  revenue <- cbind(revenue[,1], revenue[,2:days] - revenue[,1:(days-1)])
  return(revenue)
}

from_daily_to_quarterly <- function(x) {
  quarterly <- rep(NA, 8)
  groups <- c(rep(1,91),rep(2,91),rep(3,91),rep(4,91),rep(5,91),rep(6,91),rep(7,92),rep(8,92))
  for(i in 1:8) {
      quarterly[i] <- sum(x[groups==i])
  } 
  return(quarterly)
}

from_daily_to_monthly <- function(x) {
  monthly <- rep(NA, 24)
  groups <- c(sort(rep(1:14,30)), sort(rep(15:24,31)))
  for(i in 1:24) {
    monthly[i] <- sum(x[groups==i])
  }
  return(monthly)
}

from_quarterly_to_yearly <- function(x) {
  yearly <- c(sum(x[1:4]), sum(x[5:8]))
  return(yearly)
}

get_monthly_cashflow <- function(init_cash, revenue, marketing, opex) {
  cash_balance_bom <- rep(NA, 24)
  cash_balance_eom <- rep(NA, 24)
  cash_balance_bom[1] <- init_cash

  for(i in 1:24) {
    cash_balance_eom[i] = cash_balance_bom[i] + revenue[i] - marketing[i] - opex[i]
    if(i < 25) {cash_balance_bom[i+1] <- cash_balance_eom[i]}
  }
  return(cash_balance_eom)
}

get_cash_info <- function(monthly_cashflow) {
  capital <- min(monthly_cashflow)
  if(capital > 0) {capital <- 0} 
  # you will run out of cash in month number...
  months <- which(monthly_cashflow <= 0)[1]
  return(list(capital=-capital, months=months))
}
#summarize_revenue


######### UNDERSTANDING LTV AND REVENUE DATA

input_conversion <- c(0.5,1,1.5,2)


get_conv_curve <- function(inputs, weights) {
  
  inputs <- inputs / 100
  curve_limit <- 180
  weights <- weights[1:curve_limit]
  conv_curve <- rep(NA, curve_limit)
  input_days <- c(1,7,30,180)
  
  for(i in 1:curve_limit) {
    if(i %in% input_days) {
      conv_curve[i] <- inputs[match(i, input_days)]
    } else {
      
      last_input_day <- max(input_days[input_days<i])
      last_input <- inputs[match(last_input_day, input_days)]
      next_input_day <- min(input_days[input_days>i])
      next_input <- inputs[match(next_input_day, input_days)]
      last_conv_value <- conv_curve[i-1]
      current_weight <- weights[i]
      sum_weights_tranche <- sum(weights[(last_input_day+1):next_input_day])
      
      conv_curve[i] <- last_conv_value + (next_input - last_input) * (current_weight / sum_weights_tranche)
    }
  }
  return(conv_curve)
}

# out of every 100 new game users, how many will have spend money in the game by.. after 1 day of playing, x days...
# out of every 100 customers, how many will spend the following amounts on a monthly basis??

acquire_payers <- function(ngu, conv_c) {
  
  
}


# guess the metrics behind each creative