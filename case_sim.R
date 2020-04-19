
case_sim <- function(source, delay, duration, new_cases, growth, k_1, k_2, bias){
  
  source <- source
  
  last_date <- as.Date(tail(source$dates,1))
  
  sim_start_tp <- delay
  
  sim_dur <- duration
  
  sim_start_c <- new_cases
  
  sim_g <- growth
  
  k_1 <- k_1
  k_2 <- k_2
  front_bias <- bias
  
  sim_date_range <- seq.Date(from = last_date + sim_start_tp, length.out = sim_dur, by =  "days")
  x_1 <- seq(1, sim_dur, 1)

  
  midpoint_1 <- ifelse(sim_dur %% 2 == 0,
                      sim_dur/2 + front_bias,
                      (sim_dur + 1)/2 + front_bias)

  
  sim <- data.frame(x = x_1) %>%
    mutate(logistic = ifelse(x <= midpoint_1,
                             (-1 / (1 + exp(-k_1 * (x - midpoint_1)))+1),
                             (-1 / (1 + exp(-k_2 * (x - midpoint_1)))+1)),
           rate = logistic * sim_g + 1)
  
  sim_ini <- data.frame(x = 0, logistic = NA, rate = sim_start_c)
  
  sim_bind <- bind_rows(sim_ini, sim) %>%
    mutate(total_cases = ceiling(cumprod(rate))) %>%
    mutate(cases = c(sim_start_c, diff(total_cases)))
  
  # qplot(sim$x, sim$cases)
  # qplot(sim$x, sim$new_cases)
  # qplot(sim$x, sim$logistic)
  
  results_sim <- sim_bind %>%
    filter(x != sim_dur) %>%
    select(cases) %>%
    mutate(dates = sim_date_range)

  
  fill_dates <- seq.Date(last_date + 1, last_date + sim_start_tp - 1, "days")
  
  fill <- data.frame(dates = fill_dates,
                     cases = 0)
  
  data = bind_rows(source, fill, results_sim)

  return(data)
  
}




