
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
    mutate(cases = c(sim_start_c, diff(total_cases))) %>%
    filter(x != sim_dur) %>%
    mutate(dates = sim_date_range)
  
  sim_bind$rate[1] <- sim_g + 1
  
  p_c_n <- ggplot(sim_bind) +
    geom_col(aes(dates, cases)) +
    scale_x_date(date_breaks = "5 days", date_labels = "%b %d") +
    theme_bw() +
    ggtitle("Number of simulated new cases") +
    labs(y = "Number of new cases") +
    theme_update(plot.margin = margin(10, 10, 10, 10),
                 axis.title.x = element_blank())
  
  p_c_t <- ggplot(sim_bind) +
    geom_col(aes(dates, total_cases)) +
    scale_x_date(date_breaks = "5 days", date_labels = "%b %d") +
    theme_bw() +
    ggtitle("Total number of simulated cases") +
    labs(y = "Number of total cases") +
    theme_update(plot.margin = margin(10, 10, 10, 10),
                 axis.title.x = element_blank())
  
  p_g_r <- ggplot(sim_bind) +
    geom_point(aes(dates, rate)) +
    geom_vline(xintercept=sim_bind$dates[sim_bind$x==midpoint_1], linetype="dashed", color = "red") +
    scale_x_date(date_breaks = "5 days", date_labels = "%b %d") +
    theme_bw() +
    ggtitle("Growth factor (1 + growth rate) over time") +
    labs(y = "Growth factor") +
    theme_update(plot.margin = margin(10, 10, 10, 10),
                 axis.title.x = element_blank())
  
  ar <- ggarrange(p_g_r, p_c_n, p_c_t, ncol = 1, align = "v")
  
  results_sim <- sim_bind %>%
    select(cases, dates) 

  fill_dates <- seq.Date(last_date + 1, last_date + sim_start_tp - 1, "days")
  
  fill <- data.frame(dates = fill_dates,
                     cases = 0)
  
  data = bind_rows(source, fill, results_sim)

  output <- list(data, ar)
  
  return(output)
  
}




