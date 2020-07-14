
esti_r <- function(source, si_mean, si_sd, d_min, d_max, r_y, c_y){
  
  data <- source
  
  # feeding in terms into the main function and extracting cases and dates
  R_results <- estimate_R(data$cases,
                          method = "parametric_si",
                          config = make_config(list(mean_si = si_mean,
                                                    std_si = si_sd))) %>%
    extract2(1) %>% 
    mutate(dates = data$dates[8:nrow(data)],
           cases = data$cases[8:nrow(data)])

  # date limites
  date_limits <- as.Date(c(d_min, d_max))
  
  # y axis height for R
  heightcheck_r <- R_results %>% 
    filter(dates >= as.Date(d_min) & dates <= as.Date(d_max)) %>%
    summarise(max = max(`Mean(R)`)) %>%
    extract2(1)
  
  # y axis height for cases
  heightcheck_c <- R_results %>% 
    filter(dates >= as.Date(d_min) & dates <= as.Date(d_max)) %>%
    summarise(max = max(cases)) %>%
    extract2(1)

  # setting heights
  # if R chart height set by user, use user input
  if(is.numeric(r_y)){
    
    y_max_r = r_y

    # else set as 3.8 or 1.2 times the mean R
  } else {
    
    y_max_r <- ifelse(heightcheck_r <= 3.8,
                      heightcheck_r * 1.2,
                      3.8 * 1.2)
  }
  
  # same for y axis height for cases
  if(is.numeric(c_y)){
    
    y_max_c = c_y

  } else {
    
    y_max_c <- heightcheck_c * 1.2
    
  }
  
  p_r <- ggplot(R_results, aes(dates, `Mean(R)`)) +
    geom_ribbon(aes(ymin = `Quantile.0.05(R)`, 
                    ymax = `Quantile.0.95(R)`), fill = "grey70") +
    geom_line() +
    geom_hline(yintercept=1, linetype="dashed", color = "red") +
    coord_cartesian(xlim = date_limits, ylim = c(0, y_max_r)) +
    scale_x_date(date_labels = "%b %d") +
    ggtitle("Estimated instantaneous reproduction number") +
    theme_bw() +
    labs(y = "Estimated R (95% CI)") +
    theme_update(plot.margin = margin(10, 10, 10, 10),
                 axis.title.x = element_blank())
  
  p_c <- ggplot(R_results, aes(dates, cases)) +
    geom_col() +
    scale_x_date(date_labels = "%b %d") +
    coord_cartesian(xlim = date_limits, ylim = c(0, y_max_c)) +
    theme_bw() +
    ggtitle("Number of confirmed new cases") +
    labs(y = "Number of new cases") +
    theme_update(plot.margin = margin(10, 10, 10, 10),
                 axis.title.x = element_blank())
   
  plots <- list(p_r, p_c)
  
  return(plots)
  
}







