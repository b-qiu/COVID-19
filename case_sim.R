
case_sim <- 0


# expansion function


# last date in real data
last_date <- as.Date("2020-04-15")

# days t + 
sim_start_tp <- 5

# new cases
sim_start_c <- 5

# duration of pip
sim_dur <- 20

# constant growth
sim_g <- 4

# ratio between recovery phase and growth phase
sim_rate <- 2

sim_end_date <- last_date + sim_start_tp + sim_dur

sim_date_range <- seq.Date(last_date + 1, sim_end_date)

sim_case_grow_length <- sim_dur * (sim_rate/(1 + sim_rate))

sim_case_decay_length <- sim_dur - sim_case_growh_length

sim_growth_shape <-  
  
sim_decay_shape <-

