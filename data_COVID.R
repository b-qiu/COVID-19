library("pacman")
p_load("tidyverse", "rvest", "qdapRegex", 
       "RCurl", "magrittr", "googlesheets4")

options(gargle_oauth_cache = ".secrets")

gs4_auth(email = XXXX,
  cache = ".secrets")

jh <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  select(-`Province/State`, -`Long`, -`Lat`) %>%
  filter(`Country/Region` == "Australia") %>%
  gather(dates, cases, -`Country/Region`) %>%
  group_by(dates) %>%
  summarize(cases = sum(cases)) %>%
  mutate(dates = as.Date(dates, "%m/%d/%y")) %>%
  arrange(dates) %>%
  mutate(cases = c(0,diff(cases)))

ow <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/ecdc/new_cases.csv") %>%
  select(date, Australia) %>%
  mutate(date = date - 1) %>%
  rename(dates = date, cases = Australia)

wo <- read_html("https://www.worldometers.info/coronavirus/country/australia/") %>%
  html_node("body") %>%
  xml_find_all("//script[contains(@type, 'text/javascript')]") %>%
  html_text() %>%
  extract2(11) %>%
  rm_between("[", "]", extract = T)

xlab <- wo[[1]][1] %>%
  str_split(",") %>%
  unlist(use.names = F) %>%
  str_sub(2, 7) %>%
  as.Date("%b %d")

ylab <- wo[[1]][2] %>%
  str_split(",") %>%
  unlist(use.names = F) %>%
  str_replace("null", "0") %>%
  as.numeric()

wo <- data.frame(dates = xlab, cases = ylab)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 
# jh_gs_id <- gs4_get(jh_gs) %>% pluck("spreadsheet_id")
# ow_gs_id <- gs4_get(ow_gs) %>% pluck("spreadsheet_id")

write_sheet(jh, ss = "1OfdJUC-1nB8cqQ0HsrCpNFC8yPDjDPUUkQFxZA6EdYY", sheet = "jh")
write_sheet(ow, ss = "1OfdJUC-1nB8cqQ0HsrCpNFC8yPDjDPUUkQFxZA6EdYY", sheet = "ow")
write_sheet(wo, ss = "1OfdJUC-1nB8cqQ0HsrCpNFC8yPDjDPUUkQFxZA6EdYY", sheet = "wo")

# saveRDS(wo, file = "worldometer_data.RDS")
# saveRDS(ow, file = "our_world_data.RDS")
# saveRDS(jh, file = "john_hopkins_data.RDS")
