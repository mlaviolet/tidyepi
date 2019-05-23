library(dplyr)
library(purrr)

std_pop <- readxl::read_excel("data-raw/standard-population.xlsx",
                              na = "0")

master_names <- pull(std_pop[, 1])
  
names_lst <- 
  list(names_01 = c("00", "01-04", "05-14","15-24", "25-34", "35-44", "45-54", 
                    "55-64", "65-74", "75-84", "85+"),
       names_02 = c("00-11","12-19", "20-29", "30-39", "40-49", "50-59", 
                    "60-69", "70-79", "80+"),
       names_03 = c("00-17", "18-44", "45-54", "55-64", "65-74", "75+"),
       names_04 = c("00-18", "18-44", "45-64", "65-74", "75+"),
       names_05 = c("02-05", "06-11", "12-19", "20-29", "30-39", "40-49", 
              "50-59", "60-69", "70-79", "80+"), 
       names_06 = c("02-17", "18-44", "45-54", "55-64", "65-74", "75+"), 
       names_07 = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69",
              "70-79", "80+"), 
       names_08 = c("18-24", "25-44", "45-64", "65+")
       )  




names_22 <- c("18-24", "25-34", "35-44", "45-64")


makeVector <- function(x, y) {
  var_enq <- sym(paste0("dist_", x))
  names_enq <- sym(paste0("names_", x))
  std_pop %>% 
    group_by(!!var_enq) %>% 
    summarize(pop = sum(master)) %>% 
    filter(!is.na(!!var_enq)) %>% 
    pull(pop) %>% 
    setNames(y)
  }

map2(sprintf("%02d", 1:8), names_lst,  makeVector)
makeVector("01", names_lst[[as.numeric("01")]])
# LOOKS GOOD

std_pop %>% 
  group_by(dist_01) %>% 
  summarize(pop = sum(master)) %>% 
  filter(!is.na(dist_01)) %>% 
  pull(pop) %>% 
  setNames(names_01)



makeVector("02")



