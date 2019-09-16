library(dplyr)
library(purrr)

std_pop <- readxl::read_excel("data-raw/standard-population.xlsx",
                              na = "0")

master_pop <- pull(std_pop[, 2]) %>% setNames(pull(std_pop[, 1]))
master_tbl <- std_pop %>% 
  select(age_group, pop = master)
  
names_lst <- 
  list(names_01 = c("00", "01-04", "05-14","15-24", "25-34", "35-44", "45-54", 
                    "55-64", "65-74", "75-84", "85+"),
       names_02 = c("00-11","12-19", "20-29", "30-39", "40-49", "50-59", 
                    "60-69", "70-79", "80+"),
       names_03 = c("00-17", "18-44", "45-54", "55-64", "65-74", "75+"),
       names_04 = c("00-17", "18-44", "45-64", "65-74", "75+"),
       names_05 = c("02-05", "06-11", "12-19", "20-29", "30-39", "40-49", 
              "50-59", "60-69", "70-79", "80+"), 
       names_06 = c("02-17", "18-44", "45-54", "55-64", "65-74", "75+"), 
       names_07 = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69",
              "70-79", "80+"), 
       names_08 = c("18-24", "25-44", "45-64", "65+"),
       names_09 = c("18-24", "25-34", "35-44", "45-64", "65+"),
       names_10 = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", 
                    "80+"), 
       names_11 = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", 
                    "80+"), 
       names_12 = c("20-39", "40-59", "60+"),
       names_13 = c("20-44", "45-64", "65+"),
       names_14 = c("25-34", "35-44", "45-64", "65+"),
       names_15 = c("40-49", "50-64", "65+"),
       names_16 = c("45-49", "50-64", "65+"),
       names_17 = c("50-64", "65+"),
       names_18 = c("65-74", "75+"),
       names_19 = c("00-04", "05-11", "12-17"),
       names_20 = c("00-17", "18-44", "45-64"),
       names_21 = c("05-17", "18-44", "45-64"),
       names_22 = c("18-24", "25-34", "35-44", "45-64"))  

# create names vector of populations of modified age groupings
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

# SEER 19 age groups
seer_pop <- std_pop %>% 
  group_by(seer) %>% 
  summarize(pop = sum(master)) %>% 
  pull(pop) %>% 
  setNames(c("00", "01-04", "05-09", "10-14", "15-19", 
             "20-24", "25-29", "30-34", "35-39", "40-44", 
             "45-49", "50-54", "55-59", "60-64", "65-69", 
             "70-74", "75-79", "80-84", "85+"))

# five-year, 18 age groups
five_year_pop <- std_pop %>% 
  group_by(five_year) %>% 
  summarize(pop = sum(master)) %>% 
  pull(pop) %>% 
  setNames(c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
             "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
             "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
  
std_pop_list <- map2(sprintf("%02d", 1:22), names_lst,  makeVector)
std_pop_list <- c(list(master_pop, seer_pop, five_year_pop), std_pop_list)
names(std_pop_list) <- c("master_pop", "seer_pop", "five_year_pop",
                     paste0("dist_", sprintf("%02d", 1:22)))
lapply(std_pop_list, sum)

# usethis::use_data(std_pop_list, overwrite = TRUE)





