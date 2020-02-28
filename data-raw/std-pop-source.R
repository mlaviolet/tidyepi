# make table of source data for 2000 standard population
library(here)
library(dplyr)
library(stringr)

# https://www.census.gov/prod/1/pop/p25-1130/p251130.pdf

step1 <- 
  tabulizer::locate_areas("R:/OCPH/EPI/BHSDM/Group/Michael Laviolette/Stat tools/p251130.pdf",
                 pages = 60)
step2 <- data.frame(step1[[1]])
step3 <- setNames(step2, c("agegroup", "pop"))
step3[, 1] <- str_trim(str_remove_all(step3[, 1], "-"), "both")
step3[, 2] <- as.numeric(str_remove(as.character(step3$pop), " "))

step4 <- step3 %>% 
  slice(-c(1,2,8,14,20,26,32,38,44,50,56,62,68,74))
  
  filter(str_detect(agegroup, "to", negate = TRUE)) %>% 
  filter(str_detect(agegroup, "All", negate = TRUE)) 
  
step5 <- edit(step4)
  
step6 <- step5 %>% 
  group_by(master) %>% 
  summarize(pop = sum(pop))


save(step5, step6, file = here("data-raw", "std_pop_matrix.Rdata"))

library(rvest)
url <- "https://seer.cancer.gov/stdpopulations/stdpop.singleages.html"
population <- url %>% 
  html_nodes(xpath = '/html/body/div[5]/div[2]') %>% 
  html_table()

url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
population <- url %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[1]")
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% 
  html_table()

url <- paste0("https://web.archive.org/web/20190202054736/",
              "https://www.boxofficemojo.com/movies/?id=ateam.htm")
ateam <- read_html(url)
html_nodes(ateam, "center")

/html/body/div[3]/div[3]/div[4]/div/table[1]

