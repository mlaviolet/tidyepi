# make table of source data for 2000 standard population
library(here)
library(dplyr)
library(stringr)

step1 <- 
  locate_areas("R:/OCPH/EPI/BHSDM/Group/Michael Laviolette/Stat tools/p251130.pdf",
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

