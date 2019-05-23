# assemble data frame of cancer incidence for package data

library(tidyverse)
library(here)

cancer <- read_tsv(here("data-raw",
                        "US cancer incidence, all sites.txt"),
                   n_max = 646) %>%
  select(Year, agegroup = `Age Groups Code`, Sex, n = Count,
         pop = Population) %>%
  mutate(Sex = factor(Sex, levels = c("Male", "Female")),
         agegroup = sub("^1$", "0", agegroup),
         agegroup = fct_inorder(agegroup)) %>%
  arrange(Year, Sex, agegroup)
levels(cancer$agegroup)[1:3] <- c("00", "01-04", "05-09")






