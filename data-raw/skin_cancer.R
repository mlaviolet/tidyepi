library(tidyverse)
library(tidyepi)
library(haven)
library(here)

us_c43 <- read_sas(here("data-raw", "us_c43.sas7bdat"))
florida_c43 <- read_sas(here("data-raw", "florida_c43.sas7bdat"))

skin_cancer <- inner_join(florida_c43, us_c43, by = "Age",
                          suffix = c("_FL", "_US"))
save(skin_cancer, file = here("data-raw", "skin.Rdata"))
xlsx::write.xlsx(skin_cancer, here("data-raw", "skin.xlsx"))
sir <- indirect_adjust(skin_cancer, Event_FL, PYear_FL, Event_US, PYear_US)
