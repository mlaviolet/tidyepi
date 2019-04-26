# test rehapeForSIR, directAdjust, and indirectAdjust functions
library(here)
library(dplyr)
library(tidyepi)

# example for indirect adjustment
# create SAS data set from data given in example
# http://documentation.sas.com/?docsetId=statug&docsetTarget=statug_stdrate_examples04.htm&docsetVersion=15.1&locale=en
# http://documentation.sas.com/api/docsets/statug/15.1/content/statug_code_sratex4.htm?locale=en
# cs <- haven::read_sas(here("data-raw", "us_cs.sas7bdat")) %>%
#   mutate(region = "US") %>%
#   bind_rows(haven::read_sas(here("data-raw", "florida_cs.sas7bdat")) %>%
#               mutate(region = "Florida")) %>%
#   select(region, everything())

load(here("data-raw", "sas-examples.rda"))

cs %>%
  group_by(Cause) %>%
  do(reshape_for_SIR(., Age, region, "US", Event, PYear)) %>%
  do(indirect_adjust(., study_count, study_pop, ref_count, ref_pop))

data(cancer)
cancer %>%
  group_by(Year) %>%
  do(reshape_for_SIR(., agegroup, Sex, "Female", n, pop)) %>%
  do(indirect_adjust(., study_count, study_pop, ref_count, ref_pop))

# example for direct adjustment
# http://documentation.sas.com/?docsetId=statug&docsetTarget=statug_stdrate_examples01.htm&docsetVersion=15.1&locale=en
# http://documentation.sas.com/api/docsets/statug/15.1/content/statug_code_sratex1.htm?locale=en
# two_states <- haven::read_sas(here("data-raw", "twostates.sas7bdat"))
# std_pop <- haven::read_sas(here("data-raw", "us.sas7bdat"))
# # standardizing by both sex and age
# # add weights of standard variables
# two_states <- std_pop %>%
#   mutate(Pwt = PYear/sum(PYear)) %>%
#   select(-PYear) %>%
#   inner_join(two_states, ., by = c("Sex", "Age"))

# two_states %>%
#   group_by(State) %>%
#   do(direct_adjust(., Death, PYear, Pwt, base = 1000))

# save(cs, std_pop, two_states, file = here("data-raw", "sas-examples.rda"))




