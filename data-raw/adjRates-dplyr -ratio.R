# compute age-adjusted rates by county (subregion)
#   and rate ratios with CI's of county to state (parent region) using methods 
# Tiwari RC et al., Stat Methods Med Res 15, 547-569, 2006 (adjusted rates)
# Tiwari RC et al., J. Data Science 8, 471-482, 2010 (rate ratios and CI's)
# Swift MB, Commun Stat-Theor M, 38, 748-759, 2009 (crude or age-specific
#   rates using approximate mid-P method for CI's)
# Using mortality rates for coronary heart disease, 2013
#   M. Laviolette, 2015-04-01

# library(RODBC)
# ptm <- proc.time()
library(dplyr)
library(tidyr)

# function to compute age-adjusted rates for each region, ratio of region
#   rate to state rate, confidence interval on rate ratio, and to flag
#   each region's rate as significantly higher than, significantly lower than, 
#   or similar to the state rate
adjRatio <- 
  function(count.x, count.c, pop.x, pop.c, wt, base = 100000, level = 95) {
    # count.x = count of events within region 
    # count.c = count of events outside region
    # pop.x = population (or person-years) within region 
    # pop.c = population or person-years outside region
    # wt = weights of standard population, as proportions summing to 1
    # base = population base, defaults to 100,000
    # level = confidence level as percent, defaults to 95
    z     <- qnorm((100 + level) / 200) # normal percentile for CI's 
    k     <- (z^2 + 2) / 12  # adjustment term for mid-P intervals
    alpha <- 1 - level / 100  
    J     <- 1 / length(wt)  # adjustment term to prevent zero counts
    data.frame(count.x, count.c, pop.x, pop.c, wt) %>%
      # compute adjusted rates for regions and state
      summarize(events = sum(count.x),
                person.yrs = sum(pop.x),
                adj.rate = sum(wt*count.x / pop.x),
                rx = sum(wt * (count.x + J) / pop.x),
                rxc = sum(wt * (count.c + J) / pop.c),
                var.x = sum(wt^2 * count.x / pop.x^2),
                var.j = sum(wt^2 * (count.x + J) / pop.x^2),
                var.c = sum(wt^2 * (count.c + J) / pop.c^2),
                pxc = sum(pop.c) / sum(pop.x + pop.c),
                r.state = sum(wt * (count.x + count.c) / (pop.x + pop.c))) %>%
      # compute confint for adjusted rates  
      mutate(adj.lci = qgamma(alpha / 2, 
                              shape = adj.rate^2 / var.x, 
                              scale = var.x / adj.rate),
             adj.uci = qgamma(1 - alpha/2, 
                              shape = (rx^2) / var.j,
                              scale = var.j / rx),
             # ratio of region rate to state rate with confint
             ratio = adj.rate / r.state,
             moe = z * rx * rxc * 
               sqrt(pxc * (var.x / rx^2 + var.c / rxc^2)) / r.state^2,
             ratio.lci = max(0, ratio - moe), 
             ratio.uci = ratio + moe,
             # significance flag 
             sig = ifelse(ratio.lci > 1, "Higher",
                          ifelse(ratio.uci < 1, "Lower", "Similar")),
             # crude rate with confint
             crude.rate = events / person.yrs, 
             crude.lci = (sqrt(events + k) - z / 2)^2 / person.yrs,
             crude.uci = (sqrt(events + k) + z / 2)^2 / person.yrs) %>%
      # subset to variables for reporting   	   
      select(events, person.yrs, starts_with("crude"), starts_with("adj"), 
             starts_with("ratio"), sig) %>% 
      mutate_at(vars(starts_with("ratio")), funs(round(., 2))) %>% 
      mutate_at(vars(starts_with("adj"), starts_with("crude")), funs(base * .))
    }

county.lbl <- c("Belknap", "Carroll", "Cheshire", "Coos", "Grafton",
                "Hillsborough", "Merrimack", "Rockingham", "Strafford",
                "Sullivan")

# make data frame of age groups and standard weights
yr5.lbl <- c("0 To 4", "05 To 9", "10 To 14", "15 To 19", "20 To 24", 
             "25 To 29", "30 To 34", "35 To 39", "40 To 44", "45 To 49", 
             "50 To 54", "55 To 59", "60 To 64", "65 To 69", "70 To 74", 
             "75 To 79", "80 To 84", "85 Plus")
yr5.wt <- c(18987, 19920, 20057, 19820, 18257, 17722, 19511, 22180, 22479,
            19806, 17224, 13307, 10654,  9410,  8726,  7415,  4900,  4259)
weight <- data.frame(AGEGROUP = yr5.lbl, wt = yr5.wt / sum(yr5.wt))


# coronary heart disease, 2013
events <- dbGetQuery(edw, 
  "select ST_FILE_NBR,
   trunc(months_between(DECD_DTH_DT, DECD_BRTH_DT) / 12) as AGE,
     substr(DECD_RES_GEO_CD, 3, 2) as REGION 
   from BVRODS.BVR_DEATH_RESTRICTED_VW 
   where DECD_DTH_YEAR = '2013' and DECD_RES_GEO_CD like '28%'
     and substr(CERTFR_UNLY_CAUSE_DTH_CD, 1, 3) in 
        ('I20', 'I21')")
# 				   
# get population as total by county and age group
popn <- dbGetQuery(edw, "select COUNTY_CDE as REGION, 
                         FIVE_YEAR_AGE_GROUP_TXT as AGEGROUP,
                         sum(POPULATION_CNT) as POP
                       from WRQPRD.POPULATION_CENSUS_TBL
                       where CALENDAR_YR = '2013' 
                       group by COUNTY_CDE, FIVE_YEAR_AGE_GROUP_TXT
                       order by COUNTY_CDE, FIVE_YEAR_AGE_GROUP_TXT")

# convert counties and age groups to factors in population data
popn <- popn %>%
  mutate(REGION = factor(REGION, labels = county.lbl),
         AGEGROUP = factor(AGEGROUP))
# check
popn %>%
  group_by(REGION) %>%
  summarize(sum(POP))

# group ages and convert counties to factors in event data
events <- events %>%
  mutate(REGION = factor(REGION, labels = county.lbl),
         AGEGROUP = cut(AGE, c(seq(0, 85, 5), Inf), 
                        right = FALSE, labels = yr5.lbl))
# clean up
# rm(yr5.lbl, county.lbl); gc()

# get event count for each region-age group combination, imputing zero 
#   for missing combinations
# for each age group, split event counts into total within and outside county
# aggregate by age group to get state totals, then merge with county data 
#   and take difference to get event counts outside county

counts <- left_join(events %>%
                       # aggregate counts by region and age group
                       group_by(REGION, AGEGROUP) %>%
                       summarize(count.x = n()) %>%
                       # impute zero for missing combinations
                       spread(REGION, count.x, fill = 0, drop = FALSE) %>%
                       gather(REGION, count.x, -AGEGROUP),
                     events %>%
                       # aggregate counts by age group only
                       group_by(AGEGROUP) %>%
                       summarize(N = n()), 
                     by = "AGEGROUP") %>%
  # event counts outside each region
  mutate(N = ifelse(is.na(N), 0, N),
         count.c = N - count.x) %>%
  select(REGION, AGEGROUP, count.x, count.c) %>%
  arrange(REGION, AGEGROUP)

# for each age group, split population into total within and outside county
# this segment merges county data with state data and creates variable of 
#   difference between county total and state total
person.yr <- inner_join((popn %>% 
                           # aggregate population by region and age group
                           group_by(REGION, AGEGROUP) %>% 
                           summarize(pop.x = sum(POP))), 
                        (popn %>% 
                           # aggregate population by age group only 
                           group_by(AGEGROUP) %>% 
                           summarize(POP = sum(POP))), 
                        by = "AGEGROUP") %>%
  # population outside each region
  mutate(pop.c = POP - pop.x) %>%
  arrange(REGION, AGEGROUP) 

# each region now has all data needed to compute rates and ratios
# merge event and population data, then merge with weights
region.rates <- inner_join(counts, person.yr, by = c("REGION", "AGEGROUP")) %>%
  inner_join(weight, by = "AGEGROUP") %>%
   # compute rates and ratios by region 
  group_by(REGION) %>%
  do(with(., adjRatio(count.x, count.c, pop.x, pop.c, wt))) %>%
  select(REGION, events, person.yrs, crude.rate, crude.lci, crude.uci, 
         adj.rate, adj.lci, adj.uci, ratio, ratio.lci, ratio.uci, sig)

base  <- 100000
level  <- 95
alpha <- 1 - level / 100
z     <- qnorm((100 + level) / 200) # normal percentile for CI's 
k     <- (z^2 + 2)/12 

# get state rate and add to table of regional rates
# rate and CI computed similarly to regions as above
all.rates <- counts %>% 
  group_by(AGEGROUP) %>%
  summarize(N = sum(count.x)) %>%
  inner_join(popn %>% 
               group_by(AGEGROUP) %>%
               summarize(pop = sum(POP)), by = "AGEGROUP") %>%
  inner_join(weight, by = "AGEGROUP") %>%
  summarize(events = sum(N),
            person.yrs = sum(pop),
            adj.rate = sum(wt * N/pop),
            var = sum(wt^2 * N / pop^2),
            rate.j = sum(wt*(N + 1 / length(wt)) / pop), 
            var.j = sum(wt^2 * (N + 1 / length(wt)) / pop^2)) %>%
  mutate(adj.lci = qgamma(alpha/2, 
                          shape = adj.rate^2 / var, 
                          scale = var / adj.rate),
         adj.uci = qgamma(1 - alpha / 2, 
                          shape = (rate.j^2) / var.j,
                          scale = var.j / rate.j)) %>%
  mutate(adj.rate = round(base * adj.rate, 1),
         adj.lci = round(base * adj.lci, 1),
         adj.uci = round(base * adj.uci, 1),
         crude.rate = round(base * events / person.yrs, 1),
         crude.lci = round(base * (sqrt(events + k) - z / 2)^2 / person.yrs, 1),
         crude.uci = round(base * (sqrt(events + k) + z / 2)^2 / person.yrs, 1),
         REGION = "New Hampshire") %>%
  select(REGION, events, person.yrs, crude.rate, crude.lci, crude.uci, 
         adj.rate, adj.lci, adj.uci) %>%
  bind_rows(region.rates, .)

# wrap state rates into function
# adjRate <- function(counts, popn, weight, base = 100000, level = 95, places = 1){
#   z          <- qnorm((100 + level)/200)
#   k          <- (z^2 + 2)/12
#   J          <- length(weights)
#   alpha      <- 1 - level/100
#   final <- inner_join()
#   
#   events     <- sum(num)
#   pop        <- sum(den)
#   crude.rate <- events/pop
#   crude.lci  <- (sqrt(events + k) - z/2)^2/pop
#   crude.uci  <- (sqrt(events + k) + z/2)^2/pop
#   as.data.frame()
# }

### END
