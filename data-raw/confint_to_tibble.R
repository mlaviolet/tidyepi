# https://stackoverflow.com/questions/29614849/dplyrmutate-to-add-multiple-values
# latest solution by RyanFrost

df <- data.frame(id = c("A", "B", "C"),  x = c(0, 10, 10), 
                 n = c(100, 400, 400))
# GROUPING BY RECORD ID WOULD OCCUR OUTSIDE WORKING FUNCTION
# nest() and unnest() would probably not be needed
# key is using tidy() to put test results in data frame
df %>%
  group_by(id, x, n) %>%
  nest() %>%
  mutate(ptest = map(data, ~ tidy(poisson.exact(x, n)))) %>%
  unnest(ptest) %>% 
  select(x, n, estimate, conf.low, conf.high)
