library(tidyverse)

# read in paul comps
paul_ac <- vroom::vroom(here::here('dev', 'ai_subreg_comp', 'ai_nr_re.csv')) %>% 
  tidytable::pivot_longer(cols = -year, names_to = 'age', values_to = 'p_comp')

# read in surveyISS comps
srvISS_ac <- vroom::vroom(here::here('output', 'ai', 'base_age_ai_subreg.csv'))

srvISS_ac %>% 
  tidytable::filter(region == 'ai' & sex != 0) %>% 
  tidytable::summarise(agepop = sum(agepop), .by = c(year, age)) %>% 
  tidytable::mutate(s_comp = agepop / sum(agepop), .by  = c(year))

vroom::vroom(here::here('dev', 'ai_subreg_comp', 'ai_nr_re.csv')) %>% 
  tidytable::pivot_longer(cols = -year, names_to = 'age', values_to = 'p_comp') %>% 
  tidytable::mutate(age = as.numeric(age)) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', 'ai', 'base_age_ai_subreg.csv')) %>% 
                         tidytable::filter(region == 'ai' & sex != 0) %>% 
                         tidytable::summarise(agepop = sum(agepop), .by = c(year, age)) %>% 
                         tidytable::mutate(s_comp = agepop / sum(agepop), .by  = c(year)) %>% 
                         tidytable::select(-agepop))
