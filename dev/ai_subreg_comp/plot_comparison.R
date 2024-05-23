# load surveyISS library ----
devtools::install_github("BenWilliams-NOAA/surveyISS", force = TRUE)
library(surveyISS)

# run surveyISS to get comps ----

## get data (note, need to be connected to network) ----
region = 'ai'
yrs = 1991
species = c(30420)
survey = 52

data_ai <- surveyISS::query_data(survey = survey,
                                 region = region,
                                 species = species,
                                 yrs = yrs)

## run for ai northern rockfish (subregion expansion) ----
surveyISS::srvy_iss_ai_subreg(iters = 2,
                              lfreq_data = data_ai$lfreq, 
                              specimen_data = data_ai$specimen, 
                              cpue_data = data_ai$cpue, 
                              strata_data = data_ai$strata, 
                              yrs = yrs,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              region = 'ai',   
                              save_stats = TRUE, 
                              save = 'ai_nr_comp')







# read in comps
comps <- vroom::vroom(here::here('dev', 'ai_subreg_comp', 'ai_nr_re.csv')) %>% 
  tidytable::pivot_longer(cols = -year, names_to = 'age', values_to = 'p_comp') %>% 
  tidytable::mutate(age = as.numeric(age)) %>% 
  spec()
  tidytable::full_join(vroom::vroom(here::here('output', 'ai', 'base_age_ai_subreg.csv')) %>% 
                         tidytable::filter(region == 'ai' & sex != 0) %>% 
                         tidytable::summarise(agepop = sum(agepop), .by = c(year, age)) %>% 
                         tidytable::filter(age < 40) %>% 
                         tidytable::bind_rows(vroom::vroom(here::here('output', 'ai', 'base_age_ai_subreg.csv')) %>% 
                                                tidytable::filter(region == 'ai' & sex != 0) %>% 
                                                tidytable::summarise(agepop = sum(agepop), .by = c(year, age)) %>% 
                                                tidytable::filter(age >= 40) %>% 
                                                tidytable::summarise(agepop = sum(agepop), .by = c(year)) %>% 
                                                tidytable::mutate(age = 40)) %>% 
                         tidytable::mutate(s_comp = agepop / sum(agepop), .by  = c(year)) %>% 
                         tidytable::select(-agepop)) %>% 
  tidytable::replace_na(list(s_comp = 0)) %>% 
  tidytable::replace_na(list(p_comp = 0))


comps %>% 
  tidytable::pivot_longer(cols = c('p_comp', 's_comp'), names_to = 'groups') %>% 
  tidytable::mutate(Year = factor(year)) -> .comps

ggplot2::theme_set(afscassess::theme_report())


comp <- function(data, variable, fleet) {
  var <- rlang::enquo(variable)
  
  data %>%
    dplyr::mutate(level = factor(.data[[var]]),
                  Year = factor(Year)) %>%
    dplyr::filter(groups == "p_comp") %>%
    ggplot2::ggplot(ggplot2::aes(.data[[var]], value)) +
    ggplot2::geom_col(ggplot2::aes(fill = level), width = 1, color = 1) +
    ggplot2::facet_wrap(~Year, strip.position="right",
                        dir = "v",
                        ncol = 1) +
    ggplot2::geom_point(data = dplyr::filter(data, groups == "s_comp"),
                        ggplot2::aes(.data[[var]], value, group = 1)) +
    ggplot2::theme(panel.spacing.y = grid::unit(0, "mm")) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::xlab(Hmisc::capitalize(variable)) +
    ggplot2::ylab(paste0(Hmisc::capitalize(fleet)," ", variable,  " composition")) +
    afscassess::scale_x_tickr(data=data, var=.data[[var]], start = 0, min = 5) +
    ggplot2::theme(legend.position = "none")
}

comp(data = .comps, variable = 'age', fleet = 'survey')

ggplot2::ggsave(here::here('dev', 'ai_subreg_comp', "age_comp.png"),
                width = 6.5, height = 6.5, units = "in", dpi = 200)
