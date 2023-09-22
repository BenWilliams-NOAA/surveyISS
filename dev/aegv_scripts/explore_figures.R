
# load ----
library(tidyverse)
library(tidytable)
library(vroom)
library(here)
library(purrr)
library(rsample)
library(data.table)
library(scico)
library(extrafont)
library(cowplot)
library(egg)
library(ggplot2)
library(gg.layers)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# remotes::install_github("rpkgs/gg.layers")
# extrafont::font_import()
loadfonts(device="win")   

# if you get a lot of messages like 'C:\Windows\Fonts\ALGER.TTF : No FontName. Skipping.'
# then load this package and then run font_import
# remotes::install_version("Rttf2pt1", version = "1.3.8")

# add fonts to all text (last line)
ggplot2::theme_set(
  ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # axis.ticks.length = grid::unit(base_ / 2.2, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      legend.background = ggplot2::element_rect(colour = NA, fill = NA),
      text = element_text(family = "Times New Roman")
    )
)

# pull in output ----

if(!file.exists(here::here('output', 'afsc_iss_err_full.csv'))){
  
  spec <- vroom::vroom(here::here('data', 'species_code_name.csv')) # species_code and common names
  
  r_t <- vroom::vroom(here::here('data', 'reader_tester.csv'))
  
  specimen <- vroom::vroom(here::here('data', 'specimen_ai.csv')) %>% 
    tidytable::mutate(region = 'ai') %>% 
    tidytable::bind_rows(vroom::vroom(here::here('data', 'specimen_bs.csv')) %>% 
                           tidytable::mutate(region = 'bs')) %>% 
    tidytable::bind_rows(vroom::vroom(here::here('data', 'specimen_goa.csv')) %>% 
                           tidytable::mutate(region = 'goa'))
  
  err_iss <- vroom::vroom(here::here('output', 'afsc_iss_err.csv'))
  
  surv_labs <- c("Aleutian Islands", "Eastern Bering Sea Shelf", "Gulf of Alaska")
  names(surv_labs) <- c("ai", "bs", "goa")
  
  # add specimen info to iss results
  
  specimen %>% 
    tidytable::drop_na() %>% 
    tidytable::summarise(nhls = length(unique(hauljoin)),
                         nss = length(age), .by = c(year, species_code, region, sex)) %>% 
    tidytable::filter(sex != 3) %>% 
    tidytable::mutate(comp_type = case_when (sex == 1 ~ 'male',
                                             sex == 2 ~ 'female')) %>% 
    tidytable::select(-sex) %>% 
    tidytable::bind_rows(specimen %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(nhls = length(unique(hauljoin)),
                                                nss = length(age), .by = c(year, species_code, region, sex)) %>% 
                           tidytable::summarise(nhls = sum(nhls),
                                                nss = sum(nss), .by = c(year, species_code, region)) %>% 
                           tidytable::mutate(comp_type = 'total')) -> samp_dat
  
  species = unique(err_iss$species_code)
  
  r_t %>% 
    tidytable::filter(species_code %in% species,
                      Region %in% c('AI', 'BS', 'BS/AI', 'BS_GOA', 'BSAI', 'BSGOA', 'GOA')) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::select(species_code, read_age, test_age) %>% 
    tidytable::drop_na() %>% 
    tidytable::filter(read_age > 0) %>% 
    tidytable::mutate(diff = abs(read_age - test_age) / read_age) %>% 
    tidytable::summarise(ape1 = sum(diff),
                         R = length(diff),
                         cv_a = sd(test_age) / read_age, .by = c(species_code, read_age)) %>% 
    tidytable::summarise(ape2 = sum(ape1 / R),
                         N = sum(R),
                         cv_a = mean(cv_a, na.rm = TRUE), .by = c(species_code)) %>% 
    tidytable::mutate(ape = 100 * ape2 / N) %>% 
    tidytable::select(species_code, ape, cv_a) -> ape
  
  specimen %>% 
    tidytable::drop_na() %>% 
    tidytable::summarise(sd_l1 = sd(length),
                         cv_l = sd(length) / mean(length), .by = c(species_code, sex, region, age)) %>%
    tidytable::drop_na() %>% 
    tidytable::summarise(sd_l = mean(sd_l1),
                         cv_l = mean(cv_l), .by = c(species_code, sex, region)) %>% 
    tidytable::filter(sex != 3) %>% 
    tidytable::mutate(comp_type = case_when (sex == 1 ~ 'male',
                                             sex == 2 ~ 'female')) %>% 
    tidytable::select(-sex) %>% 
    tidytable::bind_rows(specimen %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(sd_l1 = sd(length),
                                                cv_l = sd(length) / mean(length), .by = c(species_code, region, age)) %>%
                           tidytable::drop_na() %>% 
                           tidytable::summarise(sd_l = mean(sd_l1),
                                                cv_l = mean(cv_l), .by = c(species_code, region)) %>% 
                           tidytable::mutate(comp_type = 'total')) -> sd_l
  
  err_iss %>% 
    tidytable::left_join(samp_dat) %>% 
    tidytable::left_join(ape) %>% 
    tidytable::left_join(sd_l) %>% 
    vroom::vroom_write(.,
                       here::here('output', 'afsc_iss_err_full.csv'),
                       delim = ',') -> plot_dat
} else{
  plot_dat <- vroom::vroom(here::here('output', 'afsc_iss_err_full.csv'))
}

surv_labs <- c("Aleutian Isalnds", "Eastern Bering Sea Shelf", "Gulf of Alaska")
names(surv_labs) <- c("ai", "bs", "goa")
spec <- vroom::vroom(here::here('data', 'species_code_name.csv'))







# compare pre/post aggregation (supp mat) ----

vroom::vroom(here::here('output', 'goa', 'add_err', 'popollatf_ann_iss_ag.csv')) %>% 
  tidytable::select(year, species_code, comp_type, ae, ae_al, al, base) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', 'goa', 'add_err', 'popoll_prepost_iss_ag.csv')) %>% 
                         tidytable::rename(ae_pre = 'ae',
                                           ae_al_pre = 'ae_al',
                                           al_pre = 'al',
                                           base_pre = 'base')) %>% 
  tidytable::filter(comp_type == 'total', species_code != 10110) %>% 
  tidytable::mutate(ae_rel = ae / base,
                    ae_al_rel = ae_al / base,
                    al_rel = al / base,
                    base_rel = 1,
                    ae_pre_rel = ae_pre / base_pre,
                    ae_al_pre_rel = ae_al_pre / base_pre,
                    al_pre_rel = al_pre / base_pre,
                    base_pre_rel = 1) %>% 
  tidytable::pivot_longer(cols = c(ae, ae_al, al, base, 
                                   ae_pre, ae_al_pre, al_pre, base_pre,
                                   ae_rel, ae_al_rel, al_rel, base_rel, 
                                   ae_pre_rel, ae_al_pre_rel, al_pre_rel, base_pre_rel)) %>%
  tidytable::mutate(pool_type = case_when(name %in% c('ae', 'ae_al', 'al', 'base',
                                                      'ae_rel', 'ae_al_rel', 'al_rel', 'base_rel') ~ 'post',
                                          name %in% c('ae_pre', 'ae_al_pre', 'al_pre', 'base_pre',
                                                      'ae_pre_rel', 'ae_al_pre_rel', 'al_pre_rel', 'base_pre_rel') ~ 'pre'),
                    err_src = case_when(name %in% c('ae', 'ae_pre', 'ae_rel', 'ae_pre_rel') ~ 'AE',
                                        name %in% c('al', 'al_pre', 'al_rel', 'al_pre_rel') ~ 'GV',
                                        name %in% c('ae_al', 'ae_al_pre', 'ae_al_rel', 'ae_al_pre_rel') ~ 'AE & GV',
                                        name %in% c('base', 'base_pre', 'base_rel', 'base_pre_rel') ~ 'Base'),
                    iss_type = case_when(name %in% c('ae', 'ae_al', 'al', 'base',
                                                     'ae_pre', 'ae_al_pre', 'al_pre', 'base_pre') ~ 'Age composition ISS',
                                        name %in% c('ae_rel', 'ae_al_rel', 'al_rel', 'base_rel',
                                                    'ae_pre_rel', 'ae_al_pre_rel', 'al_pre_rel', 'base_pre_rel') ~ 'Relative age composition ISS'),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV'))) %>%
  tidytable::left_join(spec) %>% 
  ggplot(aes(err_src, value, fill = pool_type)) +
  geom_boxplot2(width.errorbar = 0, alpha= 0.5) +
  facet_grid(iss_type ~ species_name, 
             scales = "free_y",
             labeller = label_wrap_gen(40)) +
  theme(legend.position = "bottom",
        strip.text.y.right = element_text(angle = 270),
        strip.text = element_text(size = 12)) +
  xlab("\nUncertainty scenario") +
  ylab("") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type")












# plot age iss with added error for all species-regions (figure 1) ----

png(filename=here::here("figs", "alt_iss.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

plot_dat %>% 
  select(year, species_code, comp_type, ae, ae_al, al, base, region) %>% 
  tidytable::left_join(spec) %>%
  tidytable::pivot_longer(cols = c(ae, ae_al, al, base)) %>% 
  tidytable::mutate(err_src = case_when(name == 'ae' ~ 'AE',
                                        name == 'al' ~ 'GV',
                                        name == 'ae_al' ~ 'AE & GV',
                                        name == 'base' ~ 'Base'),
                    surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "Eastern Bering Sea Shelf"),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV'))) %>% 
  # filter(species_name %in% c("Arrowtooth flounder", "Pacific ocean perch", "Pacific cod", "Walleye pollock")) %>% 
  ggplot(aes(comp_type, value, fill = err_src)) +
  geom_boxplot2(width.errorbar = 0, alpha= 0.5) +
  facet_grid(species_name ~ surv_labs, 
             scales = "free_y",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "bottom",
        strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge composition sex category") +
  ylab("Age composition ISS\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Uncertainty scenario")  +
  scale_y_continuous(breaks = c(0, 250, 500)) +
  expand_limits(y=c(0, 250))
# scale_y_continuous(breaks  = c(0, 200, 00))

dev.off() 




# plot relative iss with added error by species type (figure 2) ----

png(filename=here::here("figs", "rel_iss.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)


plot_dat %>% 
  select(year, species_code, comp_type, ae, ae_al, al, base) %>%
  tidytable::mutate(ae_rel = ae / base,
                    ae_al_rel = ae_al / base,
                    al_rel = al / base,
                    base_rel = 1) %>% 
  tidytable::pivot_longer(cols = c(ae, ae_al, al, base,
                                   ae_rel, ae_al_rel, al_rel, base_rel)) %>%
  tidytable::mutate(err_src = case_when(name %in% c('ae', 'ae_rel') ~ 'AE',
                                        name %in% c('al', 'al_rel') ~ 'GV',
                                        name %in% c('ae_al', 'ae_al_rel') ~ 'AE & GV',
                                        name %in% c('base', 'base_rel') ~ 'Base'),
                    iss_type = case_when(name %in% c('ae', 'ae_al', 'al', 'base') ~ 'Age composition ISS',
                                         name %in% c('ae_rel', 'ae_al_rel', 'al_rel', 'base_rel') ~ 'Relative age composition ISS'),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV'))) %>%
  tidytable::left_join(spec) %>%
  tidytable::filter(species_type != "other") %>% 
  ggplot(aes(comp_type, value, fill = err_src)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(iss_type ~ species_type,
             scales = "free_y",
             labeller = label_wrap_gen(30),
             switch = "y") +
  ylab("\n") +
  xlab("\nAge composition sex category scenario") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Uncertainty scenario") +
  theme(legend.position = "bottom",
        strip.text.y.right = element_text(angle = 270),
        strip.placement = "outside",
        strip.text = element_text(size = 12))


dev.off()



# compare pooled vs annual growth (figure 3), avg across sex categories ----

png(filename=here::here("figs", "ann_iss.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

plot_dat %>% 
  tidytable::filter(species_code %in% c(21720, 30060, 10110) & region == 'goa') %>% 
  tidytable::select(year, species_code, comp_type, ae, ae_al, al, base) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', 'goa', 'add_err', 'popollatf_ann_iss_ag.csv')) %>% 
                         tidytable::rename(ae_ann = 'ae',
                                           ae_al_ann = 'ae_al',
                                           al_ann = 'al',
                                           base_ann = 'base')) %>% 
  tidytable::mutate(ae_rel = ae / base,
                    ae_al_rel = ae_al / base,
                    al_rel = al / base,
                    base_rel = 1,
                    ae_ann_rel = ae_ann / base_ann,
                    ae_al_ann_rel = ae_al_ann / base_ann,
                    al_ann_rel = al_ann / base_ann,
                    base_ann_rel = 1) %>% 
  tidytable::pivot_longer(cols = c(ae, ae_al, al, base, 
                                   ae_ann, ae_al_ann, al_ann, base_ann,
                                   ae_rel, ae_al_rel, al_rel, base_rel, 
                                   ae_ann_rel, ae_al_ann_rel, al_ann_rel, base_ann_rel)) %>%
  tidytable::mutate(pool_type = case_when(name %in% c('ae', 'ae_al', 'al', 'base',
                                                      'ae_rel', 'ae_al_rel', 'al_rel', 'base_rel') ~ 'pooled',
                                          name %in% c('ae_ann', 'ae_al_ann', 'al_ann', 'base_ann',
                                                      'ae_ann_rel', 'ae_al_ann_rel', 'al_ann_rel', 'base_ann_rel') ~ 'annual'),
                    err_src = case_when(name %in% c('ae', 'ae_ann', 'ae_rel', 'ae_ann_rel') ~ 'AE',
                                        name %in% c('al', 'al_ann', 'al_rel', 'al_ann_rel') ~ 'GV',
                                        name %in% c('ae_al', 'ae_al_ann', 'ae_al_rel', 'ae_al_ann_rel') ~ 'AE & GV',
                                        name %in% c('base', 'base_ann','base_rel', 'base_ann_rel') ~ 'Base'),
                    iss_type = case_when(name %in% c('ae', 'ae_al', 'al', 'base',
                                                     'ae_ann', 'ae_al_ann', 'al_ann', 'base_ann') ~ 'Age composition ISS',
                                         name %in% c('ae_rel', 'ae_al_rel', 'al_rel', 'base_rel',
                                                     'ae_ann_rel', 'ae_al_ann_rel', 'al_ann_rel', 'base_ann_rel') ~ 'Relative age composition ISS'),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV'))) %>% 
  tidytable::summarise(value = mean(value), .by = c(year, species_code, name, pool_type, err_src, iss_type)) %>% 
  tidytable::left_join(spec) %>% 
  ggplot(aes(pool_type, value, fill = err_src)) +
  geom_boxplot2(width.errorbar = 0, alpha= 0.5) +
  facet_grid(iss_type ~ species_type, 
             scales = "free_y",
             labeller = label_wrap_gen(30),
             switch = "y") +
  theme(legend.position = "bottom",
        strip.text.y.right = element_text(angle = 270),
        strip.placement = "outside",
        strip.text = element_text(size = 12)) +
  xlab("\nGrowth variability scenario") +
  ylab("\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Uncertainty scenario")

dev.off()



# compare bin sizes (figure 4) ----

png(filename=here::here("figs", "bin_iss.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)


vroom::vroom(here::here('output', 'goa', 'add_err', 'popollatf_ann_iss_ag.csv')) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', 'goa', 'add_err', 'popoll_bin2_iss_ag.csv')) %>% 
                         tidytable::rename(ae_2cm = 'ae',
                                           ae_al_2cm = 'ae_al',
                                           al_2cm = 'al',
                                           base_2cm = 'base')) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', 'goa', 'add_err', 'popoll_bin5_iss_ag.csv')) %>% 
                         tidytable::rename(ae_5cm = 'ae',
                                           ae_al_5cm = 'ae_al',
                                           al_5cm = 'al',
                                           base_5cm = 'base')) %>% 
  tidytable::mutate(ae_rel = ae / base,
                    ae_al_rel = ae_al / base,
                    al_rel = al / base,
                    base_rel = 1,
                    ae_2cm_rel = ae_2cm / base_2cm,
                    ae_al_2cm_rel = ae_al_2cm / base_2cm,
                    al_2cm_rel = al_2cm / base_2cm,
                    base_2cm_rel = 1,
                    ae_5cm_rel = ae_5cm / base_5cm,
                    ae_al_5cm_rel = ae_al_5cm / base_5cm,
                    al_5cm_rel = al_5cm / base_5cm,
                    base_5cm_rel = 1) %>% 
  tidytable::pivot_longer(cols = c(ae, ae_al, al, base, 
                                   ae_2cm, ae_al_2cm, al_2cm, base_2cm,
                                   ae_5cm, ae_al_5cm, al_5cm, base_5cm,
                                   ae_rel, ae_al_rel, al_rel, base_rel, 
                                   ae_2cm_rel, ae_al_2cm_rel, al_2cm_rel, base_2cm_rel,
                                   ae_5cm_rel, ae_al_5cm_rel, al_5cm_rel, base_5cm_rel)) %>%
  tidytable::mutate(bin_type = case_when(name %in% c('ae', 'ae_al', 'al', 'base',
                                                     'ae_rel', 'ae_al_rel', 'al_rel', 'base_rel') ~ '1cm',
                                         name %in% c('ae_2cm', 'ae_al_2cm', 'al_2cm', 'base_2cm',
                                                     'ae_2cm_rel', 'ae_al_2cm_rel', 'al_2cm_rel', 'base_2cm_rel') ~ '2cm',
                                         name %in% c('ae_5cm', 'ae_al_5cm', 'al_5cm', 'base_5cm',
                                                     'ae_5cm_rel', 'ae_al_5cm_rel', 'al_5cm_rel', 'base_5cm_rel') ~ '5cm'),
                    err_src = case_when(name %in% c('ae', 'ae_rel', 'ae_2cm', 'ae_2cm_rel', 'ae_5cm', 'ae_5cm_rel') ~ 'AE',
                                        name %in% c('al', 'al_rel', 'al_2cm', 'al_2cm_rel', 'al_5cm', 'al_5cm_rel') ~ 'GV',
                                        name %in% c('ae_al', 'ae_al_rel', 'ae_al_2cm', 'ae_al_2cm_rel', 'ae_al_5cm', 'ae_al_5cm_rel') ~ 'AE & GV',
                                        name %in% c('base', 'base_rel', 'base_2cm', 'base_2cm_rel', 'base_5cm', 'base_5cm_rel') ~ 'Base'),
                    iss_type = case_when(name %in% c('ae', 'ae_al', 'al', 'base',
                                                     'ae_2cm', 'ae_al_2cm', 'al_2cm', 'base_2cm',
                                                     'ae_5cm', 'ae_al_5cm', 'al_5cm', 'base_5cm') ~ 'Age composition ISS',
                                         name %in% c('ae_rel', 'ae_al_rel', 'al_rel', 'base_rel',
                                                     'ae_2cm_rel', 'ae_al_2cm_rel', 'al_2cm_rel', 'base_2cm_rel',
                                                     'ae_5cm_rel', 'ae_al_5cm_rel', 'al_5cm_rel', 'base_5cm_rel') ~ 'Relative age composition ISS'),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV'))) %>% 
  tidytable::summarise(value = mean(value), .by = c(year, species_code, name, bin_type, err_src, iss_type)) %>% 
  tidytable::left_join(spec) %>% 
  ggplot(aes(bin_type, value, fill = err_src)) +
  geom_boxplot2(width.errorbar = 0, alpha= 0.5) +
  facet_grid(iss_type ~ species_type, 
             scales = "free_y",
             labeller = label_wrap_gen(30),
             switch = "y") +
  theme(legend.position = "bottom",
        strip.text.y.right = element_text(angle = 270),
        strip.placement = "outside",
        strip.text = element_text(size = 12)) +
  xlab("\nLength bin scenario") +
  ylab("\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Uncertainty scenario")

dev.off()


# plot caal results (figure 5) ----

png(filename=here::here("figs", "caal_iss.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

vroom::vroom(here::here('output', 'goa', 'add_err', 'pcod_caal_iss_ag.csv')) %>% 
  tidytable::summarise(ae = mean(ae, na.rm = TRUE),
                       al = mean(al, na.rm = TRUE),
                       ae_al = mean(ae_al, na.rm = TRUE),
                       base = mean(base, na.rm = TRUE), .by = c(year, species_code, comp_type)) %>% 
  tidytable::mutate(ae_rel = ae / base,
                    ae_al_rel = ae_al / base,
                    al_rel = al / base,
                    base_rel = 1) %>% 
  tidytable::pivot_longer(cols = c(ae, ae_al, al, base,
                                   ae_rel, ae_al_rel, al_rel, base_rel)) %>% 
  tidytable::mutate(err_src = case_when(name %in% c('ae', 'ae_rel') ~ 'AE',
                                        name %in% c('al', 'al_rel') ~ 'GV',
                                        name %in% c('ae_al', 'ae_al_rel') ~ 'AE & GV',
                                        name %in% c('base', 'base_rel') ~ 'Base'),
                    iss_type = case_when(name %in% c('ae', 'ae_al', 'al', 'base') ~ 'Conditional age-at-length ISS',
                                         name %in% c('ae_rel', 'ae_al_rel', 'al_rel', 'base_rel') ~ 'Relative conditional age-at-length ISS'),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV'))) %>% 
  tidytable::left_join(spec) %>% 
  ggplot(aes(comp_type, value, fill = err_src)) +
  geom_boxplot2(width.errorbar = 0, alpha= 0.5) +
  facet_grid(iss_type ~ species_name, 
             scales = "free_y",
             labeller = label_wrap_gen(50),
             switch = "y") +
  theme(legend.position = "bottom",
        strip.text.y.right = element_text(angle = 270),
        strip.placement = "outside",
        strip.text = element_text(size = 12)) +
  xlab("\nAge composition sex category") +
  ylab("\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Uncertainty scenario")

  
  scico::scico_palette_show('roma')
  
  dev.off()
  
  
# plot iss and nss per haul with added error (figure 6) ----

plot_dat %>% 
  select(year, species_code, comp_type, ae, ae_al, al, base, nhls, nss, region) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::mutate(ae_hl = ae / nhls,
                    al_hl = al / nhls,
                    ae_al_hl = ae_al / nhls,
                    base_hl = base / nhls,
                    n_hl = nss/ nhls) %>% 
  tidytable::pivot_longer(cols = c(ae_hl, al_hl, ae_al_hl, base_hl)) %>% 
  tidytable::mutate(err_src = case_when(name == 'ae_hl' ~ 'AE',
                                        name == 'al_hl' ~ 'GV',
                                        name == 'ae_al_hl' ~ 'AE & GV',
                                        name == 'base_hl' ~ 'Base'),
                    surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "eastern Bering Sea shelf"),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV')),
                    surv_labs = factor(surv_labs)) %>% 
  tidytable::filter(species_type != 'other') -> hls_dat

hls_dat %>% 
  ggplot(aes(n_hl, value, color = err_src)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, lty=3) +
  facet_grid( ~ species_type,
              labeller = labeller(region = surv_labs)) +
  xlab("\nNumber of age samples per sampled haul") +
  ylab("Age composition input sample size per sampled haul\n") +
  labs(pch = "Stock") +
  scale_color_scico_d(palette = 'roma',
                      name = "Species type") + 
  scale_fill_scico_d(palette = 'roma',
                     name = "Species type") +
  theme(legend.position = "none") + 
  geom_smooth(method = 'lm',
              formula = y ~ x + 0) +
  stat_poly_eq(label.x.npc = "right",
               label.y.npc = "bottom", 
               formula = y ~ x + 0,
               parse = TRUE) -> p1

hls_dat %>% 
  tidytable::drop_na() %>% 
  ggplot(aes(err_src, value, fill = err_src)) +
  geom_boxplot2(width.errorbar = 0, alpha= 0.5) +
  facet_grid( ~ species_type) +
  xlab("\nUncertainty scenario") +
  ylab("Age composition input sample size per sampled haul") +
  scale_color_scico_d(palette = 'roma') + 
  scale_fill_scico_d(palette = 'roma', alpha = 0.5) + 
  theme(legend.position = "none") -> p2

ggpubr::ggarrange(p1 + ggpubr::rremove("ylab"),
                  p2 + ggpubr::rremove("ylab"),
                  ncol= 1) -> fig


png(filename=here::here("figs", "alt_hls-iss-nss.png"), 
    width = 6.5, height = 6.5,
    units = "in", res=200)

ggpubr::annotate_figure(fig, 
                        left = grid::textGrob("Age composition ISS per sampled haul\n", 
                                              rot = 90, vjust = 1, 
                                              gp = grid::gpar(cex = 1, fontface="plain", fontfamily="Times New Roman")))

dev.off()







# plot iss and nss with added error (alt figure 6) ----

plot_dat %>% 
  select(year, species_code, comp_type, ae, ae_al, al, base, nhls, nss, region) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::pivot_longer(cols = c(ae, al, ae_al, base)) %>% 
  tidytable::mutate(err_src = case_when(name == 'ae' ~ 'AE',
                                        name == 'al' ~ 'GV',
                                        name == 'ae_al' ~ 'AE & GV',
                                        name == 'base' ~ 'Base'),
                    surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "eastern Bering Sea shelf"),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV')),
                    surv_labs = factor(surv_labs)) %>% 
  tidytable::filter(species_type != 'other') %>% 
  ggplot(aes(nss, value, color = err_src)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, lty=3) +
  # stat_ellipse(aes(fill = species_type, 
  #                  color = species_type), 
  #              alpha = 0.3, 
  #              level = 0.95,
  #              type = "norm",
  #              geom = "polygon") +
  facet_grid( ~ species_type,
              labeller = labeller(region = surv_labs)) +
  xlab("\nNumber of age samples") +
  ylab("Age composition ISS\n") +
  labs(pch = "Stock") +
  scale_color_scico_d(palette = 'roma',
                      name = "Species type") + 
  scale_fill_scico_d(palette = 'roma',
                     name = "Species type") +
  theme(legend.position = "none") + 
  geom_smooth(method = 'lm',
              formula = y ~ x + 0) +
  stat_poly_eq(label.x.npc = "right",
               label.y.npc = "bottom", 
               formula = y ~ x + 0,
               parse = TRUE) -> p1

plot_dat %>% 
  select(year, species_code, comp_type, ae, ae_al, al, base, nhls, nss, region) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::mutate(ae_ss = ae / nss,
                    al_ss = al / nss,
                    ae_al_ss = ae_al / nss,
                    base_ss = base / nss) %>% 
  tidytable::pivot_longer(cols = c(ae_ss, al_ss, ae_al_ss, base_ss)) %>% 
  tidytable::mutate(err_src = case_when(name == 'ae_ss' ~ 'AE',
                                        name == 'al_ss' ~ 'GV',
                                        name == 'ae_al_ss' ~ 'AE & GV',
                                        name == 'base_ss' ~ 'Base'),
                    surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "eastern Bering Sea shelf"),
                    err_src = factor(err_src, level = c('Base', 'AE', 'GV', 'AE & GV')),
                    surv_labs = factor(surv_labs)) %>% 
  tidytable::filter(species_type != 'other') %>% 
  ggplot(aes(err_src, value, fill = err_src)) +
  geom_boxplot2(width.errorbar = 0, alpha= 0.5) +
  facet_grid( ~ species_type) +
  xlab("\nUncertainty scenario") +
  ylab("Age composition ISS per age sample") +
  scale_color_scico_d(palette = 'roma') + 
  scale_fill_scico_d(palette = 'roma', alpha = 0.5) + 
  theme(legend.position = "none") -> p2

ggpubr::ggarrange(p1 + ggpubr::rremove("ylab"),
                  p2 + ggpubr::rremove("ylab"),
                  ncol= 1) -> fig


png(filename=here::here("figs", "alt_hls-iss-nss.png"), 
    width = 6.5, height = 6.5,
    units = "in", res=200)

ggpubr::annotate_figure(fig, 
                        left = grid::textGrob("Age composition ISS per age sample\n", 
                                              rot = 90, vjust = 0.5, 
                                              gp = grid::gpar(cex = 1, fontface="plain", fontfamily="Times New Roman")))

dev.off()








# plot relationship with ape and sd_l ----

plot_dat %>% 
  tidytable::left_join(spec) %>%
  tidytable::mutate(prop_ae = ae / base,
                    prop_al = al / base,
                    prop_ae_al = ae_al / base) %>% 
  tidytable::pivot_longer(cols = c(prop_ae, prop_ae_al, prop_al)) %>% 
  tidytable::mutate(err_src = case_when(name == 'prop_ae' ~ 'AE',
                                        name == 'prop_al' ~ 'GV',
                                        name == 'prop_ae_al' ~ 'AE & GV'),
                    surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "Eastern Bering Sea Shelf"),
                    err_src = factor(err_src)) -> plot_dat_ape



plot_dat_ape %>% 
  tidytable::filter(err_src == 'AE', species_type != 'other') %>% 
  tidytable::summarise(mean_ae = mean(value, na.r = TRUE), .by = c(species_name, species_type, ape, cv_a, comp_type, region)) %>% 
  ggplot(.,aes(x = cv_a, y = mean_ae, color = as.factor(species_type))) +
  geom_point() +
  stat_ellipse(aes(fill = species_type,
                   color = species_type),
               alpha = 0.25,
               level = 0.95,
               type = "norm",
               geom = "polygon") +
  xlab("Average reader-tester CV") +
  ylab("AE") +
  scale_color_scico_d(palette = 'roma',
                      name = "Species type") + 
  scale_fill_scico_d(palette = 'roma',
                     name = "Species type") + 
  theme(text = element_text(size = 10),
        legend.position = "none") -> p1


plot_dat_ape %>% 
  tidytable::filter(err_src == 'GV', species_type != 'other') %>% 
  tidytable::summarise(mean_ae = mean(value, na.r = TRUE), .by = c(species_name, species_type, sd_l, cv_l, region)) %>% 
  ggplot(.,aes(x = cv_l, y = mean_ae, color = as.factor(species_type))) +
  geom_point() +
  stat_ellipse(aes(fill = species_type,
                   color = species_type),
               alpha = 0.25,
               level = 0.95,
               type = "norm",
               geom = "polygon") +
  xlab("Average age-length CV") +
  ylab("GV") +
  scale_color_scico_d(palette = 'roma',
                      name = "Species type") + 
  scale_fill_scico_d(palette = 'roma',
                     name = "Species type") + 
  theme(text = element_text(size = 10),
        legend.position = "none") +
  scale_x_continuous(breaks = c(0, 0.05, 0.1, 0.15)) +
  expand_limits(x=0) -> p2

plot_dat_ape %>% 
  tidytable::filter(err_src == 'AE & GV', species_type != 'other') %>% 
  tidytable::summarise(mean_ae = mean(value, na.rm = TRUE),
                       lci_hl = quantile(value, probs = 0.025, na.rm = TRUE),
                       uci_hl = quantile(value, probs = 0.975, na.rm = TRUE), 
                       .by = c(species_name, species_type)) %>% 
  ggplot(aes(reorder(species_name, -mean_ae), mean_ae, fill = species_type)) +
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_errorbar(aes(x = reorder(species_name, mean_ae), 
                    ymin = lci_hl, ymax = uci_hl), 
                width = 0) +
  scale_fill_scico_d(palette = 'roma',
                     name = "") + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank(),
        legend.position = c(0.47, 0.9)) +
  # guides(fill = guide_legend(nrow = 1)) +
  ylab("AE & GV") +
  ylim(0, 1.3) -> p3

ggpubr::ggarrange(ggpubr::ggarrange(p1, p2, ncol = 2),
                  p3,
                  nrow = 2) -> fig

png(filename=here::here("figs", "alt_ae-gv-stats.png"), 
    width = 6.5, height = 6.5,
    units = "in", res=200)

ggpubr::annotate_figure(fig, 
                        left = grid::textGrob("Relative age composition input sample size\n", 
                                              rot = 90, vjust = 1, 
                                              gp = grid::gpar(cex = 1, fontface="plain", fontfamily="Times New Roman")))

dev.off()

