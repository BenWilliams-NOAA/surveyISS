#' population at length
#'
#' @param lcomp length comp
#' @param cpue cpue data
#' @param lngs complete lengths by year
#'
#' @export lpop
#'
lpop <- function(lcomp, 
                 cpue, 
                 lngs) {
  lcomp %>%
    tidytable::summarise(comp = sum(comp) / mean(nhauls), 
                         .by = c(year, species_code, stratum, sex, length)) -> .unk
  
  # id hauls without lengths
  # see issue #35 for reasoning 
  cpue %>%
    tidytable::filter(numcpue > 0) %>%
    tidytable::distinct(hauljoin, species_code)  %>% 
    tidytable::anti_join(lcomp %>%
                           tidytable::summarise(hauljoin = unique(hauljoin),
                                                .by = c(species_code, stratum))) %>% 
    tidytable::left_join(cpue) -> .no_length
  
  # compute population est by year, species, strata
  cpue %>%
    tidytable::mutate(st_num = mean(numcpue) * area,
                      tot = sum(numcpue), 
                      .by = c(year, species_code, stratum)) %>%
    tidytable::summarise(abund = mean(numcpue) / tot * st_num,
                         .by = c(year, species_code, stratum, hauljoin)) -> .pop
  
  # if there are any samples w/o lengths rejoin them
  if(nrow(.no_length) == 0){
    lcomp %>%
      tidytable::left_join(.pop) %>%
      tidytable::mutate(sz_pop = round(comp * abund, 0)) %>% 
      tidytable::filter(!is.na(sex)) -> .temp
  } else {
    .no_length %>%
      tidytable::left_join(.unk) %>%
      tidytable::select(year, species_code, stratum, hauljoin, sex, length, comp) %>%
      tidytable::bind_rows(lcomp) %>%
      tidytable::left_join(.pop) %>%
      tidytable::mutate(sz_pop = round(comp * abund, 0)) %>% 
      tidytable::filter(!is.na(sex)) -> .temp
  }
  
  # get annual pop'n @ length
  .temp %>%
    tidytable::summarise(abund = sum(sz_pop, na.rm = T), 
                         .by = c(year, species_code, length, sex)) %>%
    tidytable::left_join(lngs, .) %>%
    tidytable::drop_na()
  
}

#' Compute population at length
#' 
#' @description
#' Function to expand length frequency data to population at length aligned with gapindex package.
#'
#' @param lfreq_un expanded length frequency data (one row per sample) input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param by_strata Boolean. Are the results to be given at strata level, or summed to region level (default = FALSE)
#'
#' @return dataframe of population at length by year and species
#' 
#' @export
#'
lpop_gap <- function(lfreq_un, 
                     cpue_data,
                     by_strata = FALSE) {
  
  # remove ebs strata 82 & 90 (to match results of gapindex)
  if(length(unique(cpue$survey)) == 1 && unique(cpue$survey) %in% c(98)){
    cpue <- cpue %>% 
      tidytable::filter(!(stratum %in% c(82, 90) & year <= 1985))
  }
  
  # note from gapindex -  
  # Wakabayashi et al. 1985 Equation 16:
  # At each station with length-frequency records, the number of individuals
  #     within each sex/length class was estimated by expanding the
  #     length-frequency subsample to the total catch (per area swept).
  #     This is S_ijklm, the estimated relative number of individuals of
  #     species-k with sex-m and length-l caught at station-j within stratum-i.
  #
  # s_ijklm is the "FREQUENCY" column of the `size`, the number of records
  #     of sex-m and length-l of species-k recorded at station-j in stratum-i
  # s_ijk is the frequency of recorded individuals summed over sex and length
  #        class, i.e., the number of individuals of species-k recorded
  #        at station-j in stratum-i summed over all length and sex classes
  # S_ijk is the estimated numbers per area swept from the cpue df
  #     of species-k at station-j in stratum-i. The notation S_ijk is not
  #     used below, instead CPUE_NOKM2 is used.
  #
  # s_ijk and NUMCPUE_IND_KM2 are merged into the size df by joining
  #     via the "HAULJOIN" and "SPECIES_CODE" as a composite key
  #     so that calculations can be vectorized easily.
  
  # male/female/unsexed
  lfreq_un %>%
    tidytable::filter(sex != 0) %>% 
    tidytable::summarise(frequency = .N, 
                         .by = c(year, species_code, stratum, hauljoin, sex, length)) %>% 
    tidytable::inner_join(lfreq_un %>%
                            tidytable::filter(sex != 0) %>% 
                            tidytable::summarise(s_ijk = .N, 
                                                 .by = c(year, species_code, stratum, hauljoin))) %>% 
    tidytable::inner_join(cpue %>%
                            tidytable::filter(numcpue > 0)) %>% 
    ## The size CPUE (S_ijklm)
    tidytable::mutate(S_ijklm = frequency / s_ijk * numcpue) -> size
  
  # combined sex
  lfreq_un %>%
    tidytable::filter(sex == 0) %>% 
    tidytable::summarise(frequency = .N, 
                         .by = c(year, species_code, stratum, hauljoin, sex, length)) %>% 
    tidytable::inner_join(lfreq_un %>%
                            tidytable::filter(sex == 0) %>% 
                            tidytable::summarise(s_ijk = .N, 
                                                 .by = c(year, species_code, stratum, hauljoin))) %>% 
    tidytable::inner_join(cpue %>%
                            tidytable::filter(numcpue > 0)) %>% 
    ## The size CPUE (S_ijklm)
    tidytable::mutate(S_ijklm = frequency / s_ijk * numcpue) -> size_tot
  
  # id hauls without lengths
  # see issue #35 for reasoning 
  #
  # note from gapindex - 
  # For AI and GOA survey  region, the size composition for hauls with
  # positive counts but missing size data is imputted by averaging the
  # size composition from the hauls in that same stratum and year.
  
  if(length(unique(cpue$survey)) == 1 && unique(cpue$survey) %in% c(47, 52)){
    # male/female/unsexed
    size <- size %>% 
      tidytable::select(year, stratum, hauljoin, species_code, length, sex, numcpue, S_ijklm) %>% 
      ## Append the `missing_hauljoins` to size.
      tidytable::bind_rows(cpue %>%
                             tidytable::filter(numcpue > 0) %>%
                             tidytable::distinct(hauljoin, species_code)  %>% 
                             ## ID hauls with positive counts but have no records in `size`
                             tidytable::anti_join(size %>%
                                                    tidytable::summarise(hauljoin = unique(hauljoin),
                                                                         .by = c(species_code, stratum))) %>% 
                             tidytable::inner_join(cpue %>%
                                                     tidytable::filter(numcpue > 0)) %>%
                             ## Calculate mean S_ijklm of individuals of species-k with sex-m and 
                             ## length-l among the hauls within stratum-i. Technically, this would be
                             ## something like S_hat_iklm. 
                             tidytable::inner_join(size %>% 
                                                     tidytable::mutate(p_ijklm = frequency / s_ijk) %>% 
                                                     ## First we sum all the proportions within a stratum...
                                                     tidytable::summarise(p_ijklm = sum(p_ijklm), .by = c(year, stratum, species_code, sex, length)) %>% 
                                                     ## Then we calculate the total number of unique hauls within a stratum...
                                                     tidytable::inner_join(lfreq_un %>%
                                                                             tidytable::filter(sex != 0) %>% 
                                                                             tidytable::summarise(nhauls = data.table::uniqueN(hauljoin),
                                                                                                  .by = c(year, species_code, stratum))) %>% 
                                                     ## Then we calculate the average proportion.
                                                     tidytable::mutate(p_ijklm = p_ijklm / nhauls), .by = c("year", "stratum", "species_code")) %>% 
                             ## Recalculate S_ijklm using the imputted size composition. 
                             tidytable::mutate(S_ijklm = p_ijklm * numcpue) %>% 
                             tidytable::select(year, stratum, hauljoin, species_code, length, sex, numcpue, S_ijklm))
    
    # combined sex
    size_tot <- size_tot %>% 
      tidytable::select(year, stratum, hauljoin, species_code, length, sex, numcpue, S_ijklm) %>% 
      ## Append the `missing_hauljoins` to size.
      tidytable::bind_rows(cpue %>%
                             tidytable::filter(numcpue > 0) %>%
                             tidytable::distinct(hauljoin, species_code)  %>% 
                             ## ID hauls with positive counts but have no records in `size`
                             tidytable::anti_join(size_tot %>%
                                                    tidytable::summarise(hauljoin = unique(hauljoin),
                                                                         .by = c(species_code, stratum))) %>% 
                             tidytable::inner_join(cpue %>%
                                                     tidytable::filter(numcpue > 0)) %>%
                             ## Calculate mean S_ijklm of individuals of species-k with sex-m and 
                             ## length-l among the hauls within stratum-i. Technically, this would be
                             ## something like S_hat_iklm. 
                             tidytable::inner_join(size_tot %>% 
                                                     tidytable::mutate(p_ijklm = frequency / s_ijk) %>% 
                                                     ## First we sum all the proportions within a stratum...
                                                     tidytable::summarise(p_ijklm = sum(p_ijklm), .by = c(year, stratum, species_code, sex, length)) %>% 
                                                     ## Then we calculate the total number of unique hauls within a stratum...
                                                     tidytable::inner_join(lfreq_un %>%
                                                                             tidytable::filter(sex == 0) %>% 
                                                                             tidytable::summarise(nhauls = data.table::uniqueN(hauljoin),
                                                                                                  .by = c(year, species_code, stratum))) %>% 
                                                     ## Then we calculate the average proportion.
                                                     tidytable::mutate(p_ijklm = p_ijklm / nhauls), .by = c("year", "stratum", "species_code")) %>% 
                             ## Recalculate S_ijklm using the imputted size composition. 
                             tidytable::mutate(S_ijklm = p_ijklm * numcpue) %>% 
                             tidytable::select(year, stratum, hauljoin, species_code, length, sex, numcpue, S_ijklm))
  }
  
  # note from gapindex - 
  # Continuing on with Wakabayashi et al.
  #   1985 Equation 17: The number of individuals (population) of species-k
  #   of sex-m and length-l in stratum-i is noted as P_iklm.
  #
  # S_ijklm is summed over hauls-j, creating S_iklm and this is divided
  #    by S_ik, which is S_ijklm summed over hauls-j, sex-m, and length-l.
  # S_iklm / S_ik is the proportion of the stratum population attributed
  #    to sex-m and length-l for species-k in stratum-i. When multiplied by
  #    P_ik, we get P_iklm.
  #
  # P_ik (racebase_stratum_popn$pop) and S_ik are merged into the S_iklm
  #    df via the year, stratum, and species_code values. Wakabayashi et al.
  #    1985 assumes one year of data so there is no year index in their
  #    paper but this function has the capability of calculating size comps
  #    for multiple years of data.
  
  # male/female/unsexed
  size %>% 
    ## Aggregate S_ijklm across stratum, species_code, length bin, and sex
    tidytable::summarise(S_iklm = sum(S_ijklm), .by = c(year, stratum, species_code, length, sex)) %>% 
    ## Aggregate S_ijklm across stratum and species_code
    tidytable::inner_join(size %>% 
                            tidytable::summarise(S_ik = sum(S_ijklm),
                                                 .by = c(year, stratum, species_code))) %>% 
    # join pop'n estimates
    tidytable::inner_join(cpue %>%
                            tidytable::filter(numcpue >= 0) %>%
                            tidytable::mutate(st_num = mean(numcpue) * area,
                                              tot = sum(numcpue), 
                                              .by = c(year, species_code, stratum)) %>%
                            tidytable::summarise(abund = mean(numcpue) / tot * st_num,
                                                 .by = c(year, species_code, stratum, hauljoin)) %>% 
                            tidytable::summarise(abund = sum(abund, na.rm = TRUE), 
                                                 .by = c(year, species_code, stratum))) %>% 
    tidytable::replace_na(list(length = -9, sex = 3, S_iklm = 1, S_ik = 1)) %>%   # There are some strata with no length data. In these cases, the length
    # is coded as -9 and sex = 3. S_ik and S_ik are set to 1 to ease calculations
    tidytable::mutate(number = round(abund * S_iklm / S_ik)) %>% # Stratum-level population calculation
    tidytable::filter(number > 0) %>% ## Remove zero-records
    tidytable::select(-S_iklm, - S_ik, -abund) %>% 
    tidytable::rename(abund = 'number') %>% # rename
    # combined sex
    tidytable::bind_rows(size_tot %>% # Aggregate S_ijklm across stratum, species_code, length bin, and sex
                           tidytable::summarise(S_iklm = sum(S_ijklm), .by = c(year, stratum, species_code, length, sex)) %>% 
                           tidytable::inner_join(size_tot %>% ## Aggregate S_ijklm across stratum and species_code
                                                   tidytable::summarise(S_ik = sum(S_ijklm),
                                                                        .by = c(year, stratum, species_code))) %>% 
                           tidytable::inner_join(cpue %>% # join pop'n estimates
                                                   tidytable::filter(numcpue >= 0) %>%
                                                   tidytable::mutate(st_num = mean(numcpue) * area,
                                                                     tot = sum(numcpue), 
                                                                     .by = c(year, species_code, stratum)) %>%
                                                   tidytable::summarise(abund = mean(numcpue) / tot * st_num,
                                                                        .by = c(year, species_code, stratum, hauljoin)) %>% 
                                                   tidytable::summarise(abund = sum(abund, na.rm = TRUE), 
                                                                        .by = c(year, species_code, stratum))) %>% 
                           ## There are some strata with no length data. In these cases, the length
                           ## is coded as -9 and sex = 3. S_ik and S_ik are set to 1 to ease calculations
                           tidytable::replace_na(list(length = -9, sex = 3, S_iklm = 1, S_ik = 1)) %>% 
                           ## Stratum-level population calculation
                           tidytable::mutate(number = round(abund * S_iklm / S_ik)) %>% 
                           ## Remove zero-records
                           tidytable::filter(number > 0) %>% 
                           tidytable::select(-S_iklm, - S_ik, -abund) %>% 
                           # rename
                           tidytable::rename(abund = 'number')) -> lpopn
  
  if(isTRUE(by_strata)){
    lpopn
  } else{
    # get annual pop'n @ length
    lpopn %>% 
      tidytable::summarise(abund = sum(abund, na.rm = T),
                           .by = c(year, species_code, length, sex))
  }
  
}