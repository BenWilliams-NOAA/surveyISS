SELECT
    gap_products.akfin_cruise.year,
    gap_products.akfin_cruise.survey_definition_id as survey,
    gap_products.akfin_cpue.species_code,
    gap_products.akfin_haul.stratum,
    gap_products.akfin_cpue.hauljoin,
    gap_products.akfin_cpue.cpue_nokm2 as numcpue
FROM gap_products.akfin_haul INNER JOIN gap_products.akfin_cruise 
  ON gap_products.akfin_cruise.cruisejoin = gap_products.akfin_haul.cruisejoin
    INNER JOIN gap_products.akfin_cpue
      ON gap_products.akfin_haul.hauljoin = gap_products.akfin_cpue.hauljoin
WHERE
gap_products.akfin_cruise.survey_definition_id
-- insert survey
AND gap_products.akfin_cpue.species_code
-- insert species
AND gap_products.akfin_cruise.year
-- insert year 
