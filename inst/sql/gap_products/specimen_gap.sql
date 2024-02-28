SELECT 
    gap_products.akfin_cruise.year,
    gap_products.akfin_cruise.survey_definition_id as survey,
    gap_products.akfin_specimen.species_code,
    gap_products.akfin_haul.stratum,
    gap_products.akfin_specimen.hauljoin,
    gap_products.akfin_specimen.sex,
    gap_products.akfin_specimen.length_mm as length, 
    gap_products.akfin_specimen.weight_g as weight, 
    gap_products.akfin_specimen.age
FROM gap_products.akfin_haul INNER JOIN gap_products.akfin_cruise 
  ON gap_products.akfin_cruise.cruisejoin = gap_products.akfin_haul.cruisejoin
    INNER JOIN gap_products.akfin_specimen 
      ON gap_products.akfin_haul.hauljoin = gap_products.akfin_specimen.hauljoin
WHERE
gap_products.akfin_cruise.survey_definition_id
-- insert survey
AND gap_products.akfin_specimen.species_code
-- insert species
AND gap_products.akfin_cruise.year
-- insert year 
