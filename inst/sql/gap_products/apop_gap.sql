SELECT 
    gap_products.akfin_agecomp.survey_definition_id as survey,
    gap_products.akfin_agecomp.year,
    gap_products.akfin_agecomp.area_id as stratum,
    gap_products.akfin_agecomp.species_code,
    gap_products.akfin_agecomp.sex,
    gap_products.akfin_agecomp.age,
    gap_products.akfin_agecomp.population_count
FROM gap_products.akfin_agecomp
WHERE 
gap_products.akfin_agecomp.survey_definition_id
-- insert survey
AND gap_products.akfin_agecomp.species_code
-- insert species
AND gap_products.akfin_agecomp.year
-- insert year 
