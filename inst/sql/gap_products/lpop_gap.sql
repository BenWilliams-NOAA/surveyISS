SELECT 
    gap_products.akfin_sizecomp.survey_definition_id as survey,
    gap_products.akfin_sizecomp.year,
    gap_products.akfin_sizecomp.area_id as stratum,
    gap_products.akfin_sizecomp.species_code,
    gap_products.akfin_sizecomp.length_mm as length,
    gap_products.akfin_sizecomp.sex,
    gap_products.akfin_sizecomp.population_count
FROM gap_products.akfin_sizecomp
WHERE 
gap_products.akfin_sizecomp.survey_definition_id
-- insert survey
AND gap_products.akfin_sizecomp.species_code
-- insert species
AND gap_products.akfin_sizecomp.year
-- insert year 
