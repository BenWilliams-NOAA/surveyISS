SELECT 
    gap_products.akfin_area.survey_definition_id as survey,
    gap_products.akfin_area.design_year,
    gap_products.akfin_area.area_id as stratum,
    gap_products.akfin_area.area_km2 as area
FROM gap_products.akfin_area 
WHERE 
gap_products.akfin_area.survey_definition_id
-- insert survey
AND gap_products.akfin_area.area_type = 'STRATUM'
