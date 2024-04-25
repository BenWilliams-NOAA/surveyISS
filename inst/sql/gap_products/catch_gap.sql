SELECT
    gap_products.akfin_cruise.year,
    gap_products.akfin_cruise.survey_definition_id as survey,
    gap_products.akfin_cruise.design_year as design_year,
    gap_products.akfin_haul.stratum,
    gap_products.akfin_haul.hauljoin,
    (gap_products.akfin_haul.latitude_dd_start + gap_products.akfin_haul.latitude_dd_end) / 2 as lat_mid,
    (gap_products.akfin_haul.longitude_dd_start + gap_products.akfin_haul.longitude_dd_end) / 2 as long_mid,
    gap_products.akfin_haul.distance_fished_km as distance_fished_km,
    gap_products.akfin_haul.net_width as net_width,
    gap_products.akfin_catch.species_code,
    gap_products.akfin_catch.count
FROM gap_products.akfin_haul INNER JOIN gap_products.akfin_cruise 
  ON gap_products.akfin_cruise.cruisejoin = gap_products.akfin_haul.cruisejoin
    INNER JOIN gap_products.akfin_catch
      ON gap_products.akfin_haul.hauljoin = gap_products.akfin_catch.hauljoin
WHERE
gap_products.akfin_cruise.survey_definition_id
-- insert survey
AND gap_products.akfin_catch.species_code
-- insert species
AND gap_products.akfin_cruise.year
-- insert year 
