SELECT 
    survey,
    stratum,
    area,
    summary_area
FROM goa.goa_strata
WHERE survey
-- insert region
