SELECT 
    species_code,
    stratum,
    racebase.length.hauljoin,
    racebase.haul.vessel,
    racebase.haul.cruise,
    sex,
    length, 
    frequency
FROM racebase.haul INNER JOIN racebase.length 
    ON racebase.haul.hauljoin = racebase.length.hauljoin
WHERE
racebase.length.region  
-- insert region
AND species_code 
-- insert species
AND racebase.haul.abundance_haul = 'Y'
AND racebase.haul.vessel IN (57,134,94,178)
AND racebase.haul.cruise IN (200202,200401,200801,201001,201201,201601)
