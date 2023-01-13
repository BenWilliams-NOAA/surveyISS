SELECT 
    species_code,
    stratum,
    racebase.specimen.hauljoin,
    racebase.haul.vessel,
    racebase.haul.cruise,
    sex,
    length,
    age
FROM racebase.haul INNER JOIN racebase.specimen
    ON racebase.haul.hauljoin = racebase.specimen.hauljoin
WHERE racebase.specimen.species_code
-- insert species
AND racebase.specimen.region
-- insert region
AND racebase.haul.vessel IN (57,134,94,178)
AND racebase.haul.cruise IN (200202,200401,200801,201201,201601)