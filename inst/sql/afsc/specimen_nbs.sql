SELECT 
    year,
    species_code,
    stratum,
    racebase.specimen.hauljoin,
    sex,
    length,
    age
FROM racebase.haul INNER JOIN racebase.specimen
    ON racebase.haul.hauljoin = racebase.specimen.hauljoin
        INNER JOIN HAEHNR.SURVEYS_NBS
            ON racebase.specimen.cruisejoin = HAEHNR.SURVEYS_NBS.cruisejoin 
WHERE racebase.specimen.species_code
-- insert species
AND racebase.specimen.region
-- insert region
AND HAEHNR.SURVEYS_NBS.year
-- insert year