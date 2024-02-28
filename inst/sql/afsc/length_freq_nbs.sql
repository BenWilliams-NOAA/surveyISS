SELECT 
    year,
    species_code,
    stratum,
    racebase.length.hauljoin,
    sex,
    length, 
    frequency
FROM racebase.haul INNER JOIN racebase.length 
    ON racebase.haul.hauljoin = racebase.length.hauljoin
        INNER JOIN HAEHNR.SURVEYS_NBS
            ON  racebase.length.cruisejoin = HAEHNR.SURVEYS_NBS.cruisejoin 
WHERE
racebase.length.region  
-- insert region
AND species_code 
-- insert species
AND HAEHNR.SURVEYS_NBS.year
-- insert year
AND racebase.haul.abundance_haul = 'Y' 
