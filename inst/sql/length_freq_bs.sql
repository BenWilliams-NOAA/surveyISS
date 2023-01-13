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
        INNER JOIN HAEHNR.SURVEYS_EBSSHELF
            ON  racebase.length.cruisejoin = HAEHNR.SURVEYS_EBSSHELF.cruisejoin 
WHERE
racebase.length.region  
-- insert region
AND species_code 
-- insert species
AND HAEHNR.SURVEYS_EBSSHELF.year
-- insert year
AND racebase.haul.abundance_haul = 'Y' 
