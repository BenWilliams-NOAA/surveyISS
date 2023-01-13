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
        INNER JOIN HAEHNR.SURVEYS_EBSSHELF
            ON racebase.specimen.cruisejoin = HAEHNR.SURVEYS_EBSSHELF.cruisejoin 
WHERE racebase.specimen.species_code
-- insert species
AND racebase.specimen.region
-- insert region
AND HAEHNR.SURVEYS_EBSSHELF.year
-- insert year
