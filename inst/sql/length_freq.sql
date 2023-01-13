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
        INNER JOIN goa.biennial_surveys
            ON  racebase.length.cruisejoin = goa.biennial_surveys.cruisejoin 
WHERE
racebase.length.region  
-- insert region
AND species_code 
-- insert species
AND goa.biennial_surveys.year
-- insert year
AND racebase.haul.abundance_haul = 'Y' 
