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
        INNER JOIN goa.biennial_surveys
            ON racebase.specimen.cruisejoin = goa.biennial_surveys.cruisejoin 
WHERE racebase.specimen.species_code
-- insert species
AND racebase.specimen.region
-- insert region
AND goa.biennial_surveys.year
-- insert year
