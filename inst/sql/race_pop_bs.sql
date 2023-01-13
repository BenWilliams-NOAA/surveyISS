SELECT
   'BS' as SURVEY,
    year,
    species_code,
    subarea,
    length,
    males,
    females,
    unsexed,
    total
FROM
    haehnr.sizecomp_ebs_plusnw
WHERE
    subarea > 990
    AND year
    -- insert year
    AND species_code
    -- insert species
    