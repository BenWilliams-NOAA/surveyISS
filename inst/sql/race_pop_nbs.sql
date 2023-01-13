SELECT
    survey,
    year,
    species_code,
    stratum,
    length,
    males,
    females,
    unsexed,
    total
FROM
    haehnr.nbs_sizecomp
WHERE
    stratum > 990
    AND year
    -- insert year
    AND species_code
    -- insert species