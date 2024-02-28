SELECT
'BS' as SURVEY,
year,
species_code,
stratum as subarea,
length,
males,
females,
unsexed,
total
FROM
hoffj.sizecomp_ebsslope
WHERE
stratum < 9999
AND stratum >6
AND year
-- insert year
AND species_code
-- insert species
