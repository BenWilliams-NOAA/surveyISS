SELECT
    gap_products.akfin_taxonomic_classification.species_code,
    gap_products.akfin_taxonomic_classification.species_name,
    gap_products.akfin_taxonomic_classification.common_name
FROM gap_products.akfin_taxonomic_classification
WHERE gap_products.akfin_taxonomic_classification.species_code 
-- insert species
