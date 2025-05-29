## code to prepare `DATASET` dataset goes here

abridged_cols = readr::cols(
  identifier = readr::col_character(),
  genus = readr::col_character(),
  species = readr::col_character(),
  subspecies = readr::col_character(),
  kcat_val = readr::col_double(),
  kcat_T = readr::col_double(),
  Kc_val = readr::col_double(),
  Kc_T = readr::col_double(),
  Ko_val = readr::col_double(),
  Ko_T = readr::col_double(),
  S_val = readr::col_double(),
  S_T = readr::col_double(),
  temp = readr::col_double(),
  form = readr::col_character(),
  PGS = readr::col_character(),
  taxonomy = readr::col_character(),
  note = readr::col_character()
)

alias_cols = readr::cols(
  alternate_name = readr::col_character(),
  genus_database = readr::col_character(),
  species_database = readr::col_character(),
  subspecies_database = readr::col_character(),
)

Rubisco_kinetics_cols = readr::cols(
  identifier = readr::col_character(),
  genus = readr::col_character(),
  species = readr::col_character(),
  subspecies = readr::col_character(),
  mutant = readr::col_logical(),
  mutant_details = readr::col_character(),
  primary = readr::col_logical(),
  heterologous_expression = readr::col_character(),
  kcat_val = readr::col_double(),
  kcat_T = readr::col_double(),
  kcat_pH = readr::col_double(),
  Kc_val = readr::col_double(),
  Kc_T = readr::col_double(),
  Kc_pH = readr::col_double(),
  Ko_val = readr::col_double(),
  Ko_T = readr::col_double(),
  Ko_pH = readr::col_double(),
  S_val = readr::col_double(),
  S_T = readr::col_double(),
  S_pH = readr::col_double(),
  temp = readr::col_double(),
  pH = readr::col_double(),
  pKa_used = readr::col_double(),
  form = readr::col_character(),
  PGS = readr::col_character(),
  taxonomy = readr::col_character(),
  note = readr::col_character(),
  short_ref = readr::col_character(),
  pmid_or_doi = readr::col_character(),
  citation = readr::col_character(),
  year = readr::col_character()
)

Rubisco_aliases <- readr::read_csv(
  "inst/extdata/alias_table.csv",
  col_types = alias_cols
)

Rubisco_abridged <- readr::read_csv(
  "inst/extdata/abridged_table.csv",
  col_types = abridged_cols
)

Rubisco_25C <- readr::read_csv(
  "inst/extdata/Rubisco_kinetics_25C.csv",
  col_types = Rubisco_kinetics_cols,
  na = c("", "na", "nf", "NA")
)

usethis::use_data(Rubisco_aliases, overwrite=TRUE)
usethis::use_data(Rubisco_abridged, overwrite=TRUE)
usethis::use_data(Rubisco_25C, overwrite=TRUE)
usethis::use_data(
  Rubisco_aliases, Rubisco_abridged, Rubisco_25C, 
  internal=TRUE, overwrite=TRUE
)
