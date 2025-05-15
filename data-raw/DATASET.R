## code to prepare `DATASET` dataset goes here

abridged_cols = readr::cols(
  identifier = readr::col_character(),
  Genus = readr::col_character(),
  Species = readr::col_character(),
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
  taxonomy = readr::col_character(),
  note = readr::col_character()
)

abridged <- readr::read_csv(
  "inst/extdata/abridged_table.csv",
  col_types = abridged_cols
)

Rubisco_kinetics_cols = readr::cols(
  identifier = readr::col_character(),
  Genus = readr::col_character(),
  Species = readr::col_character(),
  Subspecies = readr::col_character(),
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
  pKa = readr::col_double(),
  form = readr::col_character(),
  taxonomy = readr::col_character(),
  note = readr::col_character(),
  short_ref = readr::col_character(),
  pmid_or_doi = readr::col_character(),
  reference = readr::col_character(),
  year = readr::col_character()
)

Rubisco_kinetics <- readr::read_csv(
  "inst/extdata/Rubisco_kinetics_table.csv",
  col_types = Rubisco_kinetics_cols
)

usethis::use_data(abridged, overwrite=TRUE)
usethis::use_data(Rubisco_kinetics, overwrite=TRUE)
usethis::use_data(abridged, Rubisco_kinetics, internal=TRUE, overwrite=TRUE)
