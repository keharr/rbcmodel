## code to prepare `DATASET` dataset goes here

Rubisco_abridged_cols = readr::cols(
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

Rubisco_alias_cols = readr::cols(
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

temp_averaged_cols = readr::cols(
  identifier = readr::col_character(),
  kcat_dH = readr::col_double(),
  Kc_dH = readr::col_double(),
  Ko_dH = readr::col_double(),
  S_dH = readr::col_double(),
  Form = readr::col_character(),
  Taxonomy = readr::col_character(),
  Note = readr::col_character(),
  short_ref = readr::col_character()
)

temp_abridged_cols = readr::cols(
  identifier = readr::col_character(),
  genus = readr::col_character(),
  species = readr::col_character(),
  subspecies = readr::col_character(),
  kcat_dH = readr::col_double(),
  Kc_dH = readr::col_double(),
  Ko_dH = readr::col_double(),
  S_dH = readr::col_double(),
  Form = readr::col_character(),
  Taxonomy = readr::col_character(),
  Note = readr::col_character(),
  short_ref = readr::col_character()
)

Rubisco_aliases <- readr::read_csv(
  "inst/extdata/Rubisco_kinetics_alias.csv",
  col_types = Rubisco_alias_cols
)
Rubisco_aliases <- as.data.frame(Rubisco_aliases)

Rubisco_abridged <- readr::read_csv(
  "inst/extdata/Rubisco_kinetics_abridged.csv",
  col_types = Rubisco_abridged_cols
)
Rubisco_abridged <- as.data.frame(Rubisco_abridged)

Rubisco_25C <- readr::read_csv(
  "inst/extdata/Rubisco_kinetics_25C.csv",
  col_types = Rubisco_kinetics_cols,
  na = c("", "na", "nf", "NA")
)
Rubisco_25C <- as.data.frame(Rubisco_25C)

temp_dep_averaged <- readr::read_csv(
  "inst/extdata/temp_dependence_averaged.csv",
  col_types = temp_averaged_cols,
  na = c("", "NA")
)
temp_dep_averaged <- as.data.frame(temp_dep_averaged)

temp_dep_abridged <- readr::read_csv(
  "inst/extdata/temp_dependence_abridged.csv",
  col_types = temp_abridged_cols,
  na = c("", "NA")
)
temp_dep_abridged <- as.data.frame(temp_dep_abridged)

usethis::use_data(Rubisco_aliases, overwrite=TRUE)
usethis::use_data(Rubisco_abridged, overwrite=TRUE)
usethis::use_data(Rubisco_25C, overwrite=TRUE)
usethis::use_data(temp_dep_averaged, overwrite=TRUE)
usethis::use_data(temp_dep_abridged, overwrite=TRUE)
usethis::use_data(
  Rubisco_aliases, Rubisco_abridged, Rubisco_25C,
  temp_dep_averaged,
  internal=TRUE, overwrite=TRUE
)
