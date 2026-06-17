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
  group = readr::col_character(),
  note = readr::col_character(),
  short_ref = readr::col_character()
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
  group = readr::col_character(),
  note = readr::col_character(),
  short_ref = readr::col_character(),
  year = readr::col_character()
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
  form = readr::col_character(),
  taxonomy = readr::col_character(),
  group = readr::col_character(),
  note = readr::col_character(),
  short_ref = readr::col_character()
)

refs_cols = readr::cols(
  short_ref = readr::col_character(),
  pmid_or_doi = readr::col_character(),
  citation = readr::col_character()
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

refs_to_citation <- readr::read_csv(
  "inst/extdata/refs_to_citation.csv",
  col_types = refs_cols,
)
refs_to_citation <- as.data.frame(refs_to_citation)

temp_dep_abridged <- readr::read_csv(
  "inst/extdata/temp_dependence_abridged.csv",
  col_types = temp_abridged_cols,
  na = c("", "NA")
)
temp_dep_abridged <- as.data.frame(temp_dep_abridged)

Rubisco_abridged <- rbind(Rubisco_abridged,
#averages
#avg. 1Ac (cyanobacteria)
c("average_1Ac_cyanobacteria","Average","1Ac","Cyanobacteria",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                              Rubisco_25C[["group"]]=="Bacteria"&
                              Rubisco_25C[["form"]]=="1Ac"&
                              Rubisco_25C[["primary"]]=="TRUE"&
                              ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                              Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                                        Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Ac"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                                        Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Ac"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                                        Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Ac"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                na.rm=TRUE),digits=1),NA,
  25,"1Ac","canon","Cyanobacteria","Bacteria",
  "values medians of collected cyanobacterial measurements at 25C",
  "this package"),
#avg. 1Ac (proteobacteria)
c("average_1Ac_proteobacteria","Average","1Ac","Proteobacteria",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]%in%c("Alphaproteobacteria","Betaproteobacteria","Gammaproteobacteria")&
                                        Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Ac"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]=="30"&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]=="30"&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),"30",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]%in%c("Alphaproteobacteria","Betaproteobacteria","Gammaproteobacteria")&
                                        Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Ac"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                                        Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Ac"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]%in%c("Alphaproteobacteria","Betaproteobacteria","Gammaproteobacteria")&
                                        Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Ac"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]=="30"&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]=="30"&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),"30",
  25,"1Ac",NA,NA,"Bacteria",
  "kcat & S median of 1Ac proteobacteria at 30C, Kc median of 1Ac proteobacteria at 25C, Ko median of 1Ac cyanobacteria at 25C",
  "this package"),
#avg. 1Aq
c("average_1Aq","Average","1Aq",NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Aq"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Aq"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Aq"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["group"]]=="Bacteria"&
                                        Rubisco_25C[["form"]]=="1Aq"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),NA,
  25,"1Aq",NA,NA,"Bacteria",
  "values medians of collected 1Aq measurements at 25C",
  "this package"),
#avg. C3 plant
c("average_1B_C3_plants","Average","1B","C3 plants",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),NA,
  25,"1B","canon","C3 plants","Plants",
  "values medians of collected C3 measurements at 25C",
  "this package"),
#avg. C4 plant
c("average_1B_C4_plants","Average","1B","C4 plants",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C4 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C4 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C4 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C4 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),NA,
  25,"1B","canon","C4 plants","Plants",
  "values medians of collected C4 measurements at 25C",
  "this package"),
#avg. C3-C4 plant
c("average_1B_C3-C4_plants","Average","1B","C3-C4 plants",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3-C4 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3-C4 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3-C4 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3-C4 plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),NA,
  25,"1B","canon","C3-C4 plants","Plants",
  "values medians of collected C3-C4 measurements at 25C",
  "this package"),
#avg. CAM plant
c("average_1B_CAM_plants","Average","1B","CAM plants",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="CAM plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="CAM plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="CAM plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="CAM plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),NA,
  25,"1B","canon","CAM plants","Plants",
  "values medians of collected CAM measurements at 25C",
  "this package"),
#avg. bryophyte
c("average_1B_bryophytes","Average","1B","Bryophytes",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Bryophytes"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Bryophytes"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Bryophytes"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Bryophytes"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),NA,
  25,"1B","canon","Bryophytes","Plants",
  "values medians of collected Bryophyte measurements at 25C",
  "this package"),
#avg. aquatic plant
c("average_1B_Aquatic_plants","Average","1B","Aquatic plants",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Aquatic plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Aquatic plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Aquatic plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Aquatic plants"&
                                        Rubisco_25C[["group"]]=="Plants"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),NA,
  25,"1B","canon","Aquatic plants","Plants",
  "values medians of collected aquatic plant measurements at 25C",
  "this package"),
#avg. green algae
c("average_1B_Green_algae","Average","1B","Green algae",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Green algae"&
                                        Rubisco_25C[["group"]]=="Algae"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                          na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Green algae"&
                                        Rubisco_25C[["group"]]=="Algae"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                          na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Green algae"&
                                        Rubisco_25C[["group"]]=="Algae"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                          na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Green algae"&
                                        Rubisco_25C[["group"]]=="Algae"&
                                        Rubisco_25C[["primary"]]=="TRUE"&
                                        ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                        Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                          na.rm=TRUE),digits=1),NA,
  25,"1B","canon","Green algae","Plants",
  "values medians of collected green algae measurements at 25C",
  "this package"),
#avg. 1B all
c("average_1B_all","Average","1B",NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["group"]]%in%c("Algae","Plants")&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["group"]]%in%c("Algae","Plants")&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["group"]]%in%c("Algae","Plants")&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["group"]]%in%c("Algae","Plants")&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,"1B","canon",NA,"Plants",
  "values medians of collected 1B measurements at 25C",
  "this package"),
#avg. 1Bc
c("average_1Bc","Average","1Bc",NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                                    Rubisco_25C[["group"]]=="Bacteria"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                                    Rubisco_25C[["group"]]=="Bacteria"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                                    Rubisco_25C[["group"]]=="Bacteria"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Cyanobacteria"&
                                    Rubisco_25C[["group"]]=="Bacteria"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,"1Bc","canon","Cyanobacteria","Bacteria",
  "values medians of collected 1Bc measurements at 25C",
  "this package"),
#avg. 1C
c("average_1C","Average","1C",NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="1C"&
                                    Rubisco_25C[["group"]]!="Metagenome"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]=="30"&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]=="30"&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),"30",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="1C"&
                                    Rubisco_25C[["group"]]!="Metagenome"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]=="30"&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]=="30"&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),"30",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="1C"&
                                    Rubisco_25C[["group"]]!="Metagenome"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="1C"&
                                    Rubisco_25C[["group"]]!="Metagenome"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,"1C",NA,NA,"Bacteria",
  "kcat, Kc medians of collected 1C measurements at 30C; Ko, S medians of collected 1C measurements at 25C",
  "this package"),
#avg. 1D
c("average_1D_Diatoms","Average","1D","Diatoms",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Diatoms"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Diatoms"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Diatoms"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Diatoms"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,"1D","canon","Diatoms","Algae",
  "values medians of collected diatom measurements at 25C",
  "this package"),
#avg. red algae
c("average_1D_Red_algae","Average","1D","Red algae",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Red algae"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Red algae"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Red algae"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Red algae"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,"1D","canon","Red algae","Algae",
  "values medians of collected red algae measurements at 25C",
  "this package"),
#avg. macroalgae
c("average_1D_Macroalgae","Average","1D","Macroalgae",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Macroalgae"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Macroalgae"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Macroalgae"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="Macroalgae"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,"1D","canon","Macroalgae","Algae",
  "values medians of collected macroalgae measurements at 25C",
  "this package"),
#avg. 1D all
c("average_1D_all","Average","1D",NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="1D"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="1D"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="1D"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="1D"&
                                    Rubisco_25C[["group"]]=="Algae"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,"1D","canon",NA,"Algae",
  "values medians of collected 1D measurements at 25C",
  "this package"),
#avg. 2 prok.
c("average_2_Prokaryote","Average","2","Prokaryote",
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="2"&
                                    Rubisco_25C[["group"]]=="Bacteria"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="2"&
                                    Rubisco_25C[["group"]]=="Bacteria"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="2"&
                                    Rubisco_25C[["group"]]=="Bacteria"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["form"]]=="2"&
                                    Rubisco_25C[["group"]]=="Bacteria"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,"2",NA,NA,"Bacteria",
  "values medians of collected prokaryote form 2 measurements at 25C",
  "this package"),
#avg. Rubisco
c("average_Rubisco","Average","Rubisco",NA,
  round(median_if_num(Rubisco_25C[!(Rubisco_25C[["form"]]%in%c("2_3","3"))&
                                    Rubisco_25C[["group"]]!="Metagenome"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["kcat_T"]]==25&!is.na(Rubisco_25C[["kcat_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["kcat_val"]],
                      na.rm=TRUE),digits=2),NA,
  round(median_if_num(Rubisco_25C[!(Rubisco_25C[["form"]]%in%c("2_3","3"))&
                                    Rubisco_25C[["group"]]!="Metagenome"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Kc_T"]]==25&!is.na(Rubisco_25C[["Kc_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                      na.rm=TRUE),digits=1),NA,
  round(median_if_num(Rubisco_25C[!(Rubisco_25C[["form"]]%in%c("2_3","3"))&
                                    Rubisco_25C[["group"]]!="Metagenome"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  round(median_if_num(Rubisco_25C[!(Rubisco_25C[["form"]]%in%c("2_3","3"))&
                                    Rubisco_25C[["group"]]!="Metagenome"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["S_T"]]==25&!is.na(Rubisco_25C[["S_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                      na.rm=TRUE),digits=1),NA,
  25,NA,NA,NA,NA,
  "values medians of collected Rubisco measurements at 25C, excluding forms 2/3 and 3",
  "this package"),
#A. vinosum composite
c("vinosum_composite","Allochromatium","vinosum",NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="vinosum_Iñiguez_2021",][["kcat_val"]],NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Allochromatium"&
                              Rubisco_25C[["species"]]=="vinosum"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["Kc_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                na.rm=TRUE),NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Allochromatium"&
                              Rubisco_25C[["species"]]=="vinosum"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["Ko_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                na.rm=TRUE),NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Allochromatium"&
                              Rubisco_25C[["species"]]=="vinosum"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["S_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                na.rm=TRUE),NA,
  25,"1Aq",NA,"Gammaproteobacteria","Bacteria",
  "kcat methodology-corrected average from Iñiguez 2021; Kc, Ko, S medians of A. vinosum studies at 25C",
  "this package"),
#A. thaliana composite
c("thaliana_composite","Arabidopsis","thaliana",NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="thaliana_Iñiguez_2021",][["kcat_val"]],NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Arabidopsis"&
                              Rubisco_25C[["species"]]=="thaliana"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["Kc_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                na.rm=TRUE),NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Arabidopsis"&
                              Rubisco_25C[["species"]]=="thaliana"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["Ko_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                na.rm=TRUE),NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Arabidopsis"&
                              Rubisco_25C[["species"]]=="thaliana"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["S_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                na.rm=TRUE),NA,
  25,"1B","canon","C3 Plants","Plants",
  "kcat methodology-corrected average from Iñiguez 2021; Kc average of Galmés 2014 and Oh 2024; Ko from Galmés 2014; S from Oh 2024",
  "this package"),
#G. max composite
c("max_composite","Glycine","max",NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="max_Iñiguez_2021",][["kcat_val"]],NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="max_Iñiguez_2021",][["Kc_val"]],NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="max_Iñiguez_2021",][["Ko_val"]],NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Glycine"&
                              Rubisco_25C[["species"]]=="max"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["S_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                na.rm=TRUE),NA,
  25,"1B","canon","C3 Plants","Plants",
  "kcat, Kc, Ko methodology-corrected average from Iñiguez 2021; S median of G. max studies at 25C",
  "this package"),
#H. annuus composite
c("annuus_composite","Helianthus","annuus",NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="annuus_Iñiguez_2021",][["kcat_val"]],NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Helianthus"&
                              Rubisco_25C[["species"]]=="annuus"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["Kc_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                na.rm=TRUE),NA,
  round(median_if_num(Rubisco_25C[Rubisco_25C[["taxonomy"]]=="C3 plants"&
                                    Rubisco_25C[["group"]]=="Plants"&
                                    Rubisco_25C[["primary"]]=="TRUE"&
                                    ((Rubisco_25C[["temp"]]==25&!is.na(Rubisco_25C[["temp"]]))|(Rubisco_25C[["Ko_T"]]==25&!is.na(Rubisco_25C[["Ko_T"]])))&
                                    Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                      na.rm=TRUE)),NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Helianthus"&
                              Rubisco_25C[["species"]]=="annuus"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["S_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                na.rm=TRUE),NA,
  25,"1B","canon","C3 Plants","Plants",
  "kcat methodology-corrected average from Iñiguez 2021; Kc from Makino 1985; Ko from median C3 plant value; S median of H. annuus studies at 25C",
  "this package"),
#O. sativa composite
c("max_composite","Oryza","sativa",NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="sativa_Iñiguez_2021",][["kcat_val"]],NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="sativa_Iñiguez_2021",][["Kc_val"]],NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="sativa_Iñiguez_2021",][["Ko_val"]],NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Oryza"&
                              Rubisco_25C[["species"]]=="sativa"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["S_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                na.rm=TRUE),NA,
  25,"1B","canon","C3 Plants","Plants",
  "kcat, Kc, Ko methodology-corrected average from Iñiguez 2021; S median of O. sativa studies at 25C",
  "this package"),
#P. vulgaris composite
c("vulgaris_composite","Phaseolus","vulgaris",NA,
  Rubisco_25C[Rubisco_25C[["identifier"]]=="vulgaris_Iñiguez_2021",][["kcat_val"]],NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Phaseolus"&
                              Rubisco_25C[["species"]]=="vulgaris"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["Kc_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["Kc_val"]],
                na.rm=TRUE),NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Phaseolus"&
                              Rubisco_25C[["species"]]=="vulgaris"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["Ko_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["Ko_val"]],
                na.rm=TRUE),NA,
  median_if_num(Rubisco_25C[Rubisco_25C[["genus"]]=="Phaseolus"&
                              Rubisco_25C[["species"]]=="vulgaris"&
                              (Rubisco_25C[["temp"]]==25|Rubisco_25C[["S_T"]]==25)&
                              Rubisco_25C[["mutant"]]=="FALSE",][["S_val"]],
                na.rm=TRUE),NA,
  25,"1B","canon","C3 Plants","Plants",
  "kcat methodology-corrected average from Iñiguez 2021; Kc, Ko, S medians of P. vulgaris studies at 25C",
  "this package")
)
Rubisco_abridged$kcat_val<-as.numeric(Rubisco_abridged$kcat_val)
Rubisco_abridged$kcat_T<-as.numeric(Rubisco_abridged$kcat_T)
Rubisco_abridged$Kc_val<-as.numeric(Rubisco_abridged$Kc_val)
Rubisco_abridged$Kc_T<-as.numeric(Rubisco_abridged$Kc_T)
Rubisco_abridged$Ko_val<-as.numeric(Rubisco_abridged$Ko_val)
Rubisco_abridged$Ko_T<-as.numeric(Rubisco_abridged$Ko_T)
Rubisco_abridged$S_val<-as.numeric(Rubisco_abridged$S_val)
Rubisco_abridged$S_T<-as.numeric(Rubisco_abridged$S_T)
Rubisco_abridged$temp<-as.numeric(Rubisco_abridged$temp)

#make temp_dep_averaged table
temp_dep_averaged <- data.frame(rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9),rep(NA,9))
colnames(temp_dep_averaged)<-colnames(temp_dep_abridged)
#avg. 1B all plants
temp_dep_averaged[1,]<-c("average_1B_Plants_dH","Average","1B","Plants",
  58.1,
  41.1,
  26.7,
  -21.5,
  "1B",NA,"Plants",
  "Ko from in vivo estimates, all others average Streptophyta from Galmés 2016",
  "this package")
#avg. 1B C3 plants, warm
temp_dep_averaged[2,]<-c("average_1B_C3_warm_dH","Average","1B","C3 plants (warm)",
  64.5,
  46.1,
  26.7,
  -22,
  "1B","C3 plants","Plants",
  "Ko from in vivo estimates, all others average C3 warm Streptophyta from Galmés 2016",
  "this package")
#avg. 1B C3 plants, cool
temp_dep_averaged[3,]<-c("average_1B_C3_cool_dH","Average","1B","C3 plants (cool)",
  55.3,
  40.5,
  26.7,
  -19.9,
  "1B","C3 plants","Plants",
  "Ko from in vivo estimates, all others average C3 cool Streptophyta from Galmés 2016",
  "this package")
#avg. 1B C4 plants
temp_dep_averaged[4,]<-c("average_1B_C4_Plants_dH","Average","1B","C4 plants",
  52.8,
  34.7,
  26.7,
  -22.3,
  "1B","C4 plants","Plants",
  "Ko from in vivo estimates, all others average C4 Streptophyta from Galmés 2016",
  "this package")
#avg. 1A (Proteobacteria)
temp_dep_averaged[5,]<-c("average_1A_Proteobacteria_dH","Average","1A","Proteobacteria",
  47.2,
  round(median_if_num(temp_dep_abridged[["Kc_dH"]],na.rm=TRUE),digits=1),
  26.7,
  round(median_if_num(temp_dep_abridged[
    temp_dep_abridged[["form"]]%in%c("1D","1C","1B","1Bc","1Ac","1A"),][["S_dH"]],
    na.rm=TRUE),digits=1),
  "1A",NA,"Bacteria",
  "Ko from in vivo estimates, kcat from mean of T. tepidum, A. vinosum, and T. endosymbiont values, Kc median of all values, S median of all form 1 values",
  "this package")
#avg. 1Ac (Cyanobacteria)
temp_dep_averaged[6,]<-c("average_1Ac_Cyanobacteria_dH","Average","1Ac","Cyanobacteria",
  40.1,
  38.8,
  26.7,
  -27.4,
  "1Ac","Cyanobacteria","Bacteria",
  "Ko from in vivo estimates, all others average Cyanobacteria from Galmés 2016",
  "this package")
#avg. 1Bc
temp_dep_averaged[7,]<-c("average_1Bc_dH","Average","1Bc",NA,
  40.1,
  38.8,
  26.7,
  -27.4,
  "1Bc","Cyanobacteria","Bacteria",
  "Ko from in vivo estimates, all others average Cyanobacteria from Galmés 2016",
  "this package")
#avg. 2
temp_dep_averaged[8,]<-c("average_2_dH","Average","2",NA,
  round(median_if_num(temp_dep_abridged[["kcat_dH"]],na.rm=TRUE),digits=1),
  round(median_if_num(temp_dep_abridged[["Kc_dH"]],na.rm=TRUE),digits=1),
  26.7,
  -18.8,
  "2",NA,"Bacteria",
  "kcat, Kc median of all values, Ko from in vivo estimates, S from R. rubrum",
  "this package")
#avg. Rubisco
temp_dep_averaged[9,]<-c("average_Rubisco_dH","Average","Rubisco",NA,
  round(median_if_num(temp_dep_abridged[["kcat_dH"]],na.rm=TRUE),digits=1),
  round(median_if_num(temp_dep_abridged[["Kc_dH"]],na.rm=TRUE),digits=1),
  26.7,
  round(median_if_num(temp_dep_abridged[["S_dH"]],na.rm=TRUE),digits=1),
  NA,NA,NA,
  "Ko from in vivo estimates, all others median of all collected studies",
  "this package")
temp_dep_averaged$kcat_dH<-as.numeric(temp_dep_averaged$kcat_dH)
temp_dep_averaged$Kc_dH<-as.numeric(temp_dep_averaged$Kc_dH)
temp_dep_averaged$Ko_dH<-as.numeric(temp_dep_averaged$Ko_dH)
temp_dep_averaged$S_dH<-as.numeric(temp_dep_averaged$S_dH)

usethis::use_data(Rubisco_aliases, overwrite=TRUE)
usethis::use_data(Rubisco_abridged, overwrite=TRUE)
usethis::use_data(Rubisco_25C, overwrite=TRUE)
usethis::use_data(temp_dep_averaged, overwrite=TRUE)
usethis::use_data(temp_dep_abridged, overwrite=TRUE)
usethis::use_data(refs_to_citation,overwrite=TRUE)
usethis::use_data(
  Rubisco_aliases, Rubisco_abridged, Rubisco_25C,
  refs_to_citation, temp_dep_averaged, temp_dep_abridged,
  internal=TRUE, overwrite=TRUE
)
