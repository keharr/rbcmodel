#' Rubisco alias table
#'
#' A table containing a list of some of the alternative names for species contained within the datasets. This includes both common names (i.e., "wheat") and former names that are no longer in use (i.e., "Chenopodium rubra", which has become Oxybasis rubra).
#' @format ## 'Rubisco_aliases'
#' A data frame with 79 rows and 4 columns:
#' \describe{
#'   \item{alternate_name}{Alternate, user-input name}
#'   \item{genus_database}{Genus name in database}
#'   \item{species_database}{Species name in database}
#'   \item{subspecies_database}{Subspecies name in database}
#' }
#' @source compiled by the authors
"Rubisco_aliases"

#' Abridged Rubisco kinetics data
#'
#' A subset of data from the comprehensive Rubisco kinetics table also included in the package, plus some composite data and median Rubisco kinetics as described in sections 5.1 and 5.2 of the Creating Custom Enzymes vignette.
#' @format ## 'Rubisco_abridged'
#' A data frame with 294 rows and 19 columns:
#' \describe{
#'   \item{identifier}{Unique entry ID}
#'   \item{genus}{Genus}
#'   \item{species}{Species}
#'   \item{subspecies}{Subspecies, variant, or cultivar}
#'   \item{kcat_val}{Value of kcat (/sec)}
#'   \item{kcat_T}{kcat measurement temperature (°C)}
#'   \item{Kc_val}{Value of Kc (μM)}
#'   \item{Kc_T}{kcat measurement temperature (°C)}
#'   \item{Ko_val}{Value of Ko (μM)}
#'   \item{Ko_T}{kcat measurement temperature (°C)}
#'   \item{S_val}{Value of Sc/o}
#'   \item{S_T}{Specificity measurement temperature (°C)}
#'   \item{temp}{Kinetics measurement temperature (°C)}
#'   \item{form}{Form of Rubisco}
#'   \item{PGS}{Recommended phosphoglycolate salvage pathway}
#'   \item{taxonomy}{Taxonomy}
#'   \item{group}{Broader phylogenetic group}
#'   \item{note}{Note}
#'   \item{short_ref}{Short reference}
#' }
#' @source compiled by the authors from a literature search
"Rubisco_abridged"

#' Comprehensive Rubisco kinetics data
#'
#' A dataset of Rubisco kinetics data compiled from the literature. Includes all four major kinetic parameters (kcat, Kc, Ko, and Sc/o), along with ancillary information about the measurements and the studies.
#' @format ## 'Rubisco_25C'
#' A data frame with 1454 rows and 30 columns:
#' \describe{
#'   \item{identifier}{Unique entry ID}
#'   \item{genus}{Genus}
#'   \item{species}{Species}
#'   \item{subspecies}{Subspecies, variant, or cultivar}
#'   \item{mutant}{Species}
#'   \item{mutant_details}{Species}
#'   \item{primary}{Species}
#'   \item{heterologous_expression}{Species}
#'   \item{kcat_val}{Value of kcat (/sec)}
#'   \item{kcat_T}{kcat measurement temperature (°C)}
#'   \item{kcat_pH}{kcat measurement pH}
#'   \item{Kc_val}{Value of Kc (μM)}
#'   \item{Kc_T}{Kc measurement temperature (°C)}
#'   \item{Kc_pH}{Kc measurement pH}
#'   \item{Ko_val}{Value of Ko (μM)}
#'   \item{Ko_T}{Ko measurement temperature (°C)}
#'   \item{Ko_pH}{Ko measurement pH}
#'   \item{S_val}{Value of Sc/o}
#'   \item{S_T}{Specificity measurement temperature (°C)}
#'   \item{S_pH}{Specificity measurement pH}
#'   \item{temp}{Kinetics measurement temperature (°C)}
#'   \item{pH}{Kinetics measurement pH}
#'   \item{pKa}{pKa used to determine CO2 concentration for Kc, Ko, and specificity calculations}
#'   \item{form}{Form of Rubisco}
#'   \item{PGS}{Recommended phosphoglycolate salvage pathway}
#'   \item{taxonomy}{Taxonomy}
#'   \item{group}{Broader phylogenetic group}
#'   \item{note}{Note}
#'   \item{short_ref}{Short reference}
#'   \item{Year}{Study year}
#' }
#' @source compiled by the authors from a literature search
"Rubisco_25C"

#' Averaged Delta H temperature scaling data
#'
#' A dataset of average Rubisco temperature scaling data per taxonomic group and form, as described in Section 5.3 of the "Designing Custom Enzymes" vignette. 
#' @format ## 'temp_dep_averaged'
#' A data frame with 9 rows and 13 columns:
#' \describe{
#'   \item{identifier}{Unique entry ID}
#'   \item{genus}{Genus}
#'   \item{species}{Species}
#'   \item{subspecies}{Subspecies, variant, or cultivar}
#'   \item{kcat_dH}{kcat temperature dependence parameter (kJ/mol)}
#'   \item{Kc_dH}{Kc temperature dependence parameter (kJ/mol)}
#'   \item{Ko_dH}{Ko temperature dependence parameter (kJ/mol)}
#'   \item{S_dH}{S temperature dependence parameter (kJ/mol)}
#'   \item{form}{Form of Rubisco}
#'   \item{taxonomy}{Taxonomy}
#'   \item{group}{Broader phylogenetic group}
#'   \item{note}{Note}
#'   \item{short_ref}{Short reference}
#' }
#' @source compiled by the authors, mostly from Galmés et al. 2016
"temp_dep_averaged"

#' Abridged Delta H temperature scaling data
#'
#' A dataset of Rubisco temperature dependence data compiled from a literature search.
#' @format ## 'temp_dep_abridged'
#' A data frame with 182 rows and 13 columns:
#' \describe{
#'   \item{identifier}{Unique entry ID}
#'   \item{genus}{Genus}
#'   \item{species}{Species}
#'   \item{subspecies}{Subspecies, variant, or cultivar}
#'   \item{kcat_dH}{kcat temperature dependence parameter (kJ/mol)}
#'   \item{Kc_dH}{Kc temperature dependence parameter (kJ/mol)}
#'   \item{Ko_dH}{Ko temperature dependence parameter (kJ/mol)}
#'   \item{S_dH}{S temperature dependence parameter (kJ/mol)}
#'   \item{form}{Form of Rubisco}
#'   \item{taxonomy}{Taxonomy}
#'   \item{group}{Broader phylogenetic group}
#'   \item{note}{Note}
#'   \item{short_ref}{Short reference}
#' }
#' @source compiled by the authors from a literature search
"temp_dep_abridged"

#' References table
#'
#' A table containing a list of the citation information and pmid/doi numbers to help with citing data from the package.
#' @format ## 'refs_to_citation'
#' A data frame with 150 rows and 3 columns:
#' \describe{
#'   \item{short_ref}{Short reference}
#'   \item{pmid_or_doi}{The pmid number or the doi}
#'   \item{citation}{Full citation}
#' }
#' @source compiled by the authors
"refs_to_citation"