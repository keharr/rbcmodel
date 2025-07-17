#' Constructor for the S3 class 'enzyme'.
#'
#' The class 'enzyme' is designed to encapsulate the information necessary to
#' describe a particular measured enzyme's response to CO2, O2, and temperature.
#' All of Rubisco's key kinetic parameters are used as inputs, along with the
#' temperature at which they were measured. In combination with a DHScale-class
#' object, an enzyme-class object can be used to create a function that models
#' a Rubisco enzyme's carbon output with regards to temperature and CO2/O2
#' concentration.
#'
#' @param kcat_val The measured value of kcat for the Rubisco carboxylase
#'   reaction (in s^-1). kcat represents the number of reactions per second a
#'   single active site can perform. The temperature at which it was measured
#'   can be found in kcat_T (if present) or temp.
#' @param Kc_val The measured value of Kc (in uM). Kc is the Michaelis constant
#'   of Rubisco for CO2, or its affinity for CO2. The temperature at which it
#'   was measured can be found in Kc_T (if present) or temp.
#' @param Ko_val The measured value of Ko (in uM). Ko is the Michaelis constant
#'   of Rubisco for O2, or its affinity for O2. The temperature at which it
#'   was measured can be found in Ko_T (if present) or temp.
#' @param S_val The measured value of Sc/o (unitless). Sc/o is the specificity of
#'   Rubisco for CO2 versus O2. The temperature at which it was measured can be
#'   found in S_T (if present) or temp.
#' @param temp The common temperature (in celsius) at which the kinetic
#'   constants are measured. Can be overridden by the more specialized
#'   arguments kcat_T, Kc_T, Ko_T, and S_T. If NULL these 4 arguments are
#'   used to specify temperatures.
#' @param kcat_T The temperature (in celsius) at which kcat_val is measured.
#' @param Kc_T The temperature (in celsius) at which Kc_val is measured.
#' @param Ko_T The temperature (in celsius) at which Ko_val is measured.
#' @param S_T The temperature (in celsius) at which S_val is measured.
#' @param PGS The default phosphoglycolate salvage (PGS) pathway of the enzyme.
#'   Can be NA.
#' @param name An identifier ("name") of the enzyme instance.
#' @returns A new enzyme instance.
#' @export
#' @examples
#' new_enzyme(6.6, 144, 394, 10.8, name="pro")
new_enzyme <- function(
  kcat_val, Kc_val, Ko_val, S_val, temp = 25,
  kcat_T = NULL, Kc_T = NULL, Ko_T = NULL, S_T = NULL,
  PGS=NA, name = ""
) {

  # initialize kinetic values
  an_enzyme <- list(
    name = name,
    kcat_val = kcat_val,
    Kc_val = Kc_val,
    Ko_val = Ko_val,
    S_val = S_val,
    PGS = tolower(PGS)
  )

  # if common temperature is supplied, associate it with
  # all kinetic variables
  if (!is.null(temp)) {
    an_enzyme$kcat_T <- temp
    an_enzyme$Kc_T <- temp
    an_enzyme$Ko_T <- temp
    an_enzyme$S_T <- temp
  }

  # override an associated temperature if an overriding value is supplied
  if (!is.null(kcat_T)) {
    an_enzyme$kcat_T <- kcat_T
  }
  if (!is.null(Kc_T)) {
    an_enzyme$Kc_T <- Kc_T
  }
  if (!is.null(Ko_T)) {
    an_enzyme$Ko_T <- Ko_T
  }
  if (!is.null(S_T)) {
    an_enzyme$S_T <- S_T
  }

  # add the S3 class "enzyme" to the object
  class(an_enzyme) <- append("enzyme", class(an_enzyme))

  # return the list with class updated
  an_enzyme
}

#' Validator for the S3 class 'enzyme'
#'
#' Throw error if any kinetic values or temperature are non-numeric, or if name
#' is not a character string.
#'
#' @param the_enzyme the 'enzyme' instance to be checked.
#' @returns the supplied enzyme instance, if all checks pass.
#' @export
#' @examples
#' bad_enzyme <- new_enzyme(NA, 144, 394, 10.8, name="bad") # bad instance
#' try(check_enzyme(bad_enzyme)) # produces error
check_enzyme <- function(the_enzyme) {

  PGS_allowed = c("gross", "canon", "alt", "diatom")

  if (!is.character(the_enzyme$name)) {
    stop("The name of the enzyme must be a valid string", call. = FALSE)
  }
  if (!((the_enzyme$PGS %in% PGS_allowed) || is.na(the_enzyme$PGS))){
    stop(
      paste0(
        'PGS must be NA or one of ', 
        paste(PGS_allowed, collapse=", ")
      ), call. = FALSE
    )
  }
  if (!is.numeric(the_enzyme$kcat_val)) {
    stop("The value of kcat_val must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$Kc_val)) {
    stop("The value of Kc_val must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$Ko_val)) {
    stop("The value of Ko_val must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$S_val)) {
    stop("The value of S_val must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$kcat_T)) {
    stop("The value of kcat_T must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$Kc_T)) {
    stop("The value of Kc_T must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$Ko_T)) {
    stop("The value of Ko_T must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$S_T)) {
    stop("The value of S_T must be numeric", call. = FALSE)
  }

  # if pass, the object itself is returned
  the_enzyme
}

#' Customized print for S3 class 'enzyme'
#'
#' @param x the 'enzyme' instance to be printed.
#' @param unicode whether to print using unicode characters.
#' @param ... the remaining arguments are ignored.
#' @returns No explicit return. Information of the enzyme instance is printed
#'   to stdout.
#' @export
#' @examples
#' pro_enzyme <- new_enzyme(6.6, 144, 394, 10.8, name="pro") # new instance
#' print(pro_enzyme) # print out enzyme info
print.enzyme <- function(x, unicode = TRUE, ...) {

  the_enzyme <- x

  if (unicode) {
    kcat_unit <- " s\u207B\u00B9"
    k_unit <- " \u03BCM"
    T_unit <- "\u00B0C"

  } else {
    kcat_unit <- " s^{-1}"
    k_unit <- " uM"
    T_unit <- " deg C"
  }

  cat(paste0('Enzyme "', the_enzyme$name, '":\n'))
  cat(paste0(
    "  k_cat = ", the_enzyme$kcat_val, kcat_unit,
    " @ T_cat = ", the_enzyme$kcat_T, T_unit, "\n"
  ))
  cat(paste0(
    "  K_c = ", the_enzyme$Kc_val, k_unit,
    " @ T_c = ", the_enzyme$Kc_T, T_unit, "\n"
  ))
  cat(paste0(
    "  K_o = ", the_enzyme$Ko_val, k_unit,
    " @ T_o = ", the_enzyme$Ko_T, T_unit, "\n"
  ))
  cat(paste0(
    "  S = ", the_enzyme$S_val,
    " @ T_S = ", the_enzyme$S_T, T_unit, "\n"
  ))
  if (is.na(the_enzyme$PGS)){
    cat(paste0('  PGS = NA\n'))
  } else {
    cat(paste0('  PGS = "', the_enzyme$PGS, '"\n'))
  }
}

#' Create a new instance of S3 class 'enzyme' starting from a previous instance
#'
#' If any argument (other than `the_enzyme`, which is required) is `NULL`,
#' the corresponding instance attribute will copy its value from the previous
#' instance. Otherwise, the input value will become the value of the
#' corresponding attribute in the new instance. For detailed explanation of
#' each 'enzyme' attribute, see the documentation of [new_enzyme()]
#'
#' @param the_enzyme the previous 'enzyme' instance to base the new instance on.
#' @param kcat_val if not NULL, the kcat_val value for the new instance
#' @param Kc_val if not NULL, the Kc_val value for the new instance
#' @param Ko_val if not NULL, the Ko_val value for the new instance
#' @param S_val if not NULL, the S_val value for the new instance
#' @param kcat_T if not NULL, the kcat_T value for the new instance
#' @param Kc_T if not NULL, the Kc_T value for the new instance
#' @param Ko_T if not NULL, the Ko_T value for the new instance
#' @param S_T if not NULL, the S_T value for the new instance
#' @param PGS if not NULL, the default phosphoglycolate salvage (PGS) pathway
#'   for the new instance
#' @param name if not NULL, the identifier ("name") for the new instance
#' @returns a new instance of the S3 class 'enzyme'.
#' @export
#' @examples
#' pro_enzyme <- new_enzyme(6.6, 144, 394, 10.8, name="pro") # new instance
#' print(pro_enzyme) # printout enzyme info
#' pro_worse_enzyme <- modify_enzyme(pro_enzyme, kcat_val=2, Kc_val=310, name="pro_worse")
#' print(pro_worse_enzyme) # printout modified enzyme info
modify_enzyme <- function(
  the_enzyme,
  kcat_val = NULL, Kc_val = NULL, Ko_val = NULL, S_val = NULL,
  kcat_T = NULL, Kc_T = NULL, Ko_T = NULL, S_T = NULL, 
  PGS = NULL, name = NULL
) {

  # NOTE: recall that R copy on edit

  # overwrite value whenever a new one is supplied
  if (!is.null(kcat_val)) {
    the_enzyme$kcat_val <- kcat_val
  }
  if (!is.null(Kc_val)) {
    the_enzyme$Kc_val <- Kc_val
  }
  if (!is.null(Ko_val)) {
    the_enzyme$Ko_val <- Ko_val
  }
  if (!is.null(S_val)) {
    the_enzyme$S_val <- S_val
  }
  if (!is.null(kcat_T)) {
    the_enzyme$kcat_T <- kcat_T
  }
  if (!is.null(Kc_T)) {
    the_enzyme$Kc_T <- Kc_T
  }
  if (!is.null(Ko_T)) {
    the_enzyme$Ko_T <- Ko_T
  }
  if (!is.null(S_T)) {
    the_enzyme$S_T <- S_T
  }
  if (!is.null(PGS)) {
    the_enzyme$PGS <- PGS
  }
  if (!is.null(name)) {
    the_enzyme$name <- name
  }

  # return the modified instance
  the_enzyme
}

#' Create an Enzyme instance by looking up the relevant data in database
#'
#' @param id_name the name of identifier used to pull out a single record
#'   out of the database
#' @param id_col the name of the column from which the id_name will be searched
#'   for within the database
#' @param enzyme_name the name of the enzyme instance. If NULL the value is
#'   defaulted to id_name
#' @param data the database to search from. Can be "abridged", "comprehensive",
#'   NULL, or a data.frame. If NULL, the abridged table is first searched and
#'   if no entry is found the comprehensive table is searched.
#'   NOTE: if a custom data.frame is supplied, it is assumed to have columns
#'   named "kcat_val", "Kc_val", "Ko_val", and "S_val". In addition, it may
#'   have optional columns "temp", "kcat_T", "Kc_T", "Ko_T", "S_T", and "PGS"
#' @returns a new Enzyme instance
#' @export
#' @examples
#' biuncialis <- Enzyme("biuncialis_prins_2016") # retrieved from abridged database
#' # specify enzyme using species name
#' vavilovii <- Enzyme("vavilovii", "species", data="comprehensive")
Enzyme <- function(id_name, id_col="identifier", enzyme_name=NULL, data=NULL){

  if (is.null(enzyme_name)){ enzyme_name = id_name }

  # translate name to the actual database
  if (is.character(data)){
    if (data == "abridged"){
      data <- Rubisco_abridged
    } else if (data=="comprehensive"){
      data <- Rubisco_25C
    }
  }

  # work through abridged then Rubisco_kinetics if data=NULL
  if (is.null(data)){

    data_sub <- Rubisco_abridged

    if (!is.null(id_col)){
      data_sub <- data_sub[
        !is.na(data_sub[[id_col]]) & data_sub[[id_col]] == id_name
        , ]
    }

    if (nrow(data_sub) == 0){

      data_sub <- Rubisco_25C

      if (!is.null(id_col)){
        data_sub <- data_sub[
          !is.na(data_sub[[id_col]]) & data_sub[[id_col]] == id_name
          , ]
      }
    }

  } else if (!is.null(id_col)){ # now handle the non-NULL case
    data_sub <- data[
      !is.na(data[[id_col]]) & data[[id_col]] == id_name
      , ]
  } else {
    data_sub <- data
  }

  if (nrow(data_sub) == 0){
    stop("No entry found. Abort.")
  } else if (nrow(data_sub) > 1) {
    print(data_sub)
    stop("Multiple entries found. Abort.")
  }

  cols <- colnames(data_sub)

  if ("kcat_T" %in% cols){ kcat_T = data_sub$kcat_T } else { kcat_T = NULL}
  if ("Kc_T" %in% cols){ Kc_T = data_sub$Kc_T } else { Kc_T = NULL }
  if ("Ko_T" %in% cols){ Ko_T = data_sub$Ko_T } else { Ko_T = NULL }
  if ("S_T" %in% cols){ S_T = data_sub$S_T } else { S_T = NULL }
  if ("PGS" %in% cols){ PGS = data_sub$PGS } else { PGS = NA }

  if (is.na(kcat_T)) {kcat_T = NULL}
  if (is.na(Kc_T)) {Kc_T = NULL}
  if (is.na(Ko_T)) {Ko_T = NULL}
  if (is.na(S_T)) {S_T = NULL}

  if ("temp" %in% cols){
    return(new_enzyme(
      data_sub$kcat_val, data_sub$Kc_val, data_sub$Ko_val, data_sub$S_val,
      data_sub$temp,
      kcat_T = kcat_T, Kc_T = Kc_T, Ko_T = Ko_T, S_T = S_T,
      PGS = PGS, name = enzyme_name
    ))
  } else {
    return(new_enzyme(
      data_sub$kcat_val, data_sub$Kc_val, data_sub$Ko_val, data_sub$S_val,
      kcat_T = kcat_T, Kc_T = Kc_T, Ko_T = Ko_T, S_T = S_T,
      PGS = PGS, name = enzyme_name
    ))
  }
}

#' Search for data of Rubisco enzyme alias from the database
#'
#' Data is pulled either from the abridged table or the comprehensive table,
#' both of which are included in this package
#'
#' @param string a string to search for in the database
#' @param data the source data to search from. Can be "abridged", "comprehensive",
#'   or NULL. If NULL, the search will proceed from abridged to comprehensive
#'   until a match is found
#' @param match whether to require complete match. Can be "complete" or "partial"
#' @returns a dataframe containing matching entries.
search_alias <- function(string, data=NULL, match="complete"){

  # search the alias table
  if (tolower(match)=="complete"){
    lookup <- Rubisco_aliases[
      !is.na(Rubisco_aliases[["alternate_name"]]) &
        (tolower(Rubisco_aliases[["alternate_name"]]) == tolower(string))
      , ]
  } else {
    lookup <- Rubisco_aliases[
      !is.na(Rubisco_aliases[["alternate_name"]]) &
        grepl(tolower(string), tolower(Rubisco_aliases[["alternate_name"]]), fixed=TRUE)
      , ]
  }

  if (nrow(lookup) == 0){
    # warning("No matching alias found")
    return(NULL)
  }

  message(paste0("Matched aliases: ", paste(lookup$alternate_name, collapse=", ")))

  if (is.null(data) || tolower(data)=="abridged"){

    out <- data.frame()

    for (i in seq(1, nrow(lookup))){
      src <- Rubisco_abridged
      entry = lookup[i, ]
      src <- src[
        !is.na(src[["genus"]]) &
          (src[["genus"]] == entry$genus_database)
      , ]
      if (!is.na(entry$species_database)){
        src <- src[
          !is.na(src[["species"]]) &
            (src[["species"]] == entry$species_database)
        , ]
      }
      if (!is.na(entry$subspecies_database)){
        src <- src[
          !is.na(src[["subspecies"]]) &
            (src[["subspecies"]] == entry$subspecies_database)
        , ]
      }
      out <- rbind(out, src)
    }

    if (nrow(out) > 0){
      message("Data found in abridged table")
      return(out)
    }

  }

  if (is.null(data) || tolower(data)=="comprehensive"){

    out <- data.frame()

    for (i in seq(1, nrow(lookup))){
      src <- Rubisco_25C
      entry = lookup[i, ]
      src <- src[
        !is.na(src[["genus"]]) &
          (src[["genus"]] == entry$genus_database)
      , ]
      if (!is.na(entry$species_database)){
        src <- src[
          !is.na(src[["species"]]) &
            (src[["species"]] == entry$species_database)
        , ]
      }
      if (!is.na(entry$subspecies_database)){
        src <- src[
          !is.na(src[["subspecies"]]) &
            (src[["subspecies"]] == entry$subspecies_database)
        , ]
      }
      out <- rbind(out, src)
  }

    if (nrow(out) > 0){
      message("Data found in comprehensive table")
      return(out)
    }

  }
  
  # warning("No entry found")
  return(NULL)

}


#' Search for data of Rubisco enzyme from the database
#'
#' Data is pulled either from the abridged table or the comprehensive table,
#' both of which are included in this package
#'
#' @param string a string to search for in the database
#' @param level the taxonomic level to search. Can be "alias", "genus", "species",
#'   or NULL. If NULL, the search will proceed from alias to genus to species
#'   until a match is found
#' @param data the source data to search from. Can be "abridged", "comprehensive",
#'   or NULL. If NULL, the search will proceed from abridged to comprehensive
#'   until a match is found
#' @param match whether to require complete match. Can be "complete" or "partial"
#' @returns a tibble containing matching entries.
#' @export
#' @examples
#' Aegilops <- search_enzyme("Aegilops", level="genus") # complete genus search
#' print(Aegilops) # printout search results
search_enzyme <- function(string, level=NULL, data=NULL, match="complete"){

  out <- data.frame()

  # First search for alias in abridged table
  if (is.null(level) || tolower(level) == "alias"){
    result <- search_alias(string, data, match)
    if (!is.null(result)){ return(result) }
  }

  if (is.null(level) || tolower(level) == "genus"){

    if (is.null(data) || tolower(data) == "abridged"){

      # search the abridged table
      if (tolower(match)=="complete"){
        out <- Rubisco_abridged[
          !is.na(Rubisco_abridged[["genus"]]) &
            (Rubisco_abridged[["genus"]] == string)
          , ]
      } else {
        out <- Rubisco_abridged[
          !is.na(Rubisco_abridged[["genus"]]) &
            grepl(string, Rubisco_abridged[["genus"]], fixed=TRUE)
          , ]
      }

      # return if there's any match
      if (nrow(out) > 0) {
        message("Matched genus in abridged table")
        return(out)
      }
    }

    if (is.null(data) || tolower(data) == "comprehensive"){
      # search the full table
      if (tolower(match)=="complete"){
        out <- Rubisco_25C[
          !is.na(Rubisco_25C[["genus"]]) &
            (Rubisco_25C[["genus"]] == string)
          , ]
      } else {
        out <- Rubisco_25C[
          !is.na(Rubisco_25C[["genus"]]) &
            grepl(string, Rubisco_25C[["genus"]], fixed=TRUE)
          , ]
      }
      if (nrow(out) > 0) {
        message("Matched genus in comprehensive table")
        return(out)
      }
    }

  }

  if (is.null(level) || tolower(level) == "species"){

    if (is.null(data) || data == "abridged"){

      # search the abridged table
      if (tolower(match)=="complete"){
        out <- Rubisco_abridged[
          !is.na(Rubisco_abridged[["species"]]) &
            (Rubisco_abridged[["species"]] == string)
          , ]
      } else {
        out <- Rubisco_abridged[
          !is.na(Rubisco_abridged[["species"]]) &
            grepl(string, Rubisco_abridged[["species"]], fixed=TRUE)
          , ]
      }

      # return if there's any match
      if (nrow(out) > 0) {
        message("Matched species in abridged table")
        return(out)
      }
    }

    if (is.null(data) || tolower(data) == "comprehensive"){
      # search the full table
      if (tolower(match)=="complete"){
        out <- Rubisco_25C[
          !is.na(Rubisco_25C[["species"]]) &
            (Rubisco_25C[["species"]] == string)
          , ]
      } else {
        out <- Rubisco_25C[
          !is.na(Rubisco_25C[["species"]]) &
            grepl(string, Rubisco_25C[["species"]], fixed=TRUE)
          , ]
      }

      if (nrow(out) > 0) {
        message("Matched species in comprehensive table")
        return(out)
      }
    }
  }

  warning("No entry found")
  return(NULL)

}
