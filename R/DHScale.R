#' Constructor for the S3 class 'DHScale'.
#'
#' This class collects temperature scaling constants for each of the four
#' kinetic parameters. These scaling constants are also called the activation
#' energy of that kinetic parameter, and are typically denoted with ΔH. In
#' combination with an enzyme-class object, a DHScale-class object can be used
#' to create a function that models an enzyme's carbon output with regards to
#' temperature and CO2/O2 concentration.
#'
#' @param kcat_dH The temperature scaling constant of kcat for the carboxylation
#'   reaction, also known as the activation energy (in kJ/mol).
#' @param Kc_dH The temperature scaling constant of Kc (in kJ/mol).
#' @param Ko_dH The temperature scaling constant of Ko (in kJ/mol).
#' @param S_dH The temperature scaling constant of Sc/o (in kJ/mol).
#' @param name An identifier ("name") of the scale instance.
#' @returns A new 'DHScale' instance.
#' @export
#' @examples
#' new_DHScale(40.1, 38.8, 26.7, -27.4, name = "Form Ia, cyanobacteria")
new_DHScale <- function(kcat_dH, Kc_dH, Ko_dH, S_dH, name = "") {

  # initialize kinetic values
  a_DHScale <- list(
    name = name,
    kcat_dH = kcat_dH,
    Kc_dH = Kc_dH,
    Ko_dH = Ko_dH,
    S_dH = S_dH
  )

  # add the S3 class "DHScale" to the object
  class(a_DHScale) <- append("DHScale", class(a_DHScale))

  # return the list with class updated
  a_DHScale
}

#' Validator for the S3 class 'DHScale'
#'
#' Throw error if any scaling constants are non-numeric, or if name
#' is not a character string.
#'
#' @param the_DHScale the 'DHScale' instance to be checked.
#' @returns the supplied DHScale instance, if all checks pass.
#' @export
#' @examples
#' bad_DHScale <- new_DHScale(NA, 41, 26, -13.8, name="bad") # bad instance
#' try(check_DHScale(bad_DHScale)) # produces error
#'
check_DHScale <- function(the_DHScale) {
  if (!is.character(the_DHScale$name)) {
    stop("The name of the Delta H scaling must be a valid string", call. = FALSE)
  }
  if (!is.numeric(the_DHScale$kcat_dH)) {
    stop("The value of kcat_dH must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_DHScale$Kc_dH)) {
    stop("The value of Kc_dH must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_DHScale$Ko_dH)) {
    stop("The value of Ko_dH must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_DHScale$S_dH)) {
    stop("The value of S_dH must be numeric", call. = FALSE)
  }

  # return the DHScale if all tests are passed
  the_DHScale
}

#' Customized print for S3 class 'DHScale'
#'
#' @param x the 'DHScale' instance to be printed.
#' @param unicode whether to print using unicode characters.
#' @param ... the remaining arguments are ignored.
#' @returns No explicit return. Information of the 'DHScale' instance is printed
#'   to stdout.
#' @export
#' @examples
#' ia_cyano_DHScale <- new_DHScale(40.1, 38.8, 26.7, -27.4, name = "Form Ia, cyanobacteria")
#' print(ia_cyano_DHScale) # printout DHScale info
#'
print.DHScale <- function(x, unicode = TRUE, ...) {

  the_DHScale <- x

  if (unicode) {
    DH_char <- "\u0394H"
    DH_char2 <- "\u0394H"
  } else {
    DH_char <- "Delta H"
    DH_char2 <- "{Delta H}"
  }
  cat(paste0(DH_char, ' scaling "', the_DHScale$name, '":\n'))
  cat(paste0("  ", DH_char2, "_kcat = ", the_DHScale$kcat_dH, " kJ/mol\n"))
  cat(paste0("  ", DH_char2, "_Kc = ", the_DHScale$Kc_dH, " kJ/mol\n"))
  cat(paste0("  ", DH_char2, "_Ko = ", the_DHScale$Ko_dH, " kJ/mol\n"))
  cat(paste0("  ", DH_char2, "_S = ", the_DHScale$S_dH, " kJ/mol\n"))
}

#' Create a new instance of S3 class 'DHScale', starting from a previous
#' instance
#'
#' If any argument (other than `the_DHScale`, which is required) is `NULL`,
#' the corresponding instance attribute will copy its value from the previous
#' instance. Otherwise, the input value will become the value of the
#' corresponding attribute in the new instance. For detailed explanation of
#' each 'DHScale' attribute, see the documentation of `new_DHScale()`
#'
#' @param the_DHScale the previous 'DHScale' instance to base the new
#'   instance on.
#' @param kcat_dH if not NULL, the kcat_dH value for the new instance
#' @param Kc_dH if not NULL, the Kc_dH value for the new instance
#' @param Ko_dH if not NULL, the Ko_dH value for the new instance
#' @param S_dH if not NULL, the S_dH value for the new instance
#' @param name if not NULL, the identifier ("name") for the new instance
#' @returns a new instance of the S3 class 'DHScale'.
#' @export
#' @examples
#' bacIA_DHScale <- new_DHScale(47.2,40.8,26.7,-21.8,name="bacIA") # new instance
#' print(bacIA_DHScale) # printout DHScale info
#' tmrIA_DHScale <- modify_DHScale(bacIA_DHScale,kcat_dH=32,name="tmr_IA") # modify instance
#' print(tmrIA_DHScale) # printout modified DHScale info
#'
modify_DHScale <- function(
  the_DHScale, kcat_dH = NULL, Kc_dH = NULL, Ko_dH = NULL, S_dH = NULL, 
  name = NULL
) {

  # NOTE: recall that R copy on edit

  # overwrite value whenever a new one is supplied
  if (!is.null(kcat_dH)) {
    the_DHScale$kcat_dH <- kcat_dH
  }
  if (!is.null(Kc_dH)) {
    the_DHScale$Kc_dH <- Kc_dH
  }
  if (!is.null(Ko_dH)) {
    the_DHScale$Ko_dH <- Ko_dH
  }
  if (!is.null(S_dH)) {
    the_DHScale$S_dH <- S_dH
  }
  if (!is.null(name)) {
    the_DHScale$name <- name
  }
  
  # return the modified instance
  the_DHScale
}

#' Create an DHScale instance by looking up the relevant data in database
#'
#' @param id_name the name of identifier used to pull out a single record
#'   out of the database
#' @param id_col the name of the column from which the id_name will be searched
#'   for within the database
#' @param scale_name the name of the DHScale instance. If NULL the value is
#'   defaulted to id_name
#' @param data the database to search from. Can be "averaged", NULL, or a 
#'   data.frame. If NULL or "averaged", the averaged table is searched.
#'   NOTE: if a custom data.frame is supplied, it is assumed to have columns
#'   named "kcat_dH", "Kc_dH", "Ko_dH", and "S_dH".
#' @returns a new DHScale instance
#' @export
#' @examples
#' cyano <- DHScale("cyano_1Ac") # retrieved from abridged database
DHScale <- function(id_name, id_col="identifier", scale_name=NULL, data=NULL){

  if (is.null(scale_name)){ scale_name <- id_name }

  # translate name to the actual database
  if (is.character(data)){
    if (data == "averaged"){
      data <- temp_dep_averaged
    } else {
      stop(paste0("Unknown database string '", data, "'"), call. = FALSE)
    }
  }

  # NULL is (currently) interpreted as the same as "average"
  if (is.null(data)){
    data <- temp_dep_averaged
  }

  if (!is.null(id_col)){ # now handle the non-NULL case

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

  return(new_DHScale(
    data_sub$kcat_dH, data_sub$Kc_dH, data_sub$Ko_dH, data_sub$S_dH,
    name = scale_name
  ))

}
