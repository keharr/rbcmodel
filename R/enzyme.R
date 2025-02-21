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
#' @param kc_val The measured value of Kc (in uM). Kc is the Michaelis constant
#'   of Rubisco for CO2, or its affinity for CO2. The temperature at which it
#'   was measured can be found in kc_T (if present) or temp.
#' @param ko_val The measured value of Ko (in uM). Ko is the Michaelis constant
#'   of Rubisco for O2, or its affinity for O2. The temperature at which it
#'   was measured can be found in ko_T (if present) or temp.
#' @param s_val The measured value of Sc/o (unitless). Sc/o is the specificity of
#'   Rubisco for CO2 versus O2. The temperature at which it was measured can be
#'   found in s_T (if present) or temp.
#' @param temp The common temperature (in celsius) at which the kinetic
#'   constants are measured. Can be overridden by the more specialized
#'   arguments kcat_T, kc_T, ko_T, and s_T. If NULL these 4 arguments are
#'   used to specify temperatures.
#' @param kcat_T The temperature (in celsius) at which kcat_val is measured.
#' @param kc_T The temperature (in celsius) at which kc_val is measured.
#' @param ko_T The temperature (in celsius) at which ko_val is measured.
#' @param s_T The temperature (in celsius) at which s_val is measured.
#' @param name An identifier ("name") of the enzyme instance.
#' @returns A new enzyme instance.
#' @export
#' @examples 
#' new_enzyme(6.6, 144, 394, 10.8, name="pro")
new_enzyme <- function(
  kcat_val, kc_val, ko_val, s_val, temp = 25,
  kcat_T = NULL, kc_T = NULL, ko_T = NULL, s_T = NULL,
  name = ""
) {

  # initialize kinetic values
  an_enzyme <- list(
    name = name,
    kcat_val = kcat_val,
    kc_val = kc_val,
    ko_val = ko_val,
    s_val = s_val
  )

  # if common temperature is supplied, associate it with
  # all kinetic variables
  if (!is.null(temp)) {
    an_enzyme$kcat_T <- temp
    an_enzyme$kc_T <- temp
    an_enzyme$ko_T <- temp
    an_enzyme$s_T <- temp
  }

  # override an associated temperature if an overriding value is supplied
  if (!is.null(kcat_T)) {
    an_enzyme$kcat_T <- kcat_T
  }
  if (!is.null(kc_T)) {
    an_enzyme$kc_T <- kc_T
  }
  if (!is.null(ko_T)) {
    an_enzyme$ko_T <- ko_T
  }
  if (!is.null(s_T)) {
    an_enzyme$s_T <- s_T
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

  if (!is.character(the_enzyme$name)) {
    stop("The name of the enzyme must be a valid string", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$kcat_val)) {
    stop("The value of kcat_val must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$kc_val)) {
    stop("The value of kc_val must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$ko_val)) {
    stop("The value of ko_val must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$s_val)) {
    stop("The value of s_val must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$kcat_T)) {
    stop("The value of kcat_T must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$kc_T)) {
    stop("The value of kc_T must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$ko_T)) {
    stop("The value of ko_T must be numeric", call. = FALSE)
  }
  if (!is.numeric(the_enzyme$s_T)) {
    stop("The value of s_T must be numeric", call. = FALSE)
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
    "  k_c = ", the_enzyme$kc_val, k_unit,
    " @ T_c = ", the_enzyme$kc_T, T_unit, "\n"
  ))
  cat(paste0(
    "  k_o = ", the_enzyme$ko_val, k_unit,
    " @ T_o = ", the_enzyme$ko_T, T_unit, "\n"
  ))
  cat(paste0(
    "  s = ", the_enzyme$s_val,
    " @ T_s = ", the_enzyme$s_T, T_unit, "\n"
  ))
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
#' @param kc_val if not NULL, the kc_val value for the new instance
#' @param ko_val if not NULL, the ko_val value for the new instance
#' @param s_val if not NULL, the s_val value for the new instance
#' @param kcat_T if not NULL, the kcat_T value for the new instance
#' @param kc_T if not NULL, the kc_T value for the new instance
#' @param ko_T if not NULL, the ko_T value for the new instance
#' @param s_T if not NULL, the s_T value for the new instance
#' @param name if not NULL, the identifier ("name") for the new instance
#' @returns a new instance of the S3 class 'enzyme'.
#' @export
#' @examples
#' pro_enzyme <- new_enzyme(6.6, 144, 394, 10.8, name="pro") # new instance
#' print(pro_enzyme) # printout enzyme info
#' pro_worse_enzyme <- modify_enzyme(pro_enzyme, kcat_val=2, kc_val=310, name="pro_worse")
#' print(pro_worse_enzyme) # printout modified enzyme info
modify_enzyme <- function(
  the_enzyme,
  kcat_val = NULL, kc_val = NULL, ko_val = NULL, s_val = NULL,
  kcat_T = NULL, kc_T = NULL, ko_T = NULL, s_T = NULL, name = NULL
) {

  # NOTE: recall that R copy on edit

  # overwrite value whenever a new one is supplied
  if (!is.null(kcat_val)) {
    the_enzyme$kcat_val <- kcat_val
  }
  if (!is.null(kc_val)) {
    the_enzyme$kc_val <- kc_val
  }
  if (!is.null(ko_val)) {
    the_enzyme$ko_val <- ko_val
  }
  if (!is.null(s_val)) {
    the_enzyme$s_val <- s_val
  }
  if (!is.null(kcat_T)) {
    the_enzyme$kcat_T <- kcat_T
  }
  if (!is.null(kc_T)) {
    the_enzyme$kc_T <- kc_T
  }
  if (!is.null(ko_T)) {
    the_enzyme$ko_T <- ko_T
  }
  if (!is.null(s_T)) {
    the_enzyme$s_T <- s_T
  }
  if (!is.null(name)) {
    the_enzyme$name <- name
  }

  # return the modified instance
  the_enzyme
}
