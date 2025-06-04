#' Function factory for calculating temperture-dependent kinetics
#'
#' Calculates the non-supplied constant (c) for an Arrhenius-type temperature
#' scaling function (as seen in Galmés et al. 2016). Uses measured values and
#' the temperature they were measured at, along with a supplied temperature
#' scaling constant (also called the activation energy, ΔH (in kJ/mol)).
#'
#' @encoding UTF-8
#' @param std Measured value
#' @param delta_H ΔH value for this kinetic parameter for this organism type
#'   (in kJ/mol).
#' @param temp Temperature at which param 'std' was measured. Defaults to 25°C.
#' @returns A new function that scales the supplied kinetic parameter with
#'   temperature (in celsius).
#' @export
#' @examples kcat_proteo_Ia <- kinetics_T_dep(15, 44.7) # create new function
#' kcat_proteo_Ia(30) # evaluate the kinetic parameter at a different temperature
#'
kinetics_T_dep <- function(std, delta_H, temp = 25) {
  c <- log(std) + (delta_H / (.008314 * (temp + 273)))
  function(temp) {
    exp(c - delta_H / (.008314 * (temp + 273)))
  }
}

#' Function factory to model an enzyme's carbon output
#'
#' Function factory to model an enzyme's carbon output versus temperature
#' and CO2/O2 concentration. Uses a 'DHScale' object to scale each kinetic
#' parameter from an 'enzyme' object in regards to temperature. Based on the
#' stoichiometry of known phosphoglycolate salvage pathways, it also takes
#' into account the carbon lost due to the Rubisco oxygenation reaction based
#' on user input.
#'
#' @param enzyme An 'enzyme' object. (See enzyme class for more info.)
#' @param DHScale A 'DHScale' object. (See DHScale class for more info.)
#' @param pathway One of 'gross', 'canon', 'diatom', or 'alt'. This option allows you to
#'   pick a stoichiometry for the phosphoglycolate salvage (PGS) pathway. If you
#'   do not want to incorporate PGS, use the 'gross' option, while 'canon'
#'   applies the C2 cycle/glycerate pathway stoichiometry (1 CO2 for
#'   every 2 2-phosphoglycolate created). 'alt' corresponds to the oxalyl-CoA or
#'   malate cycle pathways (2 CO2 for every 2-phosphoglycolate created).
#'   'diatom' corresponds to the proposed diatom-specific pathways, which do
#'   not return 2-PG to the Calvin Cycle, but incorporate it directly into
#'   biomass with the exception of 1 CO2 for every 2-phosphoglycolate created. 
#' @returns A new function that models carbon output with three variables: CO2
#'   (in uM), O2 (in uM), and temperature (in celsius).
#' @export
#' @examples
#' pro_enzyme <- new_enzyme(6.6, 144, 394, 10.8, name="pro")
#' bacIA_DHScale <- new_DHScale(47.2,40.8,26.7,-21.8,name="bacIA")
#' pro_model <- CO2_dependence(pro_enzyme, bacIA_DHScale, "canon") # sets up model
#' pro_model(126,54,15) # prints carbon fixation at 15 deg C, 126 uM CO2, 54 uM O2
CO2_dependence <- function(enzyme, DHScale, pathway) {

  # check validity of pathway argument
  if (is.null(pathway) || !pathway %in% c("gross", "canon", "diatom", "alt")) {
    stop("'pathway' argument must be one of: 'gross','canon', 'diatom', or 'alt'")
  }

  # validate input
  check_enzyme(enzyme)
  check_DHScale(DHScale)

  # unpack all data from enzyme and DHScale
  kcat_val <- enzyme$kcat_val
  kcat_dH <- DHScale$kcat_dH
  kcat_T <- enzyme$kcat_T
  Kc_val <- enzyme$Kc_val
  Kc_dH <- DHScale$Kc_dH
  Kc_T <- enzyme$Kc_T
  Ko_val <- enzyme$Ko_val
  Ko_dH <- DHScale$Ko_dH
  Ko_T <- enzyme$Ko_T
  S_val <- enzyme$S_val
  S_dH <- DHScale$S_dH
  S_T <- enzyme$S_T

  # create temperature dependence function for each kinetic variable
  kcat <- kinetics_T_dep(kcat_val, kcat_dH, kcat_T)
  kc <- kinetics_T_dep(Kc_val, Kc_dH, Kc_T)
  ko <- kinetics_T_dep(Ko_val, Ko_dH, Ko_T)
  s <- kinetics_T_dep(S_val, S_dH, S_T)

  # use different formula depending on the pathway chosen
  if (pathway == "gross") {
    out_func <- function(CO2, O2, temp) {
      kcat(temp) * CO2 / (CO2 + kc(temp) + kc(temp) * (O2 / ko(temp)))
    }
  } else if (pathway == "canon") {
    out_func <- function(CO2, O2, temp) {
      kcat(temp) * (CO2 / (CO2 + kc(temp) + kc(temp) * (O2 / ko(temp)))) * (1 - O2 / (2 * s(temp) * CO2))
    }
  } else if (pathway == "alt") {
    out_func <- function(CO2 ,O2, temp) {
      kcat(temp) * (CO2 / (CO2 + kc(temp) + kc(temp) * (O2 / ko(temp)))) * (1 - 2 * O2 / (s(temp) * CO2))
    }
  } else if (pathway == "diatom") {
    out_func <- function(CO2, O2, temp) {
      kcat(temp) * (CO2 / (CO2 + kc(temp) + kc(temp) * (O2 / ko(temp)))) * (1 - O2 / (s(temp) * CO2))
    }
  }

  # return the function constructed
  out_func
}

#' Creates a combined model that compares two different enzymes
#'
#' Creates a combined model that compares two different enzymes. Requires the
#' input of two models output from the CO2_dependence equation.
#'
#' @param model1 The first model to compare. Positive values from the output
#'   equation will mean that this modeled enzyme is faster.
#' @param model2 The second model to compare. Negative values from the output
#'   equation will mean that this modeled enzyme is faster.
#' @returns A function with the same inputs as the original models comparing
#'   the two models.
#' @export
#' @examples
#' pro_enzyme <- new_enzyme(6.6, 144, 394, 10.8, name="pro") 
#' bacIA_DHScale <- new_DHScale(47.2,40.8,26.7,-21.8,name="bacIA")
#' pro_model <- CO2_dependence(pro_enzyme, bacIA_DHScale, "canon") #first model
#' tmrIA_DHScale <- modify_DHScale(bacIA_DHScale,kcat_dH=32,name="tmr_IA")
#' tmrIA_model <- CO2_dependence(pro_enzyme, tmrIA_DHScale, "canon") #second model
#' tmr_vs_pro <- CO2_comparison(pro_model, tmrIA_model) #creating the comparison
#' print(tmr_vs_pro) # prints the comparison equation
#' # prints the amount for which the tmrIA_model is faster than the pro_model
#' tmr_vs_pro(126,54,15) 
#'
CO2_comparison <- function(model1, model2) {

  # create the comparison function based on two input functions
  cmp_func <- function(CO2, O2, temp) {
    model1(CO2, O2, temp) - model2(CO2, O2, temp)
  }

  # return the function constructed
  cmp_func
}