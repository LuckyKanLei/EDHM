#' AerodynamicResistance paramter caculate
#' @param VegetationDisplacementHeight Vegetation Displacement Height
#' @param VegetationRoughLength Vegetation Rough Length
#' @param WindSpeed Wind Speed
#' @return AerodynamicResistance
#' @export
fctAerodynamicResistance <- function(VegetationDisplacementHeight, VegetationRoughLength, WindSpeed){
  AerodynamicResistance <- 4.72 * (log(VegetationDisplacementHeight / VegetationRoughLength))^2 /
    (1 + 0.54 * WindSpeed)
  return(AerodynamicResistance)
}


#' dew point caculate
#' @param actual_vp actual vapor pressure
#' @return dew point temperature
#' @export
get_Dew_point <- function(actual_vp) {
  log_e <- log10(actual_vp / 611.2)
  T_d <- 243.5 * log_e / (17.67 - log_e)
  return(T_d)
}

#' Saturat_vapor_pressure caculate
#' @param Temperature Temperature
#' @return Saturat vapor pressure
#' @export
get_Saturat_vapor_pressure <- function(Temperature){
  return(0.6108 * exp(17.27 * Temperature / (Temperature + 237.3)))
}
