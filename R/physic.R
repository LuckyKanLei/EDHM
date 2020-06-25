#' @title calc_latent_heat_of_vaporization
#' @description This subroutine computes the latent heat of vaporization.
#' @references Eq. 4.2.1 in Handbook of Hydrology.
#' @param temp temprature
#' @return latent_heat_of_vaporization
#' @export
calc_latent_heat_of_vaporization <- function(temp)
{
  return(CONST_LATVAP - 2361. * temp)
}
#' @title calc_latent_heat_of_sublimation
#' @description Compute the latent heat of sublimation.
#' @references Eq. 3.19, Bras 1990
#' @param temp temprature
#' @return latent_heat_of_sublimation
#' @export
calc_latent_heat_of_sublimation <- function(temp) {
  return((677. - 0.07 * temp) * JOULES_PER_CAL * GRAMS_PER_KG);
}

#' @title calc_outgoing_longwave
#' @description Compute outgoing longwave using the Stefan-Boltzman Law.
#' @param temp temprature
#' @param emiss Snow emissivity
#' @return outgoing_longwave
#' @export
calc_outgoing_longwave <- function(temp,
                       emiss) {
  return(emiss * CONST_STEBOL * temp * temp * temp * temp);
}

#' @title calc_scale_height
#' @description Calculate scale height based on average temperature in the column
#' @param tair air temprature
#' @param elevation elevation
#' @param Param params list
#' @return scale_height
#' @export
calc_scale_height <- function(tair,
                  elevation, Param)
{
  h <- CONST_RDAIR / CONST_G *
    ((tair + CONST_TKFRZ) + 0.5 * elevation * Param$LAPSE_RATE)

  return(h)
}

#' @title calc_sensible_heat
#' @description Compute the sensible heat flux.
#' @param atmos_density atmos density
#' @param t1 tempratur
#' @param t0 tempratur
#' @param Ra AerodynaResist
#' @return scale_height
#' @export
calc_sensible_heat <- function(atmos_density,
                   t1,
                   t0,
                   Ra)
{
  sensible <- CONST_CPMAIR * atmos_density * (t1 - t0) / Ra

  return(sensible)
}

#' @title calc_saturated_vapor_pressure
#' @description This routine computes the saturated vapor pressure
#' @references Handbook of Hydrology eqn 4.2.2.
#' @param temp air temprature
#' @param Param Param list
#' @return saturated_vapor_pressure
#' @export
calc_saturated_vapor_pressure <- function(temp, Param) { # svp
  judgeT0 <- (temp < 0)
  SVP <- Param$SVP_A * exp((Param$SVP_B * temp) / (Param$SVP_C + temp))
  SVP <- SVP * ((1.0 + .00972 * temp + .000042 * temp * temp) * judgeT0 + (!judgeT0)) * PA_PER_KPA
  return (SVP);
}

#' @title q_to_vp
#' @description   convert specific humidity (q) to vapor pressure (vp) based on
#'          pressure (p)
#' @param q specific humidity
#' @param p pressure
#' @return vp vapor pressure (units are the same as p)
q_to_vp <- function(q, p)
{
  ## full equation
  # vp = q/(q+CONST_EPS*(1-q))*p;
  ## approximation used in VIC
  vp = q * p / CONST_EPS

  return(vp)
}

#' @title air_density
#' @description   convert surface pressure (Pa) to density (kg/m3) based on
#'          pressure (p), vapor pressure (vp), and temperature
#' @param t temperature
#' @param p pressure
#' @return rho surface pressure
air_density <- function(t, p)
{
  ## full equation
  ## rho = (p*1000)/(Rd * t + CONST_TKFRZ) + (pv * 1000)/(Rv * t + CONST_TKFRZ);

  ## approximation used in VIC
  rho = p / (CONST_RDAIR * (CONST_TKFRZ + t))

  return(rho)
}
