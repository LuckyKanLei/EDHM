#' @title SNOW
#' @description This routine was written to handle the various calls and data
#' handling needed to solve the various components of the new VIC
#' snow code for both the full_energy and water_balance models.
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... other arguments
#' @return use SNOW(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOW <- function(InData, ...) UseMethod("SNOW", InData)

#' Snow model day-degree-factor
#' @references Hock, R. Temperature index melt modelling in mountain areas. J. Hydrol. 2003, 282, 104–115.
#' @param InData 2-list of:
#' \itemize{
#' \item TAir
#' \item RainFall
#' \item SnowFall
#' }
#' @param Param 2-list of:
#' \itemize{
#' \item Factor_Day_degree
#' \item Base_T
#' }
#' @param ... other Parmeters
#' @return 1-list of:
#' \itemize{
#' \item Precipitation
#' }
#' @export SNOW.Ddf
#' @export
SNOW.Ddf <-function(InData, Param, ...){
  Snow_Volum_mm <- InData$Snow$Volume + InData$Prec$SnowFall
  Melt_Max <- Param$Factor_Day_degree * maxSVector(0, (InData$MetData$TAir - Param$Base_T))
  Melt <- minVector(Melt_Max, Snow_Volum_mm)
  return(list(Snow = list(Volume = Snow_Volum_mm - Melt),
    Prec = list(Precipitation = Melt + InData$Prec$RainFall)))
}

#' Snow model SNOW17
#' @references Anderson, E.A., 'National Weather Service River Forecast System - Snow Accumulation and Ablation Model', NOAA Technical Memorandum NWS HYDRO-17, 217 pp, November 1973.
#' @param InData 2-list of:
#' \itemize{
#' \item TAir
#' \item RainFall
#' \item SnowFall
#' }
#' @param Param 2-list of:
#' \itemize{
#' \item Factor_Day_degree
#' \item Base_T
#' }
#' @param ... other Parmeters
#' @return 1-list of:
#' \itemize{
#' \item Precipitation
#' }
#' @export SNOW.17
#' @export
SNOW.17 <- function(InData, Param, ...) {
  delta_t_h <- Param$TimeStepSec / 3600
  SCF <- Param$SN17_SCF ## 0.7-1.4
  MFMAX <- Param$SN17_MFMAX ## 0.5-2
  MFMIN <- Param$SN17_MFMIN ## 0.05-0.49
  UADJ <- Param$SN17_UADJ ## 0.03-0.19
  NMF <- Param$SN17_NMF ## 0.05-0.5
  TIPM <- Param$SN17_TIPM ## 0.1-1
  PXTEMP <- Param$SN17_PXTEMP ## -0.2-0.2
  MBASE <- Param$SN17_MBASE ## 0-1
  PLWHC <- Param$SN17_PLWHC ## 0.02-0.3
  DAYGM <- Param$SN17_DAYGM ## 0-0.3

  elev <- InData$GeoData$Elevation
  n_day <- InData$TimeData$NDay

  W_i <- InData$Snow$Ice_Volume # water equivalent of the ice portion of the snow cover (mm)
  ATI <- InData$Snow$SN17_ATI # antecedent temperature index (˚C),
  W_q <- InData$Snow$Liquid_Volume # liquid water held by the snow (mm)
  Deficit <- InData$Snow$SN17_HD # heat deficit (mm)

  Snow_ <- InData$Prec$SnowFall
  Rain_ <- InData$Prec$RainFall
  Ta <- InData$MetData$TAir # [Cel]

  Pn <- Snow_ * SCF  #1## Water equivalent of new snowfall [mm]
  W_i <- W_i + Pn  # Water equivalent of the ice portion of the snow cover [mm]
  E <- 0  # Excess liquid water in the snow cover

  judge_T15 <- Ta <= -15
  ##*## not correct, minus
  # rho_n <- 0.05 + (!judge_T15) * (0.0017 * abs(Ta)^1.5) #2# The density of new snow, [gm·cm3]
  # Hn <- (0.1 * Pn) / rho_n #3## depth of new snowfall [cm]

  Tn <- minSVector(0, Ta) ## temperature of the new snow [Cel],

  ## Lf = latent heat of fusion (80 cal·gm-1)
  ## ci = specific heat of ice (0.5 cal·gm-1·Cel-1)
  Delta_Dp <- -(Pn * Tn) / 80 * 0.5 #4## change in the heat deficit due to snowfall [mm]

  Tr <- maxSVector(0, Ta) ## temperature of rain [Cel]
  e_sat <- 2.7489e8 * exp(-4278.63/(Ta + 242.792)) ## saturated vapor pressure at Ta (mb)
  P_atm <- 33.86 * (29.9 - (0.335 * elev) + (0.00022 * (elev^2.4))) ## atmospheric pressure (mb)
  Mr <- 6.12e-10 * delta_t_h * (((Ta + 273)^4) - (273^4)) +
    0.0125 * Rain_ * Tr +
    8.5 * UADJ * (delta_t_h/6) * ((0.9 * e_sat - 6.11) + (0.00057 * P_atm * Ta)) #5## melt during rain-on-snow time intervals (mm)

  N_Mar21 <- n_day - 80
  Sv <- (0.5 * sin((N_Mar21 * 2 * pi)/365)) + 0.5  # Seasonal variation
  Av <- 1  # Seasonal variation adjustment, Av<-1.0 when lat < 54N
  Mf <- delta_t_h/6 * ((Sv * Av * (MFMAX - MFMIN)) + MFMIN)  #7## Seasonally varying non-rain melt factor
  Mnr <- Mf * (Ta - MBASE) + (0.0125 * Rain_ * Tr) #6## melt during non-rain periods (mm),
  judge_rb025 <- Rain_ > 0.25 * delta_t_h
  judge_tabb <- Ta > MBASE
  Melt <- judge_rb025 * Mr + ((!judge_rb025) & judge_tabb) * Mnr
  Melt <- maxSVector(0, Melt)

  # Change in the heat deficit due to new snowfall [mm], 80 cal/g: latent heat of fusion, 0.5 cal/g/C:
  # specific heat of ice
  delta_HD_snow <- -(Tn * Pn)/(80/0.5)

  delta_HD_T <- NMF * delta_t_h /6 * Mf/MFMAX * (ATI - Tn) #9## Heat Exchange due to a temperature gradient change in heat deficit due to a temperature gradient [mm]

  judge_b15 <- Pn > 1.5 * Pn
  ATI <- judge_b15 * Tn +
    (!judge_b15) * (ATI + (1-(1 - TIPM)^(delta_t_h/6)) * (Ta - ATI))
  ATI <- minSVector(0, ATI)

  Deficit <- maxSVector(0, Deficit + delta_HD_snow + delta_HD_T)  # Deficit <- heat deficit [mm]
  Deficit <- minVector(Deficit, 0.33 * W_i)

  judge_MbeW <- Melt >= W_i
  Melt[judge_MbeW] <- (W_i + W_q)[judge_MbeW]  # Melt >= W_i

  W_i <- W_i - Melt
  Qw <- Melt + Rain_
  W_qx <- PLWHC * W_i #10## liquid water capacity (mm)


  judge_QWk <- (Qw + W_q) < Deficit
  judge_QWb <- (Qw + W_q) > (Deficit + Deficit * PLWHC + W_qx)
  judge_QWm <- !(judge_QWk | judge_QWb)

  E <- judge_QWb * (Qw + W_q - W_qx - Deficit - (Deficit * PLWHC))
  W_i <- (!judge_QWk) * (W_i + Deficit) + judge_QWk * (W_i + Qw + W_q)
  Deficit <- judge_QWk * (Deficit - Qw - W_q)
  W_q <- W_q * judge_QWk +
    judge_QWb * (W_qx + PLWHC * Deficit) +
    judge_QWm * (W_q + Qw - Deficit)

  W_i[judge_MbeW] <- 0
  W_q[judge_MbeW] <- 0
  Qw_judge_MbeW <- (Melt + Rain_)[judge_MbeW]

  E[judge_MbeW] <- Qw[judge_MbeW] <- Qw_judge_MbeW


  ATI[Deficit == 0] <- 0
  # Constant daily amount of melt which takes place at the snow-soil interface whenever there is a
  # snow cover
  judge_WbD <- W_i > DAYGM
  gmwlos <- (DAYGM/W_i) * W_q
  gmwlos[is.na(gmwlos)] <- 0
  gmslos <- DAYGM
  gmro <- judge_WbD * (gmwlos + gmslos) + (!judge_WbD) * W_i + W_q
  W_i <- judge_WbD * (W_i - gmslos)
  W_q <- judge_WbD * (W_q - gmwlos)
  E <- E + gmro
  SWE <- (W_i + W_q)
  return(list(Snow = list(Ice_Volume = W_i,
                          Liquid_Volume = W_q,
                          SN17_ATI = ATI,
                          SN17_HD = Deficit),
              Prec = list(Precipitation = E)))

}
