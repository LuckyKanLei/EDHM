#' @title SnowPackEnergyBalance
#' @description Calculate the surface energy balance for the snow pack.
#' @param InData list of Input data
#' @param Param Paramaters and inputdata
#' @return a out list
#' @export
SnowPackEnergyBalance <- function(InData, Param) {
  #### initialize variable ####
  Tair <- InData$MetData$TAir
  LongSnowIn <- InData$MetData$LongWave
  AirDens <- InData$MetData$AirDensity
  EactAir <- InData$MetData$VaporPressure
  Press <- InData$MetData$AirPressure
  Vpd <- InData$MetData$VaporPressDeficit

  Wind <- InData$Aerodyna$WindSpeedSnow
  Ra <- InData$Aerodyna$AerodynaResistSnow
  RefHeight <- InData$Aerodyna$ReferHeightSnow
  RoughHeight <- InData$Aerodyna$RoughSnow
  Displacement <- InData$Aerodyna$DisplacSnow

  TGrnd <- InData$Energy$TGrnd
  OldTSurf <- InData$Energy$OldTSurf ##?##
  NetShortUnder <- InData$Energy$NetShortSnow

  TSurf <- InData$Snow$SurfTemp
  SnowDensity <- InData$Snow$Density
  SnowDepth <- InData$Snow$Depth
  SnowCoverFract <- InData$Snow$Coverage
  SurfaceLiquidWater <- InData$Snow$SurfWater

  SweSurfaceLayer <- InData$SurfaceSnowWater
  blowing_flux <- InData$Atmos$Blowing
  Rain <- InData$Prec$RainFall

  Dt <- Param$TimeStepSec


  TMean <- TSurf
  waterDensity <- CONST_RHOFW
  Lv <- calc_latent_heat_of_vaporization(Tair)

  #### Correct aerodynamic conductance for stable conditions
  # Note: If air temp >> snow temp then aero_cond -> 0 (i.e. very stable)
  # velocity (vel_2m) is expected to be in m/sec  ####

  #### Apply the stability correction to the aerodynamic resistance
  # NOTE: In the old code 2m was passed instead of Z-Displacement.  I (bart)
  # think that it is more correct to calculate ALL fluxes at the same
  # reference level  ####
  judgeWI <- (Wind > 0.0)
  Ra_used <- (Ra / (StabilityCorrection(RefHeight, Displacement,
                                        TMean, Tair, Wind,
                                        RoughHeight))) * judgeWI +
    Param$HUGE_RESIST * (!judgeWI)

  #### Calculate longwave exchange and net radiation  ####

  Tmp <- TMean + CONST_TKFRZ
  NetLongUnder <- LongSnowIn - calc_outgoing_longwave(Tmp, Param$EMISS_SNOW)
  NetRad <- NetShortUnder + NetLongUnder

  #### Calculate the sensible heat flux  ####
  SensibleHeat <- calc_sensible_heat(AirDens, Tair, TMean, Ra_used)
  #### Add in Sensible heat flux turbulent exchange from surrounding snow free patches - if present  ####
  AdvectedSensibleHeat <- advected_sensible_heat(SnowCoverFract,
                                                 AirDens,
                                                 Tair,
                                                 TGrnd,
                                                 Ra_used) * (SnowCoverFract > 0.)

  #### Convert sublimation terms from m/timestep to kg/m2s  ####
  BlowingMassFlux <- blowing_flux * waterDensity / Dt

  #### Calculate the mass flux of ice to or from the surface layer  ####
  #### Calculate the saturated vapor pressure in the snow pack, (Equation 3.32, Bras 1990)  ####
  LHSOut <- latent_heat_from_snow(AirDens, EactAir, Lv, Press, Ra_used, TMean, Vpd,
                                  BlowingMassFlux, Param)
  LatentHeat <- LHSOut$LatentHeat
  LatentHeatSub <- LHSOut$LatentHeatSub
  VaporMassFlux <- LHSOut$VaporMassFlux
  BlowingMassFlux <- LHSOut$BlowingMassFlux
  SurfaceMassFlux <- LHSOut$SurfaceMassFlux

  #### Convert sublimation terms from kg/m2s to m/timestep  ####
  vapor_flux <- VaporMassFlux * Dt / waterDensity
  blowing_flux <- BlowingMassFlux * Dt / waterDensity
  surface_flux <- SurfaceMassFlux * Dt / waterDensity

  #### Calculate advected heat flux from rain
  # Equation 7.3.12 from H.B.H. for rain falling on melting snowpack  ####
  AdvectedEnergy <- ((CONST_CPFW * CONST_RHOFW * (Tair) * Rain) / Dt) * (TMean == 0.0)

  #### Calculate change in cold content  ####
  DeltaColdContent <- CONST_VCPICE_WQ * SweSurfaceLayer * (TSurf - OldTSurf) / Dt
  #### Calculate Ground Heat Flux  ####
  GroundFlux <- (Param$SNOW_CONDUCT * (SnowDensity * SnowDensity) * (TGrnd - TMean) /
                   (SnowDepth + DBL_EPSILON) / Dt) * (SnowDepth <= Param$SNOW_DEPTH_THRES)
  DeltaColdContent <- DeltaColdContent - GroundFlux

  #### Calculate energy balance error at the snowpack surface  ####
  RestTerm <- NetRad + SensibleHeat + LatentHeat + LatentHeatSub + AdvectedEnergy +
    GroundFlux - DeltaColdContent + AdvectedSensibleHeat

  RefreezeEnergy <- (SurfaceLiquidWater * CONST_LATICE * waterDensity) / Dt
  judgeTR <- ((TSurf == 0.0) & (RestTerm > -RefreezeEnergy))
  RefreezeEnergy[which(judgeTR)] <- -RestTerm[which(judgeTR)]
  RestTerm <- (RestTerm + RefreezeEnergy) * (!judgeTR)

  return(list(RestTerm = RestTerm,
              RefreezeEnergy = RefreezeEnergy,
              DeltaColdContent = DeltaColdContent,
              GroundFlux = GroundFlux,
              VaporFlux = vapor_flux,
              Blowing = blowing_flux,
              SurfaceFlux = surface_flux,
              LatentHeat = LatentHeat,
              LatentHeatSub = LatentHeatSub))
}
#' @title calcSurfSnowRestTerm
#' @description Calculate the surface energy balance for the snow pack.
#' Virtual function of SnowPackEnergyBalance
#' @param TSurf tempratur of snowfurface
#' @param InData list of Input data
#' @param Param Paramaters and inputdata
#' @return a out list
#' @export
calcSurfSnowRestTerm <- function(TSurf, InData, Param) {
  InData$Snow$SurfTemp <- TSurf
  return(abs((SnowPackEnergyBalance(InData, Param))$RestTerm))
}


#' @title CanopyEnergyBalance
#' @description This routine iterates to determine the temperature of the canopy, and solve
#' the resulting fluxes between the canopy and the atmosphere and the canopy
#' and the ground.
#' @param InData list of Input data
#' @param Param Paramaters and inputdata
#' @return a out list
#' @export
CanopyEnergyBalance <- function(InData, Param) {
  #### initialize variable ####
  displacement <- InData$Aerodyna$DisplacCanopy
  Wind <- InData$Aerodyna$WindSpeedCanopy
  Ra1 <- InData$Aerodyna$AerodynaResistCanopy
  ref_height <- InData$Aerodyna$ReferHeightCanopy
  roughness <- InData$Aerodyna$RoughCanopy

  VaporMassFlux <- InData$Atmos$PVegVaporFlux
  IntSnow <- InData$Intercept$InterceptSnow
  IntRain <- InData$Intercept$InterceptRain
  Rainfall <- InData$Prec$RainFall
  canopy_evap <- InData$ET$EvaporationCanopy

  Tfoliage <- InData$Energy$TFoliage
  Tcanopy <- InData$Energy$TCanopy
  NetShortOver <- InData$Energy$NetShortOver
  LongOverIn <- InData$Energy$LongOverIn
  LongUnderOut <- InData$Energy$LongUnderOut
  Le <- InData$Energy$VaporizLatentHeat

  AirDens <- InData$MetData$AirDensity
  EactAir <- InData$MetData$VaporPressure
  Press <- InData$MetData$AirPressure
  Vpd <- InData$MetData$VaporPressDeficit

  delta_t <- Param$TimeStepSec

  #### Calculate the net radiation at the canopy surface, using the canopy
  # temperature.  The outgoing longwave is subtracted twice, because the
  # canopy radiates in two directions ####
  LongOverOut <- calc_outgoing_longwave(Tfoliage + CONST_TKFRZ,
                                        Param$EMISS_VEG)
  NetRadiation <- NetShortOver + LongOverIn + LongUnderOut - 2 * LongOverOut

  NetLongOver <- LongOverIn - LongOverOut

  judgeIS <- (IntSnow > 0)
  judgeWI <- (Wind > 0.0)
  EsSnow <- calc_saturated_vapor_pressure(Tfoliage, Param)  ##?## add PaList
  Ra_used1 <- Ra1
  #### Calculate the vapor mass flux between intercepted snow in
  # the canopy and the surrounding air mass ####
  Ra_used1 <- (Ra_used1 / (StabilityCorrection(ref_height,
                                               displacement, Tfoliage, Tcanopy, Wind,
                                               roughness))) * (judgeIS & judgeWI) +
    Param$HUGE_RESIST * (judgeIS & !judgeWI) + Ra_used1 * (!judgeIS)
  judgeVV <- ((Vpd == 0.0) & (VaporMassFlux < 0.0))
  VaporMassFlux[which(judgeIS)] <- ((AirDens * (CONST_EPS / Press) * (EactAir - EsSnow) /
                                       Ra_used1 / CONST_RHOFW) * (judgeIS & !judgeVV))[which(judgeIS)]
  Wdew <- IntRain * MM_PER_M
  prec <- Rainfall * MM_PER_M
  Evap <- canopy_evap
  Ls <- calc_latent_heat_of_sublimation(Tfoliage)
  LatentHeatSub <- (Ls * (VaporMassFlux) * CONST_RHOFW) * judgeIS
  LatentHeat <- Le * Evap * CONST_RHOFW * (!judgeIS)
  Wdew <- Wdew / MM_PER_M

  #### Calculate the sensible heat flux ####
  SensibleHeat <- calc_sensible_heat(AirDens, Tcanopy, Tfoliage, Ra_used1)

  #### Calculate the advected energy ####
  AdvectedEnergy <- (CONST_CPFW * CONST_RHOFW * Tcanopy * Rainfall) / (delta_t)

  #### Calculate the amount of energy available for refreezing ####
  RestTerm <- SensibleHeat + LatentHeat + LatentHeatSub + NetRadiation + AdvectedEnergy

  #### Intercepted snow present, check if excess energy can be used to
  # melt or refreeze it ####
  RefreezeEnergy <- ((IntRain * CONST_LATICE * CONST_RHOFW) / delta_t) * judgeIS
  judgeRR <- ((Tfoliage == 0.0) & (RestTerm > -RefreezeEnergy))
  #### available energy input over cold content used to melt, i.e. Qrf is negative value (energy out of pack) ####
  #### add this positive value to the pack ####
  RefreezeEnergy[which(judgeIS & judgeRR)] <- -RestTerm[which(judgeIS & judgeRR)]
  RestTerm <- (RestTerm + RefreezeEnergy) * ((judgeIS & !judgeRR)) + RestTerm * (!judgeIS)
  # browser()
  return (list(RestTerm = RestTerm,
               RefreezeEnergy = RefreezeEnergy,
               LongOverOut = LongOverOut,
               NetRadiation = NetRadiation,
               NetLongOver = NetLongOver,
               LatentHeatSub = LatentHeatSub,
               LatentHeat = LatentHeat,
               VegVaporFlux = VaporMassFlux))
}
#' @title calcCanopyRestTerm
#' @description This routine iterates to determine the temperature of the canopy, and solve
#' the resulting fluxes between the canopy and the atmosphere and the canopy
#' and the ground.
#' Virtual function of CanopyEnergyBalance
#' @param InData list of Input data
#' @param Param Paramaters and inputdata
#' @param Tfoliage tempratur of foliage
#' @return a out list
#' @export
calcCanopyRestTerm <- function(Tfoliage, InData, Param) {
  InData$Energy$TFoliage <- Tfoliage
  return(abs((CanopyEnergyBalance(InData, Param))$RestTerm))
}



#' @title StabilityCorrection
#' @description Calculate atmospheric stability correction for non-neutral conditions
#' @param Z ref_height
#' @param d displacement
#' @param TSurf tempratur of snow surface
#' @param Tair tempratur of air
#' @param Wind wind speed
#' @param Z0 roughness
#' @return Correction
#' @export
StabilityCorrection <- function(Z,
                                d,
                                TSurf,
                                Tair,
                                Wind,
                                Z0) {

  RiCr <- 0.2           #### Critical Richardson's Number ####
  Correction <- Tair        #### Correction to aerodynamic resistance ####

  #### Calculate the effect of the atmospheric stability using a Richardson
  # Number approach ####
  judgeSA <- (TSurf != Tair)

  Ri <- CONST_G * (Tair - TSurf) * (Z - d) /
    (((Tair + CONST_TKFRZ) +
        (TSurf + CONST_TKFRZ)) / 2.0 * Wind^2) # 17.
  RiLimit <- (Tair + CONST_TKFRZ) /
    (((Tair + CONST_TKFRZ) +
        (TSurf + CONST_TKFRZ)) / 2.0 * (log((Z - d) / Z0) + 5)) # 24.
  judgeLI <- (Ri > RiLimit)
  judgeRI <- (Ri > 0.0)
  judgeR5 <- (Ri < -0.5)

  Ri <- RiLimit * judgeLI + -0.5 * judgeR5 + Ri * (!(judgeLI | judgeR5))

  Correction <- ((1 - Ri / RiCr)^2) * (judgeSA & judgeRI) +
    (sqrt(abs(1 - 16 * Ri))) * (judgeSA & (!judgeRI)) + !judgeSA # 18. 19.
  return(Correction)
}

#' @title calc_sensible_heat
#' @description Compute the sensible heat flux.
#' @param atmos_density atmos density
#' @param t1 Tcanopy
#' @param t0 Tfoliage
#' @param Ra AerodynaResistCanopy
#' @return sensible_heat
#' @export
calc_sensible_heat <- function(atmos_density,
                               t1,
                               t0,
                               Ra) {
  return(CONST_CPMAIR * atmos_density * (t1 - t0) / Ra)
}

#' @title advected_sensible_heat
#' @description Compute the sensible heat flux advected from bare soil patches to
#' snow covered patches.
#' @param SnowCoverFract Snow Cover Fraction
#' @param AirDens atmos density
#' @param Tair air tempratur
#' @param TGrnd grund sufface tempratur
#' @param Ra AerodynaResist snow surface
#' @return advected sensible heat
#' @export
advected_sensible_heat <- function(SnowCoverFract,
                                   AirDens,
                                   Tair,
                                   TGrnd,
                                   Ra) {
  #### Compute sensible heat flux from bare patches ####
  Qbare = calc_sensible_heat(AirDens, Tair, TGrnd, Ra)

  #### Compute fraction of sensible heat that contributes to snowmelt ####
  judgeF6 <- (SnowCoverFract > 0.6)
  judgeF5 <- (SnowCoverFract > 0.5)
  judgeF2 <- (SnowCoverFract > 0.2)
  Fs <- judgeF6 + (10^(3. * SnowCoverFract - 1.8)) * ((!judgeF6) & judgeF5) +
    (10^(5.6667 * SnowCoverFract - 3.1333)) * ((!judgeF5) & judgeF2) +
    0.01 * (!judgeF2)

  #### Compute advected sensible heat flux ####
  Qadv = (Qbare * (1.0 - SnowCoverFract)) / (SnowCoverFract + DBL_EPSILON) * Fs

  return (Qadv)
}

#' @title latent_heat_from_snow
#' @description Calculate the saturated vapor pressure in the snow pack,
#' (Equation 3.32, Bras 1990)
#' @param AirDens atmos density
#' @param EactAir VaporPressure
#' @param Lv latent_heat_of_vaporization
#' @param Press air Press
#' @param Ra AerodynaResist
#' @param TMean tempratur
#' @param Vpd Vapor Press Defficit
#' @param BlowingMassFlux Blowing Mass Flux
#' @param Param paramters list
#' @return latent_heat
#' @export
latent_heat_from_snow <- function(AirDens,
                                  EactAir,
                                  Lv,
                                  Press,
                                  Ra,
                                  TMean,
                                  Vpd,
                                  BlowingMassFlux,
                                  Param) {

  EsSnow = calc_saturated_vapor_pressure(TMean, Param) ##?## add PaList

  #### SurfaceMassFlux and BlowingMassFlux in kg/m2s

  SurfaceMassFlux = AirDens * (CONST_EPS / Press) * (EactAir - EsSnow) / Ra
  judgeVS <- ((Vpd == 0.0) & (SurfaceMassFlux < 0.0))
  SurfaceMassFlux <- SurfaceMassFlux * (!judgeVS)
  #### Calculate total latent heat flux ####

  VaporMassFlux = SurfaceMassFlux + BlowingMassFlux
  #### Melt conditions: use latent heat of vaporization ####
  #### Accumulation: use latent heat of sublimation (Eq. 3.19, Bras 1990 ####
  judgeT0 <- (TMean >= 0.0)
  Ls = calc_latent_heat_of_sublimation(TMean)
  LatentHeat <- (Lv * VaporMassFlux) * judgeT0
  LatentHeatSublimation <- (Ls * VaporMassFlux) * (!judgeT0)
  return(list(LatentHeat = LatentHeat,
              LatentHeatSublimation = LatentHeatSublimation,
              VaporMassFlux = VaporMassFlux,
              BlowingMassFlux = BlowingMassFlux,
              SurfaceMassFlux = SurfaceMassFlux))

}



