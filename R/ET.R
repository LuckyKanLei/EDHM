#' ReferenceET caculate
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... other Paramater and inputdata
#' @return ReferenceET
#' @export
ReferenceET <- function(InData, ...) UseMethod("ReferenceET", InData)

#' ReferenceET with PenmanMonteith methond
#' @references  Pengman H L. Estimating evaporation[J]. American Geophysical Union, 1956(1):43-50.
#' @param InData list of:
#' \itemize{
#' \item JDay, date
#' \item Elevation, m
#' \item Latitude
#' \item Tmax, oC, Celius
#' \item Tmin, oC, Celius
#' \item Tmean, oC, Celius
#' \item Wind, speed, m/s
#' \item WindH, hoch, speed messeur
#' \item SunHour,
#' \item RelativeHumidity,
#' }
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @param ... other Parmeters
#' @return Reference ET with PenmanMonteith methond
#' @export ReferenceET.PenMon
#' @export
ReferenceET.PenMon <- function(InData, Param, ...){
  JDay <- InData$TimeData$NDay
  Elevation <- InData$GeoData$Elevation
  Latitude <- InData$GeoData$Latitude
  Tmax <- InData$MetData$TMax
  Tmin <- InData$MetData$TMin
  Tmean <- InData$MetData$TAir
  WindSpeed <- InData$MetData$WindSpeed
  WindH <- InData$MetData$WindH
  SunHour <- InData$MetData$SunHour
  RelativeHumidity <- InData$MetData$RelativeHumidity
  dimV <- dim(Tmean)
  timN <- Param$PeriodN
  gridN <- Param$GridN
  JDay <- matrix(rep(JDay,gridN), timN, gridN)
  Elevation <- matrix(rep(Elevation,timN), timN, gridN, byrow = T)
  Latitude <- matrix(rep(Latitude,timN), timN, gridN, byrow = T)
  # DailySolarRadiationMJ = 0.0864 * DailySolarRadiationW  #Equation 6  ##1W m-2 = 0.0864 MJ m-2
  Wind2 <- WindSpeed * 4.87 / log(67.8 * WindH - 5.42)  #Equation 7
  Delt <- 4098 * (0.6108 * exp(17.27 * Tmean / (Tmean + 237.3))) / (Tmean + 237.3)^2  #Equation 9
  Pressure <- 101.3 * ((293 - 0.0065 * Elevation) / 293)^5.26  #Equation 10
  Gama <- 0.000665 * Pressure  #Equation 11  ##γ = psychrometric constant, kPa °C-1;P = atmospheric pressure, kPa, [Eq. 10];λ = latent heat of vaporization, 2.45, MJ kg-1;cp = specific heat at constant pressure, 1.013 10-3, MJ kg-1°C-1;μ = ratio molecular weight of water vapour/dry air = 0.622.
  DeltTerm <- Delt / (Delt + Gama * (1 + 0.34 * Wind2))  #Equation 12
  PressureTerm <- Gama / (Delt + Gama * (1 + 0.34 * Wind2))  #Equation 13
  TemperatureTerm <- (900 / (Tmean + 273)) * Wind2  #Equation 14
  ETemperature <- function(Temperature){
    return(0.6108 * exp(17.27 * Temperature / (Temperature + 237.3)))
  }  #Equation 15
  SaturationVaporPressure <- (ETemperature(Tmax) + ETemperature(Tmin)) / 2  #Equation 18
  # ActualVaporPressure = (ETemperature(Tmin) * MaximumRelativeHumidity / 100 + ETemperature(Tmax) * MinimumRelativeHumidity / 100) /2  #Equation 19
  ActualVaporPressure <- RelativeHumidity / 100.0 * (ETemperature(Tmax) + ETemperature(Tmin)) / 2
  EarthSun <- 1 + 0.033 * cos(2 * pi * JDay / 365.25)  #Equation 23
  SolarDeclination <- 0.409 * sin(2* pi *JDay / 365.25 - 1.39)  #Equation 24
  LatitudeRad <- pi * Latitude / 180 #Equation 25
  SunsetHourAngle <- acos(-1 * tan(LatitudeRad) * tan(SolarDeclination))  #Equation 26
  ExtraterrestrialRadiation <- 24 * 60 / pi * 0.082 * EarthSun * (SunsetHourAngle * sin(LatitudeRad) * sin(SolarDeclination) +
                                                                    cos(LatitudeRad) * cos(SolarDeclination) * sin(SunsetHourAngle))  #Equation 27 ##Gsc = solar constant = 0.0820 MJ m-2 min-1;
  ClearSkySolarRadiation <- ExtraterrestrialRadiation * (0.75 + 2*10^-5 * Elevation)  #Equation 28
  PossibleSunshineHours <- 24 / pi * SunsetHourAngle
  DailySolarRadiationMJ <- ExtraterrestrialRadiation * (0.14233 + 0.658667 * (SunHour / PossibleSunshineHours) +
                                                          0.028  * (77.6667 / 100) -
                                                          0.11533* (EarthSun / PossibleSunshineHours) * (77.6667 / 100)) ### in zhaona: Solar radiation estimation using sunshine hour and air pollution index in China
  NetShortwaveRadiation <- (1 - 0.23) * DailySolarRadiationMJ  #Equation 29  ##α = albedo or canopy reflection coefficient, which is 0.23 for the hypothetical grass reference crop, dimensionless
  NetOutgoingLongwaveRadiation <- 4.903*10^-9 * (((Tmax + 273.16)^4 + (Tmax + 273.16)^4) / 2) *
    (0.34 - 0.14 * ActualVaporPressure^0.5) * (1.35 * DailySolarRadiationMJ / ClearSkySolarRadiation - 0.35)  #Equation 30  ##σ = Stefan-Boltzmann constant [4.903 10-9 MJ K-4 m-2 day-1],
  NetRadiation <- NetShortwaveRadiation - NetOutgoingLongwaveRadiation  #Equation 31
  EquivalentNetRadiation <- 0.408 * NetRadiation  #Equation 32
  ETRadiation <- DeltTerm * EquivalentNetRadiation #Equation 33
  ETWind <- PressureTerm * TemperatureTerm * (SaturationVaporPressure - ActualVaporPressure)  #Equation 34
  RET <- ETRadiation + ETWind  #Equation 35
  return(list(Evatrans = list(RET = RET)))
}


#' ReferenceET with Hargreaves methond
#' @references  Hargreaves G H, Samani Z A. Reference crop evapotranspiration from ambient air temperature[J]. American Society of Agricultural Engineers, 1985(1):1-12.
#' @param InData list of: JDay, Latitude, Tmax, Tmin, Tmean
#' \itemize{
#' \item JDay, date
#' \item Latitude
#' \item Tmax, oC, Celius
#' \item Tmin, oC, Celius
#' \item Tmean, oC, Celius
#' }
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @param ... other Parmeters
#' @return ReferenceET with Hargreaves methond
#' @export ReferenceET.Hargreaves
#' @export
ReferenceET.Hargreaves <- function(InData, Param, ...){
  JDay <- InData$TimeData$NDay
  Latitude <- InData$GeoData$Latitude
  Tmax <- InData$MetData$Tmax
  Tmin <- InData$MetData$Tmin
  Tmean <- InData$MetData$Tmean
  timN <- Param$PeriodN
  gridN <- Param$GridN


  JDay <- matrix(rep(JDay,gridN), timN, gridN)
  Latitude <- matrix(rep(Latitude,timN), timN, gridN, byrow = T)

  EarthSun <- 1 + 0.033 * cos(2 * pi * JDay / 365.25)  #Equation 23
  SolarDeclination <- 0.409 * sin(2* pi *JDay / 365.25 - 1.39)  #Equation 24
  LatitudeRad <- pi * Latitude / 180 #Equation 25
  SunsetHourAngle <- acos(-1 * tan(LatitudeRad) * tan(SolarDeclination))  #Equation 26
  ExtraterrestrialRadiation <- 24 * 60 / pi * 0.082 * EarthSun *
    (SunsetHourAngle * sin(LatitudeRad) * sin(SolarDeclination) +
       cos(LatitudeRad) * cos(SolarDeclination) * sin(SunsetHourAngle))  #Equation 27 ##Gsc = solar constant = 0.0820 MJ m-2 min-1;
  ExtraterrestrialRadiationMM <- ExtraterrestrialRadiation / 2.45  #B.25 ##(Ra in mm d-1 = Ra in MJ m-2 d-1 / 2.45).
  RET <- 0.0023 * (Tmax - Tmin)^0.5 * (Tmean + 17.8) * ExtraterrestrialRadiationMM
  return(list(Evatrans = data.frame(RET = RET)))
}

#' ReferenceET with PenmanMonteith methond
#' @references  Linacre ET. 1977. A simple formula for estimating evaporation rates in various climates, using temperature data alone. AgriculturalMeteorology 18: 409-424.
#' @param InData list of:
#' \itemize{
#' \item Elevation, m
#' \item Latitude
#' \item Tmean, oC, Celius
#' \item Actual_vapor_press,
#' }
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @param ... other Parmeters
#' @return Reference ET with PenmanMonteith methond
#' @export ReferenceET.Linacre
#' @export
ReferenceET.Linacre <- function(InData, Param, ...) {
  Elevation <- InData$GeoData$Elevation
  Latitude <- InData$GeoData$Latitude
  Ta <- InData$MetData$TAir ## (data$Tmax + data$Tmin) / 2
  Tdew <- get_Dew_point(InData$MetData$Actual_vapor_press)
  T_m <- Ta + 0.006 * Elevation
  ET_Linacre <- (500 * T_m /(100 - Latitude)+15*(Ta - Tdew))/(80 - Ta)
  return(list(Evatrans = list(RET = ET_Linacre)))
}



#' Actual evapotranspiration
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return Actual evapotranspiration
#' @export
ActualET <- function(InData, ...) UseMethod("ActualET", InData)

#' Actual evapotranspiration in VIC
#' @references Liang X, Lettenmaier D P. A simple hydrologically based model of land surface water and energy fluxes for general circulation models[J]. Journal of Geophysical Research, 1994(99):415-428.
#' @import HMtools
#' @param InData 9-list of:
#' \itemize{
#' \item ReferenceEvap,
#' \item PrecipitationHoch,
#' \item MoistureVolume,
#' \item CapacityMax,
#' \item Volume1,
#' \item CapacityMax1,
#' \item AerodynamicResistance,
#' \item ArchitecturalResistance,
#' \item StomatalResistance,
#' }
#' @param Param 1-list of:
#' \itemize{
#' \item paramSoilMoistureCapacityB
#' }
#' @param ... other Parmeters
#' @return Actual evapotranspiration
#' @export ActualET.Vic
#' @export
ActualET.Vic <- function(InData, Param, ...){
  RET <- InData$Evatrans$RET
  # PrecipitationHoch <- InData$Prec$Precipitation

  MoistureVolume <- InData$Intercept$Interception

  MoistureCapacityMax <- InData$Canopy$StorageCapacity

  MoistureVolume1 <- InData$Ground$MoistureVolume
  MoistureCapacityMax1 <- InData$Ground$MoistureCapacityMax

  AerodynamicResistance <- InData$Aerodyna$AerodynaResist
  ArchitecturalResistance <- InData$Aerodyna$ArchitecturalResist
  StomatalResistance <- InData$Aerodyna$StomatalResist
# browser()
  paSoilMoistureCapacityB <- Param$SoilMoistureCapacityB
  EvaporationCnopyMax <- (MoistureVolume / MoistureCapacityMax)^0.6667 *
    (AerodynamicResistance / (AerodynamicResistance + ArchitecturalResistance)) * RET
  Rate <- minSVector(1, MoistureVolume / (EvaporationCnopyMax + DBL_EPSILON))
  Rate <- maxSVector(0.0,minSVector(1.0,Rate))
  EvaporationCanopy <- Rate * EvaporationCnopyMax
  Rate1 <- minSVector(1, MoistureVolume1 / (MoistureCapacityMax1 + DBL_EPSILON))
  Rate2 <- AerodynamicResistance /
    ( AerodynamicResistance + ArchitecturalResistance + StomatalResistance)
  Rate1 <- maxSVector(0.0,minSVector(1.0,Rate1))
  Rate2 <- maxSVector(0.0,minSVector(1.0,Rate2))
  Transpiration <- ((1.0 - Rate1) * Rate2 + Rate1 * Rate2 *
                      (1 - (minSVector(1, MoistureVolume1 / MoistureCapacityMax1))^0.667)) * RET
  Transpiration[is.na(Transpiration)] <- 0.0
  MoistureCapacity <- fctMoistureCapacity(MoistureVolume1,
                                          MoistureCapacityMax1 * (paSoilMoistureCapacityB + 1),
                                          paSoilMoistureCapacityB)
  SaturatedArea <- fctSaturatedArea(MoistureCapacity - DBL_EPSILON,
                                    MoistureCapacityMax1 * (paSoilMoistureCapacityB + 1),
                                    paSoilMoistureCapacityB)
  Rate3 <- maxSVector(0.0, minSVector(1.0, SaturatedArea + (1 - SaturatedArea) * 0.3))
  # browser()
  EvaporationSoil <- Rate3 * RET
  EvaporationSoil <- minVector(MoistureVolume1, EvaporationSoil)
  Transpiration <- minVector(MoistureVolume1 - EvaporationSoil, Transpiration)
  return(list(Evatrans = list(EvaporationCanopy = EvaporationCanopy, Transpiration = Transpiration, EvaporationLand = EvaporationSoil)))
}


