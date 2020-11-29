#' @title SNOWAerodyna
#' @description     Calculate the aerodynamic resistance for each vegetation layer,
#' and the wind 2m above the layer boundary.  In case of an overstory,
#' also calculate the wind in the overstory. The values are
#' normalized based on a reference height wind speed, WindUref, of 1 m/s.
#' To get wind speeds and aerodynamic resistances for other values of
#' WindUref, you need to multiply the here calculated wind speeds by WindUref
#' and divide the here calculated aerodynamic resistances by WindUref.
#' @importFrom stats runif
#' @param InData indata list, use SNOWAerodyna(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWAerodyna(runMode = "VIEW") view the structure
#' @return use Aerodyna.SNOWVIC(runMode = "VIEW") view the outputs and theirs structure
#' @export
Aerodyna.SNOWVIC <- function(InData, Param) {
  # if(runMode == "VIEW" | runMode == "CHECK"){
  #   fcName <- "Aerodyna.SNOWVIC"
  #   MetData <- data.frame(WindSpeed = runif(viewGN, 0, 90), WindSpeed = runif(viewGN, 1, 90))
  #   MetData <- putUnit(MetData, c("m/s", "m"))
  #   VegData <- data.frame(IsOverstory = rep(T, viewGN),
  #                         # VegHeight = runif(viewGN, 0.5, 20),
  #                         TrunkRatio = runif(viewGN, 0.1, 0.9),
  #                         Displacement = rep(0., viewGN),
  #                         # RefHeight = rep(0., viewGN),
  #                         Roughness = rep(0., viewGN),
  #                         WindAttenuation = rep(0., viewGN))
  #   SoilData <- data.frame(Roughness = rep(0., viewGN))
  #   LandData <- data.frame(SnowRough = rep(0., viewGN))
  #   InData0 <- list(MetData = MetData, VegData = VegData, SoilData = SoilData, LandData = LandData)
  #   Param0 <- list(HUGE_RESIST = ParamAll$HUGE_RESIST, VEG_RATIO_DH_HEIGHT = ParamAll$VEG_RATIO_DH_HEIGHT)
  #   Arguments <- list(InData = InData0, Param = Param0)
  #
  #   if(runMode == "VIEW"){
  #     vw <- viewArgum(fcName, Arguments)
  #     return(list(Arguments = Arguments, Out = vw))
  #   } else {
  #     ck <- checkData(Arguments, list(InData = InData, Param = Param), "Arguments")
  #     return()
  #   }
  # }
  ISLIST <- length(dim(InData$MetData$WindSpeed)) > 1
  OverStory <- as.logical(InData$VegData$IsOverstory) #### overstory flag
  # Height <- InData$VegData$VegHeight #### vegetation height
  Trunk <- InData$VegData$TrunkRatio #### trunk ratio parameter
  displacement0 <- InData$VegData$Displacement #### vegetation displacement
  # ref_height0 <- InData$VegData$RefHeight #### vegetation reference height
  roughness0 <- InData$VegData$Roughness #### vegetation roughness
  windAttenuation <- InData$VegData$WindAttenuation #### wind attenuation parameter

  tmp_wind <- InData$MetData$WindSpeed #### adjusted wind speed
  wind_h <- InData$MetData$WindH
  Z0_SNOW <- InData$LandData$SnowRough ##?## snow roughness
  Z0_SOIL <- InData$SoilData$Roughness ##?## soil roughness
  K2 <- CONST_KARMAN * CONST_KARMAN
  ## Estimate reference height
  judgeDWs <- (displacement0 < wind_h)
  ref_height0 <- wind_h * judgeDWs + (displacement0 + wind_h + roughness0) * !judgeDWs
  ## Estimate vegetation height
  Height <- displacement0 / Param$VEG_RATIO_DH_HEIGHT

  #### No OverStory, thus maximum one soil layer ####
  Z0_Upper <- roughness0
  d_Upper <- displacement0
  Z0_Lower <- Z0_SOIL * OverStory + roughness0 * (!OverStory)
  d_Lower <- displacement0 * (!OverStory)
  Zw <- 1.5 * Height - 0.5 * d_Upper
  Zt <- maxVector(Trunk * Height, Z0_Lower)
  # browser()
  WindU0 <- (log((2. + Z0_Upper) / Z0_Upper) / log((ref_height0 - d_Upper) / Z0_Upper)) * OverStory +
    (log((2. + Z0_Lower) / Z0_Lower) / log((ref_height0 - d_Lower) / Z0_Lower)) * (!OverStory)
  Ra0 <- (log((2. + (1.0 / 0.63 - 1.0) * d_Upper) / Z0_Upper) *
            log((2. + (1.0 / 0.63 - 1.0) * d_Upper) / (0.1 * Z0_Upper)) / K2) * OverStory +
    (log((2. + (1.0 / 0.63 - 1.0) * d_Lower) / Z0_Lower) *
       log((2. + (1.0 / 0.63 - 1.0) * d_Lower) / (0.1 * Z0_Lower)) / K2) * (!OverStory)

  Ra1 <- (log((ref_height0 - d_Upper) / Z0_Upper) / K2 *
            (Height / (windAttenuation * (Zw - d_Upper)) *
               (exp(windAttenuation * (1 - (d_Upper + Z0_Upper) / Height)) - 1) +
               (Zw - Height) / (Zw - d_Upper) +
               log((ref_height0 - d_Upper) / (Zw - d_Upper)))) * OverStory +
    Ra0 * (!OverStory)

  #### Wind at different levels in the profile ####
  WindUw <- log((Zw - d_Upper) / Z0_Upper) / log(
    (ref_height0 - d_Upper) / Z0_Upper)
  WindUh <- WindUw - (1 - (Height - d_Upper) / (Zw - d_Upper)) /
    log((ref_height0 - d_Upper) / Z0_Upper)
  WindUt <- WindUh * exp(windAttenuation * (Zt / Height - 1.))
  WindU1 <- (WindUh * exp(windAttenuation * ((Z0_Upper + d_Upper) / Height - 1.))) * OverStory +
    WindU0 * (!OverStory)
  #### case 1: the wind profile to a height of 2m above the lower boundary is
  # entirely logarithmic ####
  judgeZ2 <- (Zt > (2. + Z0_SNOW) & OverStory)
  #### case 2: the wind profile to a height of 2m above the lower boundary
  # is part logarithmic and part exponential, but the top of the overstory
  # is more than 2 m above the lower boundary ####
  judgeH2 <- (Height > (2. + Z0_SNOW) & OverStory)
  #### case 3: the top of the overstory is less than 2 m above the lower
  # boundary.  The wind profile above the lower boundary is part
  # logarithmic and part exponential, but only extends to the top of the
  # overstory ####
  judegeEL <- (!((Zt > (2. + Z0_SNOW)) | (Height > (2. + Z0_SNOW))) & OverStory)
  if(sum(judegeEL) >= 1) warning("Top of overstory is less than 2 meters above the lower boundary")
  WindU2 <- (WindUt * log((2. + Z0_SNOW) / Z0_SNOW) / log(Zt / Z0_SNOW)) * judgeZ2 +
    (WindUh * exp(windAttenuation * ((2. + Z0_SNOW) / Height - 1.))) * judgeH2 +
    (WindUh) * judegeEL +
    (log((2. + Z0_SNOW) / Z0_SNOW) / log(ref_height0 / Z0_SNOW)) * (!OverStory)
  Ra2 <- (log((2. + Z0_SNOW) / Z0_SNOW) * log(Zt / Z0_SNOW) / (K2 * WindUt)) * judgeZ2 +
    (log(Zt / Z0_SNOW) * log(Zt / Z0_SNOW) /
       (K2 * WindUt) + Height * log((ref_height0 - d_Upper) / Z0_Upper) / (windAttenuation * K2 * (Zw - d_Upper)) *
       (exp(windAttenuation * (1 - Zt / Height)) - exp(windAttenuation * (1 - (Z0_SNOW + 2.) / Height)))) * judgeH2 +
    (log(Zt / Z0_SNOW) * log(Zt / Z0_SNOW) / (K2 * WindUt) + Height *
       log((ref_height0 - d_Upper) / Z0_Upper) / (windAttenuation * K2 * (Zw - d_Upper)) *
       (exp(windAttenuation * (1 - Zt / Height)) - 1)) * judegeEL +
    (log((2. + Z0_SNOW) / Z0_SNOW) * log(ref_height0 / Z0_SNOW) / K2) * (!OverStory)


  ref_height1 <- ref_height0
  roughness1 <- roughness0
  displacement1 <- displacement0
  ref_height0[which(OverStory)] <- 2.
  roughness0[which(OverStory)] <- Z0_Lower[which(OverStory)]
  displacement0[which(OverStory)] <- d_Lower[which(OverStory)]

  ####* Set aerodynamic resistance terms for snow ####
  ref_height2 <- 2. + Z0_SNOW
  roughness2 <- Z0_SNOW
  displacement2 <- 0. * displacement0

  judgeW0 <- (tmp_wind > 0.)
  judgeWU1 <- (WindU1 != -999)
  judgeWU2 <- (WindU2 != -999)
  WindU0 <- WindU0 * tmp_wind
  WindU1 <- (WindU1 * tmp_wind) * judgeWU1 + WindU1 * (!judgeWU1)
  WindU2 <- (WindU2 * tmp_wind) * judgeWU2 + WindU2 * (!judgeWU2)
  Ra0 <- (Ra0 / tmp_wind) * judgeW0 + Param$HUGE_RESIST * (!judgeW0)
  Ra1 <- (Ra1 / tmp_wind) * (judgeW0 & judgeWU1) + Ra1 * (judgeW0 & (!judgeWU1)) + Param$HUGE_RESIST * (!judgeW0)
  Ra2 <- (Ra2 / tmp_wind) * (judgeW0 & judgeWU2) + Ra2 * (judgeW0 & (!judgeWU2)) + Param$HUGE_RESIST * (!judgeW0)


  Aerodyna <- list(AerodynaResistCanopy = Ra1,
                   AerodynaResistSnow = Ra2,
                   ReferHeightCanopy = ref_height1,
                   ReferHeightSnow = ref_height2,
                   RoughCanopy = roughness1,
                   RoughSnow = roughness2,
                   DisplacCanopy = displacement1,
                   DisplacSnow = displacement2,
                   WindSpeedCanopy = WindU1,
                   WindSpeedSnow = WindU2)
  if(!ISLIST) Aerodyna <- as.data.frame(Aerodyna)
  return(list(Aerodyna = Aerodyna))
}

