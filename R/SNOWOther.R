#' @title calc_rainonly
#' @description  Determines from the air temperature what fraction of incoming
#' precipitation is frozen and unfrozen (snow and rain).
#' @param air_temp air_temp
#' @param prec precptation
#' @param MAX_SNOW_TEMP MAX_SNOW_TEMP
#' @param MIN_RAIN_TEMP MIN_RAIN_TEMP
#' @return rainfall only
#' @export
calc_rainonly <- function(air_temp,
              prec,
              MAX_SNOW_TEMP,
              MIN_RAIN_TEMP) {
  if (MAX_SNOW_TEMP <= MIN_RAIN_TEMP) {
    stop("MAX_SNOW_TEMP must be greater then MIN_RAIN_TEMP")
  }
  judgeTMM <- (air_temp < MAX_SNOW_TEMP & air_temp > MIN_RAIN_TEMP)
  judgeMax <- (air_temp >= MAX_SNOW_TEMP)
  rainonly <- ((air_temp - MIN_RAIN_TEMP) / (MAX_SNOW_TEMP - MIN_RAIN_TEMP) * prec) * judgeTMM + prec * judgeMax
  return(rainonly)
}

#' @title SNOWDivid
#' @description  Determines from the air temperature what fraction of incoming
#' precipitation is frozen and unfrozen (snow and rain).
#' @importFrom stats runif
#' @importFrom  HMtools mergeData deleteData viewArgum checkData
#' @param InData indata list, use SNOWDivid(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWDivid(runMode = "VIEW") view the structure
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use SNOWDivid(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOWDivid <- function(InData, Param, runMode = "RUN", viewGN = 3) {
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOWDivid"
    MetData <- data.frame(PrecipitationHoch = runif(viewGN, 0, 90), TAir = runif(viewGN, -30, 50))
    InData0 <- list(MetData = MetData)
    Param0 <- list(SNOW_MAX_SNOW_TEMP = ParamAll$SNOW_MAX_SNOW_TEMP, SNOW_MIN_RAIN_TEMP = ParamAll$SNOW_MIN_RAIN_TEMP)
    Arguments <- list(InData = InData0, Param = Param0)

    if(runMode == "VIEW"){
      vw <- viewArgum(fcName, Arguments)
      return(list(Arguments = Arguments, Out = vw))
    } else {
      ck <- checkData(Arguments, list(InData = InData, Param = Param), "Arguments")
      return()
    }
  }

  prec <- InData$MetData$PrecipitationHoch
  air_temp <- InData$MetData$TAir
  MAX_SNOW_TEMP <- Param$SNOW_MAX_SNOW_TEMP
  MIN_RAIN_TEMP <- Param$SNOW_MIN_RAIN_TEMP
  #### Calculate Fraction of Precipitation that falls as Rain ####
  rainonly <- calc_rainonly(air_temp, prec, MAX_SNOW_TEMP, MIN_RAIN_TEMP)
  snowfall <- prec - rainonly
  snowfall <- snowfall * (!(snowfall < 1e-5))
  rainfall <- prec - snowfall
  return(list(RainFall = rainfall, SnowFall = snowfall))
}

#' @title SNOWAerodyna
#' @description     Calculate the aerodynamic resistance for each vegetation layer,
#' and the wind 2m above the layer boundary.  In case of an overstory,
#' also calculate the wind in the overstory. The values are
#' normalized based on a reference height wind speed, WindUref, of 1 m/s.
#' To get wind speeds and aerodynamic resistances for other values of
#' WindUref, you need to multiply the here calculated wind speeds by WindUref
#' and divide the here calculated aerodynamic resistances by WindUref.
#' @importFrom stats runif
#' @importFrom  HMtools mergeData deleteData viewArgum checkData
#' @param InData indata list, use SNOWAerodyna(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWAerodyna(runMode = "VIEW") view the structure
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use SNOWAerodyna(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOWAerodyna <- function(InData, Param, runMode = "RUN", viewGN = 3) {
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOWAerodyna"
    MetData <- data.frame(WindSpeed = runif(viewGN, 0, 90))
    VegData <- data.frame(IsOverstory = rep(T, viewGN),
                          VegHeight = runif(viewGN, 0.5, 20),
                          TrunkRatio = runif(viewGN, 0.1, 0.9),
                          Displacement = rep(0., viewGN),
                          RefHeight = rep(0., viewGN),
                          Roughness = rep(0., viewGN),
                          WindAttenuation = rep(0., viewGN))
    SoilData <- data.frame(Roughness = rep(0., viewGN))
    LandData <- data.frame(SnowRough = rep(0., viewGN))
    InData0 <- list(MetData = MetData, VegData = VegData, SoilData = SoilData, LandData = LandData)
    Param0 <- list(HUGE_RESIST = ParamAll$HUGE_RESIST)
    Arguments <- list(InData = InData0, Param = Param0)

    if(runMode == "VIEW"){
      vw <- viewArgum(fcName, Arguments)
      return(list(Arguments = Arguments, Out = vw))
    } else {
      ck <- checkData(Arguments, list(InData = InData, Param = Param), "Arguments")
      return()
    }
  }
  ISLIST <- length(dim(InData$MetData$WindSpeed)) > 1
  OverStory <- as.logical(InData$VegData$IsOverstory) #### overstory flag
  Height <- InData$VegData$VegHeight #### vegetation height
  Trunk <- InData$VegData$TrunkRatio #### trunk ratio parameter
  displacement0 <- InData$VegData$Displacement #### vegetation displacement
  ref_height0 <- InData$VegData$RefHeight #### vegetation reference height
  roughness0 <- InData$VegData$Roughness #### vegetation roughness
  windAttenuation <- InData$VegData$WindAttenuation #### wind attenuation parameter

  tmp_wind <- InData$MetData$WindSpeed #### adjusted wind speed

  Z0_SNOW <- InData$LandData$SnowRough ##?## snow roughness
  Z0_SOIL <- InData$SoilData$Roughness ##?## soil roughness
  K2 <- CONST_KARMAN * CONST_KARMAN

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

#' @title snow_density
#' @description Compute the snow density based on swe and snow metamorphism.
#' @references  DENS_SNTHRM <- Algorithm is taken from SNTHERM89, adjusted for an essentially single-layer model.
#' @param InData indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use snow_density(runMode = "VIEW") view the structure
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use snow_density(runMode = "VIEW") view the outputs and theirs structure
#' @export
snow_density <- function(InData, Param, runMode = "RUN", viewGN = 3) {
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "snow_density"
    Snow <- data.frame(Density = runif(viewGN, 200, 800),
                       Depth = runif(viewGN, 0, 50),
                       PackWater = runif(viewGN, 0, 50),
                       SurfTemp = runif(viewGN, -30, 5),
                       SurfWater = runif(viewGN, 0, 50),
                       old_swq = runif(viewGN, 0, 50))
    MetData <- data.frame(TAir = runif(viewGN, -30, 50))

    SnowFall <- runif(viewGN, 0, 50)
    InData0 <- list(MetData = MetData,
                    Snow = Snow,
                    SnowFall = SnowFall)
    NSDOut0 <- new_snow_density(runMode = "VIEW")

    Param00 <- list(MIN_SNOW_WETFRAC = ParamAll$MIN_SNOW_WETFRAC,
                   SNOW_DENS_C1 = ParamAll$SNOW_DENS_C1,
                   SNOW_DENS_C2 = ParamAll$SNOW_DENS_C2,
                   SNOW_DENS_C3 = ParamAll$SNOW_DENS_C3,
                   SNOW_DENS_C3_CONST = ParamAll$SNOW_DENS_C3_CONST,
                   SNOW_DENS_C4 = ParamAll$SNOW_DENS_C4,
                   SNOW_DENS_C4WET = ParamAll$SNOW_DENS_C4WET,
                   SNOW_DENS_C5 = ParamAll$SNOW_DENS_C5,
                   SNOW_DENS_C6 = ParamAll$SNOW_DENS_C6,
                   SNOW_DENS_DMLIMIT = ParamAll$SNOW_DENS_DMLIMIT,
                   SNOW_DENS_DMLIMIT_FACTOR = ParamAll$SNOW_DENS_DMLIMIT_FACTOR,
                   SNOW_DENS_ETA0 = ParamAll$SNOW_DENS_ETA0,
                   SNOW_DENS_F = ParamAll$SNOW_DENS_F)
    Param0 <- c(mergeData(Param00, NSDOut0$Arguments$Param), gridN = viewGN)
    Arguments <- list(InData = InData0, Param = Param0)
    if(runMode == "VIEW"){
      vw <- viewArgum(fcName, Arguments)
      return(list(Arguments = Arguments, Out = vw))
    } else {
      Mess <- checkData(Param0, Param, "Param")
      return()
    }
  }
  snow <- InData$Snow
  new_snow <- InData$SnowFall
  sswq <- snow$old_swq
  Tair <- InData$MetData$TAir
  dt <- Param$TimeStepSec

  #### Estimate density of new snow based on air temperature ####
  density_new <- (new_snow_density(Tair, Param)) * (new_snow > 0.)
  #### Estimate average snowpack temperature ####
  Tavg <- snow$SurfTemp + CONST_TKFRZ

  judgeSDb <- (new_snow > 0.) & !(snow$Depth > 0.0)
  density <- density_new * judgeSDb + snow$Density * (!judgeSDb)
  dexpf <- exp(-Param$SNOW_DENS_C1 * (CONST_TKFRZ - Tavg))

  #### Settling due to destructive metamorphism ####
  judgeDNb <- (new_snow > 0.0 & density_new > 0.0 & (Param$SNOW_DENS_DMLIMIT <=
                                                       Param$SNOW_DENS_DMLIMIT_FACTOR * density_new))
  dm <- (Param$SNOW_DENS_DMLIMIT_FACTOR * density_new) * judgeDNb + Param$SNOW_DENS_DMLIMIT * !judgeDNb


  c3 <- Param$SNOW_DENS_C3 * (density <= dm) +
    exp(Param$SNOW_DENS_C3_CONST * (density - dm)) * !(density <= dm)
  c4 <- rep(Param$SNOW_DENS_C4, Param$gridN)
  #### presence of wet snow ####
  indexC4 <- which((snow$Depth > 0) & ((snow$SurfWater + snow$PackWater) / snow$Depth >
                                         Param$MIN_SNOW_WETFRAC))
  c4[indexC4] <- Param$SNOW_DENS_C4WET
  ddz1 <- -Param$SNOW_DENS_C2 * c3 * c4 * dexpf

  #### Compaction due to overburden ####
  #### swq in this context is the amount of snow whose weight contributes
  #### to compaction
  #### Currently VIC essentially has only one layer of snow, so compaction
  # due to overburden will come mostly from new snowfall. ####
  swq <- new_snow / MM_PER_M + Param$SNOW_DENS_F * sswq
  Ps <- 0.5 * CONST_G * CONST_RHOFW * swq
  ddz2 <- (-Ps / Param$SNOW_DENS_ETA0 *
             exp(-(-Param$SNOW_DENS_C5 *
                     (Tavg - CONST_TKFRZ) + Param$SNOW_DENS_C6 * density))) * (new_snow > 0.0)


  #### Calculate compaction rate and new snow density ####
  CR <- -ddz1 - ddz2
  density <- density * (1. + CR * dt)
  return (density)
}

#' @title new_snow_density
#' @description Estimate the density of new snow
#' @import HMtools
#' @param air_temp air tempteratur
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use new_snow_density(runMode = "VIEW") view the structure
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @return use new_snow_density(runMode = "VIEW") view the outputs and theirs structure
#' @export
new_snow_density <- function(air_temp, Param, runMode = "RUN") {
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "new_snow_density"
    Param0 <- list(SNOW_NEW_SNT_C1 = ParamAll$SNOW_NEW_SNT_C1,
                   SNOW_NEW_SNT_C2 = ParamAll$SNOW_NEW_SNT_C2,
                   SNOW_NEW_SNT_C3 = ParamAll$SNOW_NEW_SNT_C3,
                   SNOW_NEW_SNOW_DENS_MAX = ParamAll$SNOW_NEW_SNOW_DENS_MAX)
    Arguments <- list(Param = Param0)
    if(runMode == "VIEW"){
      cat(paste0(BoundryString, viewNote, BoundryString, "Function ", fcName, " has the following requirements:\n"))
      vw <- viewData(Param0, "Param")
      cat(BoundryString)
      return(list(Arguments = Arguments))
    } else {
      Mess <- checkData(Param0, Param, "Param")
      return()
    }
  }


  #### new snow density based on Hedstrom and Pomeroy (1998)
  density_new <- Param$SNOW_NEW_SNT_C1 + Param$SNOW_NEW_SNT_C2 * exp(
    air_temp / Param$SNOW_NEW_SNT_C3)

  #### cap new snow density to prevent the calculation from
  #### becoming unphysical
  density_new <- minSVector(Param$SNOW_NEW_SNOW_DENS_MAX, density_new)
  # browser()
  return (density_new)
}

#' @title snow_albedo
#' @description This subroutine computes the snow pack surface albedo.'
#' @references Computes albedo as a function of snow age and season, based on the
#' algorithm of the US Army Corps of Engineers.
#' @param dt temstep in seconds
#' @param new_snow new snow
#' @param swq snow water
#' @param cold_content cold content
#' @param last_snow last snow
#' @param MELTING logical, ob melting
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use snow_albedo(runMode = "VIEW") view the structure
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @return use snow_albedo(runMode = "VIEW") view the outputs and theirs structure
#' @export
snow_albedo <- function(dt,
            new_snow,
            swq,
            cold_content,
            last_snow,
            MELTING,
            Param,
            runMode = "RUN") {
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "snow_albedo"
    Param0 <- list(SNOW_ALB_ACCUM_A = ParamAll$SNOW_ALB_ACCUM_A,
                   SNOW_ALB_ACCUM_B = ParamAll$SNOW_ALB_ACCUM_B,
                   SNOW_ALB_THAW_A = ParamAll$SNOW_ALB_THAW_A,
                   SNOW_ALB_THAW_B = ParamAll$SNOW_ALB_THAW_B,
                   SNOW_NEW_SNOW_ALB = ParamAll$SNOW_NEW_SNOW_ALB,
                   SNOW_TRACESNOW = ParamAll$SNOW_TRACESNOW)
    Arguments <- list(Param = Param0)
    if(runMode == "VIEW"){
      cat(paste0(BoundryString, viewNote, BoundryString, "Function ", fcName, " has the following requirements:\n"))
      vw <- viewData(Param0, "Param")
      cat(BoundryString)
      return(list(Arguments = Arguments))
    } else {
      Mess <- checkData(Param0, Param, "Param")
      return()
    }
  }


  ####* New Snow *####
  judgeNS <- (new_snow > Param$SNOW_TRACESNOW & cold_content < 0.0)
  ####* Aged Snow *####
  judgeAS <- !judgeNS & (swq > 0.0)
  #### Accumulation season ####
  #### Melt Season ####
  judgeAC <- (cold_content < 0.0 & !MELTING)

  albedo <- Param$SNOW_NEW_SNOW_ALB * judgeNS +
    (Param$SNOW_NEW_SNOW_ALB *
       Param$SNOW_ALB_ACCUM_A^((last_snow * dt /
                                  SEC_PER_DAY)^Param$SNOW_ALB_ACCUM_B)) * (judgeAS & judgeAC) +
    (Param$SNOW_NEW_SNOW_ALB *
       Param$SNOW_ALB_THAW_A^((last_snow * dt /
                                 SEC_PER_DAY)^Param$SNOW_ALB_THAW_B)) * (judgeAS & !judgeAC)
  return(albedo)
}

#' @title calc_snow_coverage
#' @description  This routine computes the current fraction of the vegetation band that is
#' covered with snow.  The snow distribution is assumed to be uniform with a
#' slope based on the value of max_snow_distrib_slope.  The
#' original value was based on field observations from the University of Minnesota's
#' Rosemount Agricultural Experiment station (see dissertation by Keith
#' Cherkauer, 2001).
#' @param snowfall snowfall
#' @param melt melt
#' @param store_swq store_swq
#' @param swq swq
#' @param old_swq old_swq
#' @param store_coverage store_coverage
#' @param old_coverage old_coverage
#' @param depth snow depth
#' @param old_depth old_depth
#' @param max_snow_distrib_slope max_snow_distrib_slope
#' @return snow coverage
#' @export
calc_snow_coverage <- function(snowfall,
                               melt,
                               store_swq,
                               swq,
                               old_swq,
                               store_coverage,
                               old_coverage,
                               depth,
                               old_depth,
                               max_snow_distrib_slope) {

  snow_distrib_slope <- max_snow_depth <- rep(0., length(snowfall))
  store_snow <- (snowfall > 0)
  #### New snow falls on partial snowpack ####
  coverage <- old_coverage
  judgeSFb <- (snowfall > 0)
  judgeOCs <- (old_coverage < 1)
  #### store coverage fraction before it is buried
  judgeSSWe <- (store_swq == 0 & old_coverage < 1)
  judgeOCn <- (store_swq == 0 & !(old_coverage < 1))
  judgeMSD <- (depth >= max_snow_distrib_slope / 2.)
  indexMSD <- which(judgeSFb & store_snow & judgeMSD)
  indexSS <- which(judgeSFb & store_snow)
  coverage[which(judgeSFb)] <- 1
  store_coverage[indexSS] <- (old_coverage * judgeSSWe + (judgeOCn | judgeMSD))[indexSS]

  #### store snow falling over partial snowpack ####
  store_swq[indexSS] <- (store_swq + swq - old_swq)[indexSS]
  store_swq[indexMSD] <- 0
  store_swq <- (swq - old_swq) * (judgeSFb & judgeOCs) + store_swq * (!(judgeSFb & judgeOCs))
  store_snow[indexMSD] <- 0
  store_snow <- (judgeSFb & judgeOCs) + store_snow * (!(judgeSFb & judgeOCs))
  snow_distrib_slope[indexMSD] <- 0

  #### Snowpack begins to melt or continues melting ####
  judgeME <- !judgeSFb & (melt > 0)
  judgeSWsO <- (store_swq > 0 & swq < old_swq)
  indexSWsO <- which(judgeME & judgeSWsO)
  #### Melt thin snowfall off previous distribution ####
  store_swq[indexSWsO] <- (store_swq + swq - old_swq)[indexSWsO]
  #### Snowpack cover has melted - clear storage ####
  store_swq <- maxSVector(0., store_swq)
  indexSSse <- which(judgeME & judgeSWsO & (store_swq <= 0))
  #### restore buried cover fraction
  old_coverage[indexSSse] <- store_coverage[indexSSse]
  store_coverage[indexSSse] <- 1
  judgeSWe <- (store_swq == 0)
  indexSWe <- which(judgeME & judgeSWe)
  indexSLe <- which(judgeME & judgeSWe & (snow_distrib_slope == 0))
  judgeODS <- (old_depth > max_snow_distrib_slope / 2.)
  snow_distrib_slope[indexSLe] <- ((-max_snow_distrib_slope) * judgeODS + (-2. * old_depth) * (!judgeODS))[indexSLe]
  max_snow_depth[indexSLe] <- (-snow_distrib_slope)[indexSLe]
  store_snow[indexSLe] <- 1
  #### if currently raining, new swq may be higher than previous
  # maximum swq even if melt occurs.  reset maximum swq if this
  # occurs. ####

  old_max_snow_depth <- max_snow_depth
  max_snow_depth[indexSWe] <- 2. * depth[indexSWe]
  #### melt has occured, reduce coverage fraction ####
  judgeMSDO <- (max_snow_depth < old_max_snow_depth | old_max_snow_depth == 0)
  indexMSDO <- which(judgeME & judgeSWe & judgeMSDO)
  coverage[indexMSDO] <- (-(max_snow_depth) / (snow_distrib_slope))[indexMSDO]
  coverage[indexMSDO] <- (minSVector(1, coverage))[indexMSDO]

  #### rain or sublimation has increased swq,
  # coverage fraction does not change ####

  indexSMn <- which((!(judgeSFb | judgeME)) | (judgeME & (!judgeSWe | (judgeSWe & !judgeMSDO))))
  #### no change to snowpack ####
  coverage[indexSMn] <- old_coverage[indexSMn]
  return(list(Coverage = coverage,
              store_coverage = store_coverage,
              store_swq = store_swq))

}

#' @title MassRelease
#' @description Calculates mass release of snow from canopy.
#' @param InterceptedSnow InterceptedSnow
#' @param TempInterceptionStorage TempInterceptionStorage
#' @param ReleasedMass ReleasedMass
#' @param Drip Drip
#' @param VEG_MIN_INTERCEPTION_STORAGE  VEG_MIN_INTERCEPTION_STORAGE
#' @param iterN not klar
#' @return Release Mass
#' @export
MassRelease <- function(InterceptedSnow,
                        TempInterceptionStorage,
                        ReleasedMass,
                        Drip,
                        VEG_MIN_INTERCEPTION_STORAGE,
                        iterN = 0) {
  #### If the amount of snow in the canopy is greater than some minimum
  # value, MIN_INTERCEPTION_STORAGE, then calculte mass release and Drip ####
  Threshold = 0.10 * InterceptedSnow
  MaxRelease = 0.17 * InterceptedSnow
  #### If the amount of snow_melt after interception, snow_melt, is >= the
  # theshhold then there is mass release.  If snow_melt is < the treshhold
  # then there is no mass release but that water remains in
  # * TempInterceptionStorage which will be augmented during the next
  # compute period ####
  judgeIS <- (InterceptedSnow > VEG_MIN_INTERCEPTION_STORAGE)
  judgeTIbe <- judgeIS & (TempInterceptionStorage >= Threshold)
  indexTI <- which(judgeTIbe)
  Drip <- Drip + Threshold
  InterceptedSnow <- InterceptedSnow - Threshold
  TempInterceptionStorage <- TempInterceptionStorage - Threshold

  judgeISs <- judgeTIbe & (InterceptedSnow < VEG_MIN_INTERCEPTION_STORAGE)
  TempReleasedMass <- minVector((InterceptedSnow - VEG_MIN_INTERCEPTION_STORAGE),
                                MaxRelease) * !judgeISs

  TempDrip <- minVector(TempInterceptionStorage, InterceptedSnow)
  Drip <- (Drip + Threshold) * judgeTIbe + (Drip + TempDrip) * !judgeTIbe
  InterceptedSnow <- InterceptedSnow - Threshold
  ReleasedMass <- ReleasedMass + TempReleasedMass
  InterceptedSnow <- InterceptedSnow - TempReleasedMass
  if (any(indexTI) & iterN < 10 ) {
    MROut <- MassRelease(InterceptedSnow[indexTI], TempInterceptionStorage[indexTI], ReleasedMass[indexTI],
                         Drip[indexTI], VEG_MIN_INTERCEPTION_STORAGE, iterN + 1)
    InterceptedSnow[indexTI] <- MROut$InterceptedSnow
    TempInterceptionStorage[indexTI] <- MROut$TempInterceptionStorage
    ReleasedMass[indexTI] <- MROut$ReleasedMass
    Drip[indexTI] <- MROut$Drip
  }
  #### (InterceptedSnow < MIN_INTERCEPTION_STORAGE) If the amount of snow in
  # the canopy is less than some minimum value, MIN_INTERCEPTION_STORAGE,
  # then only melt can occur and there is no mass release. ####
  TempInterceptionStorage[which(!judgeIS)] <- 0.
  return(list(InterceptedSnow = InterceptedSnow,
              TempInterceptionStorage = TempInterceptionStorage,
              ReleasedMass = ReleasedMass,
              Drip = Drip))
}

