#' @title SNOWMelt
#' @description Calculate snow accumulation and melt using an energy balance approach for a two layer snow model
#' @importFrom stats runif
#' @importFrom  HMtools mergeData deleteData viewArgum checkData leftjoinData
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWIntercept(runMode = "VIEW") view the structure
#' @param Options Optionslist, alle the options for model is TURE seted as default,
#' and use SNOWMelt(runMode = "VIEW") view the options structure and set the options
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use SNOWIntercept(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOWMelt <- function(InData, Param, Options, runMode = "RUN", viewGN = 3) {
  ## "VIEW" and "CHECK" mode ####
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOWMelt"
    SurfWater <- runif(viewGN, 0, 0.100)
    PackWater <- runif(viewGN, 0, 0.100)
    Snow <- data.frame(Density = runif(viewGN, 200, 800), Depth = runif(viewGN,0, 800), Coverage = runif(viewGN,0, 1),
                 SWQ = SurfWater + PackWater + runif(viewGN,0, 0.0800), SurfWater = SurfWater, SurfTemp = runif(viewGN, -30, 10),
                 PackTemp = runif(viewGN, -30, 10), PackWater = PackWater)
    Energy <- data.frame(TCanopy = runif(viewGN, -10, 10), TGrnd = runif(viewGN, -10, 10),
                         # VaporizLatentHeat = rep(DBL_EPSILON, viewGN),
                   NetShortSnow = runif(viewGN, 1, 2))
    Aerodyna <- data.frame(AerodynaResistSnow = rep(DBL_EPSILON, viewGN), ReferHeightSnow = rep(DBL_EPSILON, viewGN),
                     RoughSnow = rep(DBL_EPSILON, viewGN), WindSpeedSnow = rep(DBL_EPSILON, viewGN), DisplacSnow = rep(DBL_EPSILON, viewGN))
    SnowFall <- runif(viewGN, 0, 100)
    RainFall <- runif(viewGN, 0, 100)
    Blowing <- runif(viewGN, 0, 10)
    # UNSTABLE_SNOW <- rep(F, 10)
    MetData <- data.frame(TAir = runif(viewGN, -30, 50), AirDensity = rep(DBL_EPSILON, viewGN), VaporPressure = rep(DBL_EPSILON, viewGN),
                          AirPressure = rep(DBL_EPSILON, viewGN),
                    VaporPressDefficit = rep(DBL_EPSILON, viewGN),
                    LongWave = rep(DBL_EPSILON, viewGN))
    InData0 <- list(Snow = Snow, Energy = Energy, Aerodyna = Aerodyna, SnowFall = SnowFall,
                   RainFall = RainFall, Blowing = Blowing,
                   # UNSTABLE_SNOW = UNSTABLE_SNOW,
                   MetData = MetData)
    Param0 <- list(TimeStepSec = 3600,
                   gridN = viewGN,
                   EMISS_SNOW = ParamAll$EMISS_SNOW, HUGE_RESIST = ParamAll$HUGE_RESIST,
                  SNOW_CONDUCT = ParamAll$SNOW_CONDUCT, SNOW_DEPTH_THRES = ParamAll$SNOW_DEPTH_THRES,
                  SNOW_DT = ParamAll$SNOW_DT, SNOW_LIQUID_WATER_CAPACITY = ParamAll$SNOW_LIQUID_WATER_CAPACITY,
                  SVP_A = ParamAll$SVP_A, SVP_B = ParamAll$SVP_B, SVP_C = ParamAll$SVP_C,
                  SNOW_MAX_SURFACE_SWE = ParamAll$SNOW_MAX_SURFACE_SWE, SNOW_MIN_SWQ_EB_THRES = ParamAll$SNOW_MIN_SWQ_EB_THRES)
    Options0 <- list(SPATIAL_SNOW = TRUE)
    Arguments <- list(InData = InData0, Param = Param0, Options = Options0)
    if(runMode == "VIEW"){
      vw <- viewArgum(fcName, Arguments)
      return(list(Arguments = Arguments, Out = vw))
    } else {
      ck <- checkData(Arguments, list(InData = InData, Param = Param, Options = Options), "Arguments")
      return()
    }
  }

  ## "RUN" mode ####
  ## initialize variable ####
  airTemp <- InData$MetData$TAir

  aero_resist <- InData$Aerodyna$AerodynaResistSnow
  z2 <- InData$Aerodyna$ReferHeightSnow
  Z0 <- InData$Aerodyna$RoughSnow
  wind <- InData$Aerodyna$WindSpeedSnow

  Tcanopy <- InData$Energy$TCanopy
  Tgrnd <- InData$Energy$TGrnd

  InData$SnowFall <- InData$SnowFall / MM_PER_M
  InData$RainFall <- InData$RainFall / MM_PER_M
  SnowFall <- InData$SnowFall
  RainFall <- InData$RainFall
  UNSTABLE_SNOW <- F ##?## thin snowpack

  delta_t <- Param$TimeStepSec
  gridN <- Param$gridN
  ## Output
  Energy <- list()
  Snow <- data.frame(SWQ = 0,
                     SurfTemp = 0,
                     PackWater = 0,
                     SurfWater = 0,
                     PackTemp = 0)
  Snow <- leftjoinData(Snow, InData$Snow)
  Snow$old_swq <- Snow$SWQ
  # Precipitation <- rep(0, gridN)
  if (Options$SPATIAL_SNOW) {
    #### make snowpack uniform at mean depth ####
    InData$Snow$Coverage[which(SnowFall > 0)] <- 1
    indexCSC <- which((InData$Snow$Coverage > 0 & InData$Snow$Coverage < 1 & SnowFall == 0))
    #### rain falls evenly over grid cell ####
    # Precipitation[indexCSC] <- (RainFall * (1.0 - InData$Snow$Coverage))[indexCSC] ## vic
    InData$SnowFall[indexCSC] <- (RainFall * (1.0 - InData$Snow$Coverage))[indexCSC] ## lk
    InData$RainFall[indexCSC] <- (RainFall * InData$Snow$Coverage)[indexCSC]
  }

  # InitialSwq <- InData$Snow$SWQ
  InData$Energy$OldTSurf <- InData$Snow$SurfTemp

  #### Initialize snowpack variables ####

  Ice <- InData$Snow$SWQ - InData$Snow$PackWater - InData$Snow$SurfWater

  #### Reconstruct snow pack ####
  judgeIM <- (Ice > Param$SNOW_MAX_SURFACE_SWE)
  SurfaceSwq <- Param$SNOW_MAX_SURFACE_SWE * judgeIM + Ice * (!judgeIM)
  PackSwq <- Ice - SurfaceSwq
  #### Calculate cold contents ####
  SurfaceCC <- CONST_VCPICE_WQ * SurfaceSwq * InData$Snow$SurfTemp
  PackCC <- CONST_VCPICE_WQ * PackSwq * InData$Snow$PackTemp
  SnowFallCC <- CONST_VCPICE_WQ * SnowFall * airTemp * (!(airTemp > 0.0))

  #### Distribute fresh snowfall ####
  judgeSF <- ((SnowFall > (Param$SNOW_MAX_SURFACE_SWE - SurfaceSwq)) &
                ((Param$SNOW_MAX_SURFACE_SWE - SurfaceSwq) > 0))
  DeltaPackSwq <- SurfaceSwq + SnowFall - Param$SNOW_MAX_SURFACE_SWE
  judgeDS <- (DeltaPackSwq > SurfaceSwq)
  DeltaPackCC <- (SurfaceCC + (SnowFall - Param$SNOW_MAX_SURFACE_SWE) / (SnowFall + DBL_EPSILON) * SnowFallCC) * judgeDS +
    (DeltaPackSwq / (SurfaceSwq + DBL_EPSILON) * SurfaceCC) * (!judgeDS)

  SurfaceSwq <- Param$SNOW_MAX_SURFACE_SWE * judgeSF + (SurfaceSwq + SnowFall) * (!judgeSF)
  SurfaceCC <- (SurfaceCC + SnowFallCC - DeltaPackCC) * judgeSF + SnowFallCC * (!judgeSF)
  PackSwq <- (PackSwq + DeltaPackSwq) * judgeSF + PackSwq * (!judgeSF)
  PackCC <- (PackCC + DeltaPackCC) * judgeSF + PackCC * (!judgeSF)
  ##?##
  Snow$SurfTemp <- (SurfaceSwq > 0.0) * (SurfaceCC / (CONST_VCPICE_WQ * SurfaceSwq + DBL_EPSILON))
  Snow$PackTemp <- (PackSwq > 0.0) * (PackCC / (CONST_VCPICE_WQ * PackSwq + DBL_EPSILON))

  #### Adjust ice and Snow$SurfWater ####
  Ice <- Ice + SnowFall
  Snow$SurfWater <- Snow$SurfWater + RainFall
  InData$SurfaceSnowWater <- SurfaceSwq
  #### vic original ####
  # #### Calculate the surface energy balance for snowTemp <- 0.0 ####
  SurfTempTem <- eindim618Vector(calcSurfSnowRestTerm,
                                 (Snow$SurfTemp - Param$SNOW_DT),
                                 (Snow$SurfTemp + Param$SNOW_DT),
                                 0.0,
                                 umkriesn = 20,
                                 InData = InData, Param = Param)

  Snow$SurfTemp <- SurfTempTem
  ##*##
  judgeTE <- (Snow$SurfTemp > -998 & Snow$SurfTemp < 999)
  InData$Snow$SurfTemp <- Snow$SurfTemp
  SPEBOut <- SnowPackEnergyBalance(InData, Param)

  # indexNnT <- which(judgeQNn & judgeTE)
  Qnet <- SPEBOut$RestTerm
  RefreezeEnergy <- SPEBOut$RefreezeEnergy
  DeltaColdContent <- SPEBOut$DeltaColdContent
  GroundFlux <- SPEBOut$GroundFlux
  vapor_flux <- SPEBOut$VaporFlux
  blowing_flux <- SPEBOut$Blowing
  surface_flux <- SPEBOut$SurfaceFlux

  judgeRE <- (RefreezeEnergy >= 0.0)
  RefrozenWater <- (RefreezeEnergy / (CONST_LATICE * CONST_RHOFW) * delta_t) * judgeRE
  judgeRW <- (RefrozenWater > Snow$SurfWater)
  indexEW <- which(judgeRE & judgeRW)
  indexRE <- which(judgeRE)
  RefrozenWater[indexEW] <- Snow$SurfWater[indexEW]
  RefreezeEnergy[indexEW] <- (RefrozenWater * CONST_LATICE * CONST_RHOFW / (delta_t))[indexEW]
  SurfaceSwq[indexRE] <- (SurfaceSwq + RefrozenWater)[indexRE]
  Ice[indexRE] <- (Ice + RefrozenWater)[indexRE]
  Snow$SurfWater[indexRE] <- (Snow$SurfWater - RefrozenWater)[indexRE]
  Snow$SurfWater[which(judgeRE & (Snow$SurfWater < 0.0))] <- 0.0

  SnowMelt <- ((abs(RefreezeEnergy) / (CONST_LATICE * CONST_RHOFW) * delta_t) * (!judgeRE))
  #### Adjust Snow$SurfWater for vapor_flux ####
  judgeVP <- (Snow$SurfWater < -(vapor_flux))
  indexVP <- which(judgeVP)
  blowing_flux[indexVP] <- (blowing_flux * -(Snow$SurfWater / (vapor_flux + DBL_EPSILON)))[indexVP]
  vapor_flux[indexVP] <- (-(Snow$SurfWater))[indexVP]
  surface_flux[indexVP] <- (-(Snow$SurfWater) - blowing_flux)[indexVP]

  Snow$SurfWater <- ((Snow$SurfWater + vapor_flux) * (!judgeVP))

  #### If SnowMelt < Ice, there was incomplete melting of the pack ####
  #### Else, SnowMelt > Ice and there was complete melting of the pack ####
  judgeMI <- (SnowMelt < Ice)
  judgeMIn <- (!(SnowMelt < Ice))
  judgeMP <- (SnowMelt <= PackSwq)
  SnowMelt <- (SnowMelt * judgeMI + Ice * (!judgeMI))
  Ice <- (Ice - SnowMelt) * judgeMI

  Snow$SurfWater <- ((Snow$SurfWater + SnowMelt) * (judgeMI & judgeMP) +
                                (Snow$SurfWater + SnowMelt + Snow$PackWater) * (judgeMI & (!judgeMP)) +
                                (Snow$SurfWater + Ice) * (!judgeMI))
  Snow$PackWater <- (Snow$PackWater * (!(judgeMI & (!judgeMP))))
  SurfaceSwq <- (SurfaceSwq * (judgeMI & judgeMP) + Ice * (judgeMI & (!judgeMP)))
  PackSwq <- ((PackSwq - SnowMelt) * (judgeMI & judgeMP))


  #### lk ####
  #### Done with iteration etc, now Update the liquid water content of the surface layer ####
  MaxLiquidWater <- Param$SNOW_LIQUID_WATER_CAPACITY * SurfaceSwq
  judgeSW <- (Snow$SurfWater > MaxLiquidWater)
  Melt <- (Snow$SurfWater - MaxLiquidWater) * judgeSW
  Snow$SurfWater <- MaxLiquidWater * judgeSW + Snow$SurfWater * (!judgeSW)

  #### Refreeze liquid water in the pack.
  # variable 'RefreezeEnergy' is the heat released to the snow pack
  # if all liquid water were refrozen.
  # if RefreezeEnergy < PackCC then all water IS refrozen
  # PackCC always <=0.0
  #
  # WORK IN PROGRESS: This energy is NOT added to MeltEnergy, since this does
  # not involve energy transported to the pixel.  Instead heat from the snow
  # pack is used to refreeze water ####

  Snow$PackWater <- Snow$PackWater + Melt #### add surface layer outflow to pack liquid water ####
  PackRefreezeEnergy <- Snow$PackWater * CONST_LATICE * CONST_RHOFW

  #### calculate energy released to freeze ####

  judgePC <- (PackCC < -PackRefreezeEnergy)
  DeltaPackSwq = -PackCC / (CONST_LATICE * CONST_RHOFW)
  PackSwq <- (PackSwq + Snow$PackWater) * judgePC + (PackSwq + DeltaPackSwq) * (!judgePC)
  Ice <- (Ice + Snow$PackWater) * judgePC + (Ice + DeltaPackSwq) * (!judgePC)
  Snow$PackWater <- (Snow$PackWater - DeltaPackSwq) * (!judgePC)
  judgeSC <- (judgePC & (PackSwq > 0.0))
  Snow$PackTemp <- maxSVector(0.0, PackCC / (CONST_VCPICE_WQ * PackSwq + DBL_EPSILON)) * judgeSC
  PackCC[which(judgeSC)] <- (PackSwq * CONST_VCPICE_WQ * Snow$PackTemp +
                               PackRefreezeEnergy)[which(judgeSC)]

  #### Update the liquid water content of the pack ####

  MaxLiquidWater <- Param$SNOW_LIQUID_WATER_CAPACITY * PackSwq
  judgePW <- (Snow$PackWater > MaxLiquidWater)
  Melt <- (Snow$PackWater - MaxLiquidWater) * judgePW
  Snow$PackWater[which(judgePW)] <- MaxLiquidWater[which(judgePW)]

  #### Update snow properties ####
  Ice <- PackSwq + SurfaceSwq

  judgeIC <- (Ice > Param$SNOW_MAX_SURFACE_SWE)
  judgeSS <- (SurfaceSwq > Param$SNOW_MAX_SURFACE_SWE)

  PackCC <- (CONST_VCPICE_WQ * Snow$PackTemp * PackSwq) * judgeIC
  SurfaceCC <- (CONST_VCPICE_WQ * Snow$SurfTemp * SurfaceSwq) * judgeIC + SurfaceCC * (!judgeIC)
  PackCC<- (PackCC + SurfaceCC * (SurfaceSwq - Param$SNOW_MAX_SURFACE_SWE) / (SurfaceSwq + DBL_EPSILON)) * (judgeIC & judgeSS) +
    (PackCC - PackCC * (Param$SNOW_MAX_SURFACE_SWE - SurfaceSwq) / (PackSwq + DBL_EPSILON)) * (judgeIC & (!judgeSS))
  SurfaceCC <- (SurfaceCC - SurfaceCC * (SurfaceSwq - Param$SNOW_MAX_SURFACE_SWE) / (SurfaceSwq + DBL_EPSILON)) * (judgeIC & judgeSS) +
    (SurfaceCC + PackCC * (Param$SNOW_MAX_SURFACE_SWE - SurfaceSwq) / (PackSwq + DBL_EPSILON)) * (judgeIC & (!judgeSS))

  PackSwq <- (PackSwq + SurfaceSwq - Param$SNOW_MAX_SURFACE_SWE) * (judgeIC & judgeSS) +
    (PackSwq - Param$SNOW_MAX_SURFACE_SWE - SurfaceSwq) * (judgeIC & (!judgeSS))
  SurfaceSwq <- (SurfaceSwq - SurfaceSwq - Param$SNOW_MAX_SURFACE_SWE) * (judgeIC & judgeSS) +
    (SurfaceSwq + Param$SNOW_MAX_SURFACE_SWE - SurfaceSwq) * (judgeIC & (!judgeSS)) +
    SurfaceSwq * (!judgeIC)
  Snow$PackTemp <- (PackCC / (CONST_VCPICE_WQ * PackSwq + DBL_EPSILON)) * judgeIC
  Snow$SurfTemp <- (SurfaceCC / (CONST_VCPICE_WQ * SurfaceSwq + DBL_EPSILON)) * judgeIC + Snow$SurfTemp * (!judgeIC)

  Snow$SWQ <- Ice + Snow$PackWater + Snow$SurfWater
  Snow$SurfTemp[which(Snow$SWQ == 0.0)] <- 0.0
  Snow$PackTemp[which(Snow$SWQ == 0.0)] <- 0.0
  Energy$ColdContent <- SurfaceCC
  Energy$TSnow <- Snow$SurfTemp
  #### Mass balance test ####

  # MassBalanceError <- (InitialSwq - Snow$SWQ) + (RainFall + SnowFall) - Melt + vapor_flux
  #### return ####
  Precipitation <- round((InData$Snow$SWQ + InData$SnowFall + InData$RainFall) - (Snow$SWQ) + vapor_flux, 10)
  Melt <- Melt * MM_PER_M              #### converts back to mm ####
  Precipitation <- Precipitation * MM_PER_M
  return(list(Snow = as.data.frame(Snow),
              Energy = as.data.frame(Energy),
              Melt = Melt,
              Precipitation = Precipitation,
              VaporFlux = vapor_flux,
              SurfaceFlux = surface_flux))

}
