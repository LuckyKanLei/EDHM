#' @title SNOWIntercept
#' @description Calculates the interception and subsequent release of by the forest canopy
#' using an energy balance approach.
#' @references Determine the maximum snow interception water equivalent.
#' Kobayashi, D., 1986, Snow Accumulation on a Narrow Board,
#' Cold Regions Science and Technology, (13), pp. 239-245. Figure 4.
#' @importFrom stats runif
#' @importFrom  HMtools mergeData deleteData viewArgum checkData
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWIntercept(runMode = "VIEW") view the structure
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use SNOWIntercept(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOWIntercept <- function(InData, Param, runMode = "RUN", viewGN = 3) {
  ## "VIEW" and "CHECK" mode ####
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOWIntercept"
    RainFall <- runif(viewGN, 0, 50)
    SnowFall <- runif(viewGN, 0, 50)
    InterceptRain <- runif(viewGN, 0, 50)
    InterceptSnow <- runif(viewGN, 0, 50)
    EvapCanopy <- runif(viewGN, 0, 50)
    PVegVaporFlux <- runif(viewGN, 0, 50)
    Energy <- data.frame(TFoliage = runif(viewGN, -30, 50), TCanopy = runif(viewGN, -30, 50), TSnow = runif(viewGN, -30, 5),
                         TSurf = runif(viewGN, -30, 50), VaporizLatentHeat = runif(viewGN, 200000, 500000),
                         ShortOverIn = runif(viewGN, 1, 2), LongOverIn = runif(viewGN, 80, 85))
    Snow <- data.frame(Coverage = runif(viewGN, 0, 1))
    Aerodyna <- data.frame(DisplacCanopy = rep(DBL_EPSILON, viewGN), WindSpeedCanopy = rep(DBL_EPSILON, viewGN), AerodynaResistCanopy = rep(DBL_EPSILON, viewGN),
                           ReferHeightCanopy = rep(DBL_EPSILON, viewGN), RoughCanopy = rep(DBL_EPSILON, viewGN))
    VegData <- data.frame(MaxIntercept = rep(DBL_EPSILON, viewGN), Albedo = rep(DBL_EPSILON, viewGN), LAI = rep(DBL_EPSILON, viewGN), IsOverstory = rep(DBL_EPSILON, viewGN))
    MetData <- data.frame(TAir = rep(DBL_EPSILON, viewGN),
                          AirDensity = rep(DBL_EPSILON, viewGN), VaporPressure = rep(DBL_EPSILON, viewGN),
                          AirPressure = rep(DBL_EPSILON, viewGN), VaporPressDefficit = rep(DBL_EPSILON, viewGN))
    InData0 <- list(RainFall = RainFall, SnowFall = SnowFall, PVegVaporFlux = PVegVaporFlux,
                    InterceptRain = InterceptRain, InterceptSnow = InterceptSnow, EvapCanopy = EvapCanopy,
                    Energy = Energy, Snow = Snow, Aerodyna = Aerodyna, VegData = VegData, MetData = MetData)
    Param0 <- list(TimeStepSec = 3600,
                   gridN = viewGN,
                   EMISS_GRND = ParamAll$EMISS_GRND, EMISS_SNOW = ParamAll$EMISS_SNOW,
                   EMISS_VEG = ParamAll$EMISS_VEG,
                   HUGE_RESIST = ParamAll$HUGE_RESIST, SNOW_DT = ParamAll$SNOW_DT,
                   SNOW_LIQUID_WATER_CAPACITY = ParamAll$SNOW_LIQUID_WATER_CAPACITY,
                   SNOW_MIN_SWQ_EB_THRES = ParamAll$SNOW_MIN_SWQ_EB_THRES, SNOW_NEW_SNOW_ALB = ParamAll$SNOW_NEW_SNOW_ALB,
                   SVP_A = ParamAll$SVP_A, SVP_B = ParamAll$SVP_B, SVP_C = ParamAll$SVP_C,
                   VEG_LAI_SNOW_MULTIPLIER = ParamAll$VEG_LAI_SNOW_MULTIPLIER,
                   VEG_MIN_INTERCEPTION_STORAGE = ParamAll$VEG_MIN_INTERCEPTION_STORAGE)
    Arguments <- list(InData = InData0, Param = Param0)
    if(runMode == "VIEW"){
      vw <- viewArgum(fcName, Arguments)
      return(list(Arguments = Arguments, Out = vw))
    } else {
      ck <- checkData(Arguments, list(InData = InData, Param = Param), "Arguments")
      return()
    }
  }

  ## "RUN" mode ####
  ## initialize variable ####
  air_temp <- InData$MetData$TAir

  bare_albedo <- InData$VegData$Albedo
  LAI <- InData$VegData$LAI
  overstory <- InData$VegData$IsOverstory

  Tfoliage <- InData$Energy$TFoliage
  Tcanopy <- InData$Energy$TCanopy
  ShortOverIn <- InData$Energy$ShortOverIn

  Wind <- InData$Aerodyna$WindSpeedCanopy

  Dt <- Param$TimeStepSec
  gridN <- Param$gridN

  ExcessSnowMelt <- PotSnowMelt <- Overload <- InterceptRainFract <- InterceptSnowFract <- rep(0, gridN)

  Energy <- list()

  ####  Convert Units from VIC (mm -> m)  ####
  InData$RainFall <- InData$RainFall / MM_PER_M
  InData$SnowFall <- InData$SnowFall / MM_PER_M
  InData$InterceptRain <- InData$InterceptRain / MM_PER_M
  InData$InterceptSnow <- InData$InterceptSnow / MM_PER_M
  RainFall <- InData$RainFall
  SnowFall <- InData$SnowFall
  InterceptRain <- InData$InterceptRain
  InterceptSnow <- InData$InterceptSnow
  InterceptRainOrg <- InterceptRain

  InData$EvapC <- InData$EvapC / MM_PER_M
  InData$PVegVaporFlux <- InData$PVegVaporFlux / MM_PER_M
  MaxInt <- InData$VegData$MaxIntercept / MM_PER_M

  InitialSnowInt <- InterceptSnow
  OldTfoliage <- Tfoliage

  TempIntStorage <- ReleasedMass <- Drip <- rep(0.0, gridN)


  ####  Determine the maximum snow interception water equivalent.
  # Kobayashi, D., 1986, Snow Accumulation on a Narrow Board,
  # Cold Regions Science and Technology, (13), pp. 239-245.
  # Figure 4.  ####
  Imax1 <- 4.0 * Param$VEG_LAI_SNOW_MULTIPLIER * LAI
  judgeN31 <- (((Tfoliage) < -1.0) & (Tfoliage > -3.0))
  judgeN1 <- ((Tfoliage) > -1.0)
  MaxSnowInt <- (((Tfoliage) * 3.0 / 2.0) + (11.0 / 2.0)) * judgeN31 +
    4.0 * judgeN1 + !(judgeN31 | judgeN1)

  ####  therefore LAI_ratio decreases as temp decreases  ####
  MaxSnowInt <- MaxSnowInt * Param$VEG_LAI_SNOW_MULTIPLIER * LAI

  ####  Calculate snow interception.  ####
  judgeSI <- (MaxSnowInt > 0)
  DeltaSnowInt <- (1 - InterceptSnow / MaxSnowInt) * SnowFall
  judgeMS <- (DeltaSnowInt + InterceptSnow > MaxSnowInt)
  judgeDS <- (DeltaSnowInt < 0.0)
  DeltaSnowInt <- (MaxSnowInt - InterceptSnow) * (judgeSI & judgeMS) +
    DeltaSnowInt * (judgeSI & (! (judgeMS | judgeDS)))

  ####  Reduce the amount of intercepted snow if windy and cold.
  # Ringyo Shikenjo Tokyo, #54, 1952.
  # Bulletin of the Govt. Forest Exp. Station,
  # Govt. Forest Exp. Station, Meguro, Tokyo, Japan.
  # FORSTX 634.9072 R475r #54.
  # Page 146, Figure 10.
  #
  # Reduce the amount of intercepted snow if snowing, windy, and
  # cold (< -3 to -5 C).
  # Schmidt and Troendle 1992 western snow conference paper.  ####
  judgeSW <- (((Tfoliage) < -3.0) & (DeltaSnowInt > 0.0) & (Wind > 1.0))
  judgeID <- (InterceptSnow + DeltaSnowInt > Imax1)
  BlownSnow <- DeltaSnowInt
  BlownSnow <- ((0.2 * Wind - 0.2) * DeltaSnowInt) * judgeSW
  judgeBD <- (BlownSnow >= DeltaSnowInt)
  BlownSnow <- DeltaSnowInt * (judgeSW & judgeBD) + BlownSnow * (judgeSW & (!judgeBD))
  DeltaSnowInt <- (DeltaSnowInt - BlownSnow) * judgeSW + DeltaSnowInt * (!(judgeSW | judgeID))

  ####  pixel depth     ####
  SnowThroughFall <- SnowFall - DeltaSnowInt

  ## Snow in canopy too thin for EB calculations let it fall through
  indexSMin <- which((SnowFall == 0) & (InterceptSnow < Param$SNOW_MIN_SWQ_EB_THRES))
  SnowThroughFall[indexSMin] <- (SnowThroughFall + InterceptSnow)[indexSMin]
  DeltaSnowInt[indexSMin] <- (DeltaSnowInt - InterceptSnow)[indexSMin]

  ####  physical depth  ####
  InterceptSnow <- InterceptSnow + DeltaSnowInt
  judgeEP <- (InterceptSnow < DBL_EPSILON)
  SnowThroughFall <- SnowThroughFall + InterceptSnow * judgeEP
  InterceptSnow <- InterceptSnow * (!judgeEP)

  ####  Calculate amount of rain intercepted on branches and stored in intercepted snow.  ####
  ####  physical depth  ####
  MaxWaterInt <- Param$SNOW_LIQUID_WATER_CAPACITY * InterceptSnow + MaxInt
  judgeIRM <- ((InterceptRain + RainFall) <= MaxWaterInt)
  RainThroughFall <- (InterceptRain + RainFall - MaxWaterInt) * (!judgeIRM)
  InterceptRain <- (InterceptRain + RainFall) * judgeIRM + MaxWaterInt * (!judgeIRM)
  ## Liquid water in canopy too thin for EB calculations let it fall through
  judgeRI <- (RainFall == 0 && InterceptRain < Param$SNOW_MIN_SWQ_EB_THRES)
  RainThroughFall <- (RainThroughFall + InterceptRain) * judgeRI + RainThroughFall * (!judgeRI)
  InterceptRain <- InterceptRain * (!judgeRI)
  ####  at this point we have calculated the amount of snowfall intercepted and
  # the amount of rainfall intercepted.  These values have been
  # appropriately subtracted from SnowFall and RainFall to determine
  # SnowThroughfall and RainThroughfall.  However, we can end up with the
  # condition that the total intercepted rain plus intercepted snow is
  # greater than the maximum bearing capacity of the tree regardless of air
  # temp (Imax1).  The following routine will adjust InterceptRain and InterceptSnow
  # by triggering mass release due to overloading.  Of course since InterceptRain
  # and InterceptSnow are mixed, we need to slough them of as fixed fractions   ####
  indexImax <- which(InterceptRain + InterceptSnow > Imax1)
  Overload[indexImax] <- ((InterceptSnow + InterceptRain) - Imax1)[indexImax]
  InterceptRainFract[indexImax] <- (InterceptRain / (InterceptRain + InterceptSnow))[indexImax]
  InterceptSnowFract[indexImax] <- (InterceptSnow / (InterceptRain + InterceptSnow))[indexImax]
  InterceptRain[indexImax] <- (InterceptRain - Overload * InterceptRainFract)[indexImax]
  InterceptSnow[indexImax] <- (InterceptSnow - Overload * InterceptSnowFract)[indexImax]
  RainThroughFall[indexImax] <- (RainThroughFall + (Overload * InterceptRainFract))[indexImax]
  SnowThroughFall[indexImax] <- (SnowThroughFall + (Overload * InterceptSnowFract))[indexImax]

  ## If we've lost all intercepted moisture, we've essentially lost the thermal
  ## mass of the canopy and Tfoliage should be equal to Tcanopy
  indexRSEP <- which(InterceptRain + InterceptSnow < DBL_EPSILON)
  Tfoliage[indexRSEP] <- Tcanopy[indexRSEP]

  ####  Calculate the net radiation at the canopy surface, using the canopy
  # temperature.  The outgoing longwave is subtracted twice, because the
  # canopy radiates in two directions  ####
  Tupper <- Tlower <- rep(MISSING, gridN)
  judgeSS <- ((InterceptSnow > 0) | (SnowFall > 0))
  ####  Snow present or accumulating in the canopy  ####

  ##*## Canopy Energy Balance ####
  AlbedoOver <- Param$SNOW_NEW_SNOW_ALB * judgeSS + bare_albedo * (!judgeSS) ## albedo of intercepted snow in canopy
  NetShortOver <- (1. - AlbedoOver) * ShortOverIn ## net SW in canopy
  InData$Energy$NetShortOver <- NetShortOver
  # InData$Energy$LongOverIn
  # InData$Energy$TGrnd <- InData$Energy$TSurf
  LongSnowOut <- calc_outgoing_longwave(InData$Energy$TSnow, Param$EMISS_SNOW)
  LongBareOut <- calc_outgoing_longwave(InData$Energy$TSurf, Param$EMISS_GRND)
  InData$Energy$LongUnderOut <- LongSnowOut * InData$Snow$Coverage + LongBareOut * (1 - InData$Snow$Coverage)

  ##*## lk TFoliage ####
  Tupper <- (Tfoliage + Param$SNOW_DT)
  Tlower <- (Tfoliage - Param$SNOW_DT)

  TfoliageTem <- eindim618Vector(calcCanopyRestTerm,
                                 Tlower, Tupper,
                                 0.0,
                                 umkriesn = 20,
                                 InData = InData, Param = Param)
  Tfoliage <- TfoliageTem
  Tfoliage[which((Tfoliage <= -998))] <- OldTfoliage[which((Tfoliage <= -998))]

  InData$Energy$TFoliage <- Tfoliage
  CEBOut <- CanopyEnergyBalance(InData, Param)
  Qnet <- CEBOut$RestTerm
  RefreezeEnergy <- CEBOut$RefreezeEnergy
  LongOverOut <- CEBOut$LongOverOut
  Energy$LongOverOut <- Energy$LongUnderIn <- LongOverOut
  VaporMassFlux <- CEBOut$VegVaporFlux

  RefreezeEnergy <- RefreezeEnergy * Dt

  ####  if RefreezeEnergy is positive it means energy is available to melt the
  # intercepted snow in the canopy.  If it is negative, it means that
  # intercepted water will be refrozen  ####
  ####  Update maximum water interception storage  ####
  MaxWaterInt <- Param$SNOW_LIQUID_WATER_CAPACITY * (InterceptSnow) + MaxInt

  ####  Convert the vapor mass flux from a flux to a depth per interval  ####
  VaporMassFlux <- VaporMassFlux * Dt
  judgeTf <- (Tfoliage == 0)
  indexTf <- which(judgeTf)
  indexTfn <- which(!judgeTf)
  judgeVR <- (-(VaporMassFlux) > InterceptRain)
  judgeRE <- (RefreezeEnergy < 0)
  judgeVMin <- ((SnowThroughFall > 0.0) & (InitialSnowInt <= Param$VEG_MIN_INTERCEPTION_STORAGE))
  judgeIR <- (InterceptRain > MaxWaterInt)
  indexTfIR <- which(judgeTf & judgeIR)
  VaporMassFlux[which(judgeTf & judgeVR)] <- -(InterceptRain)[which(judgeTf & judgeVR)]
  InterceptRain[indexTf] <- ((InterceptRain + VaporMassFlux) * (!judgeVR))[indexTf]
  PotSnowMelt[indexTf] <- ((minVector((-RefreezeEnergy / CONST_LATICE / CONST_RHOFW), InterceptSnow)) * judgeRE)[indexTf]
  judgeMI <- ((InterceptRain + PotSnowMelt) <= MaxWaterInt)
  indexTfMIn <- which(judgeTf & !judgeMI)
  indexTMnV <- which(judgeTf & !judgeMI & judgeVMin)

  PotSnowMelt[which(judgeTf & judgeMI)] <- 0.0

  InterceptSnow[indexTf] <- ((InterceptSnow - PotSnowMelt) * judgeMI +
                               (InterceptSnow - MaxWaterInt - InterceptRain) * (!judgeMI))[indexTf]
  InterceptRain[indexTf] <- ((InterceptRain + PotSnowMelt) * judgeMI +
                               MaxWaterInt * (!judgeMI))[indexTf]


  ExcessSnowMelt[indexTfMIn] <- (PotSnowMelt + InterceptRain - MaxWaterInt)[indexTfMIn]
  InterceptSnow[indexTfMIn] <- maxSVector(0.0, InterceptSnow[indexTfMIn])
  ####  Water in excess of MaxWaterInt has been generated.  If it is
  # snowing and there was little intercepted snow at the beginning of the time step ( <= MIN_INTERCEPTION_STORAGE),
  # then allow the snow to melt as it is intercepted  ####
  Drip[indexTMnV] <- (Drip + ExcessSnowMelt)[indexTMnV]
  InterceptSnow[indexTMnV] <- (InterceptSnow - ExcessSnowMelt)[indexTMnV]
  InterceptSnow[indexTMnV] <- maxSVector(0.0, InterceptSnow[indexTMnV])

  ####  Else, SnowThroughFall <- 0.0 or SnowThroughFall > 0.0 and there is a
  # substantial amount of intercepted snow at the beginning of the time
  # step ( > MIN_INTERCEPTION_STORAGE).  Snow melt may generate mass release.  ####
  TempIntStorage[which(judgeTf & !judgeMI & !judgeVMin)] <- (TempIntStorage +
                                                               ExcessSnowMelt)[which(judgeTf & !judgeMI & !judgeVMin)]
  MReleasTem <- MassRelease(InterceptSnow, TempIntStorage, ReleasedMass, Drip, Param$VEG_MIN_INTERCEPTION_STORAGE)
  ReleasedMass[indexTfMIn] <- MReleasTem$ReleasedMass[indexTfMIn]
  Drip[indexTfMIn] <- MReleasTem$Drip[indexTfMIn]
  ####  If intercepted snow has melted, add the water it held to drip  ####
  MaxWaterInt[indexTf] <- (Param$SNOW_LIQUID_WATER_CAPACITY * InterceptSnow + MaxInt)[indexTf]
  Drip[indexTfIR] <- (Drip + InterceptRain - MaxWaterInt)[indexTfIR]
  InterceptRain[indexTfIR] <- MaxWaterInt[indexTfIR]

  ####  Reset TempIntStorage to 0.0 when energy balance is negative  ####
  TempIntStorage[indexTfn] <- 0.0
  judgeREI <- (-RefreezeEnergy > -InterceptRain * CONST_LATICE * CONST_RHOFW)
  judgeVI <- (-VaporMassFlux > InterceptSnow)
  ####  Refreeze as much surface water as you can  ####
  ####  All of the water in the surface layer has been frozen.  ####
  ####  Energy released by freezing of intercepted water is added to the MeltEnergy  ####
  InterceptSnow[indexTfn] <- ((InterceptSnow + abs(RefreezeEnergy) /
                                 CONST_LATICE / CONST_RHOFW) * judgeREI +
                                (InterceptSnow + InterceptRain) * (!judgeREI))[indexTfn]
  InterceptRain[indexTfn] <- ((InterceptRain - abs(RefreezeEnergy) / CONST_LATICE / CONST_RHOFW) * judgeREI)[indexTfn]
  RefreezeEnergy[which(!judgeTf & judgeREI)] <- 0.0
  VaporMassFlux[which(!judgeTf & judgeVI)] <- -InterceptSnow[which(!judgeTf & judgeVI)]
  InterceptSnow[indexTfn] <- ((InterceptSnow + VaporMassFlux) * (!judgeVI))[indexTfn]
  ## if snow has melted, make sure canopy is not retaining extra water
  judgeIRa <- ((InterceptSnow == 0) & (InterceptRain > MaxInt))
  indexIRa <- which(judgeIRa)
  RainThroughFall[indexIRa] <- (RainThroughFall + InterceptRain - MaxInt)[indexIRa]
  InterceptRain[indexIRa] <- MaxInt[indexIRa]
  ####  Calculate intercepted H2O balance.  ####
  RainFall <- RainThroughFall + Drip
  SnowFall <- SnowThroughFall + ReleasedMass

  #### If No Overstory, Empty Vegetation of Stored Water ####
  indexWD <- which(!overstory & (InData$SnowFall > 0. & InterceptRain > 0.))
  RainFall[indexWD] <- (RainFall + InData$InterceptRain)[indexWD]
  InData$InterceptRain[indexWD] <- 0.

  ####  Convert Units to VIC (m -> mm)  ####
  VaporMassFlux <- VaporMassFlux * -MM_PER_M
  RainFall <- RainFall * MM_PER_M
  SnowFall <- SnowFall * MM_PER_M
  InterceptRain <- InterceptRain * MM_PER_M
  InterceptSnow <- InterceptSnow * MM_PER_M
  Tfoliage[which(!overstory)] <- air_temp[which(!overstory)]
  Energy$TFoliage <- Tfoliage
  return(list(InterceptSnow = InterceptSnow,
              InterceptRain = InterceptRain,
              VegVaporFlux = VaporMassFlux,
              RainFall = RainFall,
              SnowFall = SnowFall,
              Energy = as.data.frame(Energy)))

}
