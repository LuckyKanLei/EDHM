#' @title SNOWState
#' @description Calculate snow state.
#' @importFrom stats runif
#' @importFrom  HMtools mergeData deleteData viewArgum checkData leftjoinData putUnit
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWIntercept(runMode = "VIEW") view the structure
#' @param Options Optionslist, alle the options for model is TURE seted as default,
#' and use SNOWState(runMode = "VIEW") view the options structure and set the options
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use SNOWIntercept(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOWState <- function(InData, Param, Options, runMode = "RUN", viewGN = 3) {
  ## "VIEW" and "CHECK" mode ####
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOWState"
    Snow <- data.frame(Coverage = runif(viewGN, 0, 1),
                       Density = runif(viewGN, 200, 800),
                       Depth = runif(viewGN, 0, 10),
                       LastSnow = rep(1, viewGN),
                       MELTING = rep(F, viewGN),
                       old_swq = runif(viewGN, 0, 20),
                       store_coverage = runif(viewGN, 0, 1),
                       store_swq = runif(viewGN, 0, 10),
                       SurfTemp = runif(viewGN, -20, 10),
                       SWQ = runif(viewGN, 0, 20))
    Energy <- data.frame(ColdContent = runif(viewGN, 0, 4), TAir = runif(viewGN, -20, 40))
    LandData <- data.frame(MaxSnowDistribSlope = rep(DBL_EPSILON, viewGN))
    SnowFall <- runif(viewGN, 0, 50)
    Melt <- runif(viewGN, 0, 50)
    VaporFlux <- runif(viewGN, 0, 50)
    JDay <- 1
    Latitude <- runif(viewGN, 0, 90)
    SDSOut0 <- snow_density(runMode = "VIEW", viewGN = viewGN)
    NSDOut0 <- new_snow_density(runMode = "VIEW")
    InData00 <- (SDSOut0)$Arguments$InData
    InData0 <- mergeData(InData00, list(Energy = Energy,
                    JDay = JDay,
                    Melt = Melt,
                    Latitude = Latitude,
                    Snow = Snow,
                    LandData = LandData,
                    SnowFall = SnowFall,
                    VaporFlux = VaporFlux))
    Param00 <- mergeData(SDSOut0$Arguments$Param, NSDOut0$Arguments$Param)
    Param0 <- mergeData(Param00, list(TimeStepSec = 3600,
                                      gridN = viewGN,
                                      SNOW_TRACESNOW = ParamAll$SNOW_TRACESNOW))
    Options0 <- list(SPATIAL_SNOW = TRUE)
    Arguments <- list(InData = InData0, Param = Param0, Options = Options0)

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
  air_temp <- InData$Energy$TAir

  day_in_year <- InData$JDay
  snowfall <- InData$SnowFall
  melt <- InData$Melt
  VaporFlux <- InData$VaporFlux

  old_swq <- InData$Snow$old_swq #### store swq for density calculations ####
  old_coverage <- InData$Snow$Coverage

  dt <- Param$TimeStepSec

  Snow <- data.frame(Coverage = 0,
                     Density = 0,
                     Depth = 0,
                     MELTING = 0,
                     store_coverage = 0,
                     store_swq = 0)
  Snow <- leftjoinData(Snow, InData$Snow)
  Snow$store_swq <- InData$Snow$SWQ

  judgeSWb <- (InData$Snow$SWQ > 0.)
  indexSWb <- which(judgeSWb)
  indexSWn <- which(!judgeSWb)
  Snow[indexSWn,] <- 0.
  judgeSTs <- (InData$Snow$SurfTemp <= 0)
  indexSTs <- which(judgeSWb & judgeSTs)
  indexLSe <- which(judgeSWb & !judgeSTs & (InData$Snow$LastSnow == 0))
  #### Calculate Snow Density ####
  #### snowpack present, compress and age density
  Snow$Density[indexSTs] <- (snow_density(InData, Param))[indexSTs]
  #### no snowpack present, start with new snow density
  Snow$Density[indexLSe] <- (new_snow_density(air_temp, Param))[indexLSe]
  #### Calculate Snow Depth (H.B.H. 7.2.1) ####
  old_depth <- Snow$Depth
  Snow$Depth[indexSWb] <- (CONST_RHOFW * InData$Snow$SWQ / Snow$Density)[indexSWb]
  #### Record if snowpack is melting this time step ####
  judgeMEt <- (InData$Energy$ColdContent >= 0 & (
    (InData$Latitude >= 0 & (day_in_year > 60 && #### ~ March 1
                               day_in_year < 273)) | #### ~ October 1
      (InData$Latitude < 0 & (day_in_year < 60 | #### ~ March 1
                                day_in_year > 273)) #### ~ October 1
  ))
  judgeME2 <- (Snow$MELTING & (snowfall > Param$SNOW_TRACESNOW))
  Snow$MELTING <- judgeMEt + Snow$MELTING * (!(judgeMEt | (!judgeMEt & judgeME2)))

  #### Check for Thin Snowpack which only Partially Covers Grid Cell exists only if not snowing and snowpack has started to melt ####
  SCOut <- calc_snow_coverage(snowfall,
                              melt / MM_PER_M + VaporFlux,
                              Snow$store_swq,
                              InData$Snow$SWQ,
                              old_swq,
                              Snow$store_coverage,
                              old_coverage,
                              Snow$Depth,
                              old_depth,
                              InData$LandData$MaxSnowDistribSlope)
  Snow$Coverage <- (Options$SPATIAL_SNOW & judgeSWb) *
    (SCOut$Coverage) + (!Options$SPATIAL_SNOW & judgeSWb)
  Snow$store_coverage <- SCOut$store_coverage
  Snow$store_swq <- SCOut$store_swq
  delta_coverage <- old_coverage - Snow$Coverage
  judgeDCn <- (delta_coverage != 0)
  judgeOCb <- (old_coverage > Snow$Coverage)
  #### returns mixed surface albedo if snow cover fraction has
  # decreased (old_coverage is cover fraction for previous
  #            time step, InData$Snow$Coverage is cover fraction for current
  #            time step. ####
  coverage <- old_coverage * (judgeDCn & judgeOCb) + Snow$Coverage * (judgeDCn & !judgeOCb)
  delta_coverage[which((judgeDCn & !judgeOCb))] <- 0
  indexDO <- which(judgeDCn & judgeOCb)

  #### snow falls and melts all in one time step
  judgeOCCe <- (old_coverage == 0 & Snow$Coverage == 0)
  delta_coverage[which(judgeOCCe)] <- 1.
  coverage[which(judgeOCCe)] <- 0.
  # Snow <- leftjoinData(Snow, InData$Snow)
  Snow$Coverage = coverage
  Snow[indexSWn,1:3] <- 0.

  return(list(Snow = as.data.frame(Snow)))
}
