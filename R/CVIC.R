#' @title MODEL
#' @description Model
#' @param TimeVariData Time Vari Data
#' @param TimeInvariData Time invariabel Data
#' @param Param Param
#' @param ... other arguments
#' @return Q
#' @export
MODEL <- function(TimeVariData, TimeInvariData, Param, ...) UseMethod("MODEL", TimeVariData)

#' VIC runoff (OverInfiltrationExcessRunoff and SaturationExcessRunoff)
#' @importFrom purrr map
#' @importFrom methods new
#' @param TimeVariData Time Vari Data
#' @param TimeInvariData Time invariabel Data
#' @param Param Param
#' @param ... other arguments
#' @return Q
#' @export MODEL.CVIC
#' @export
MODEL.CVIC <- function(TimeVariData, TimeInvariData, Param, ...){
  ##1####
  PeriodN <- Param$PeriodN
  GridN <- Param$GridN

  StoreData <- new("t_vari.hm.list")
  ModelData <- new("t_vari.hm.list")
  StoreData$Ground$Runoff <- t_vari.array(0, c(PeriodN, GridN))
  StoreData$Ground$BaseFlow <- t_vari.array(0, c(PeriodN, GridN))

  ModelData <- left_merge(TimeVariData, TimeInvariData)

  SoilData <- ModelData$SoilData


  ZoneDepth <- array(matrix(rep(c(Param$ZoneDepth1,
                                         Param$ZoneDepth2,
                                         Param$ZoneDepth3), GridN),
                                   GridN, 3, byrow = T), dim = c(GridN,3))
  ZoneMoistureCapacityMax = array(c(ZoneDepth[1] * SoilData$Porosity,
                                           ZoneDepth[2] * SoilData$Porosity,
                                           ZoneDepth[3] * SoilData$Porosity),
                                         dim = c(GridN,3))

  TimeInvariData$Ground <- list(ZoneDepth = ZoneDepth,
                                ZoneMoistureCapacityMax = ZoneMoistureCapacityMax)
  ModelData <- left_merge(TimeVariData, TimeInvariData)

  ## Initial boundary ####
  PeriodData <- new('hm.list')
  Ground <- list(ZoneDepth = matrix(rep(c(Param$ZoneDepth1, Param$ZoneDepth2, Param$ZoneDepth3), GridN), 3, GridN, byrow = T),
                 MoistureCapacityMax = (Param$ZoneDepth1 + Param$ZoneDepth2) * SoilData$Porosity,
                 Overflow = array(0.0, c(GridN, 3)))
  Ground$ZoneMoistureVolume <- array(0.0, c(GridN, 3))
  Ground$MoistureVolume = array(Ground$ZoneMoistureVolume[,1])
  PeriodData$Ground <- Ground
  PeriodData$Intercept <- list(Interception = array(0.0, c(GridN)))
  PeriodData$Snow <- list(Volum_mm = array(0.0, c(GridN)))
  PeriodData$Evatrans <- list(EvaporationCanopy = array(0.0, c(GridN)))

  PeriodData <- left_merge(PeriodData, TimeInvariData)
  ## 2 ####
  for (i in 2:PeriodN) {
    PeriodData <- left_merge(PeriodData, TimeVariData[i])
    ## intercept ####
    ICOut <- INTERCEPTION.Gash(PeriodData, Param)
    PeriodData <- left_merge(PeriodData, ICOut)
    ## ET ####
    PeriodData$Ground$MoistureVolume <- PeriodData$Ground$ZoneMoistureVolume[,1]
    PeriodData$Ground$MoistureCapacityMax <- PeriodData$Ground$ZoneMoistureCapacityMax[,1]
    ETOut <- ActualET.Vic(PeriodData, Param)
    PeriodData <- left_merge(PeriodData, ETOut)

    ## precptation divid ####
    PDOut <- PRECDivid(PeriodData, Param)
    PeriodData <- left_merge(PeriodData, PDOut)
    # map(PDOut$Prec, mean)

    ## snow melt ##
    SNOut <- SNOW.Ddf(PeriodData, Param)
    PeriodData <- left_merge(PeriodData, SNOut)
    # map(SNOut$Prec, mean)
    ## base flow ####
    PeriodData$Ground$MoistureVolume <- PeriodData$Ground$ZoneMoistureVolume[,3]
    PeriodData$Ground$MoistureCapacityMax <- PeriodData$Ground$ZoneMoistureCapacityMax[,3]
    BFOut <- BASEFLOW.ARNO(PeriodData, Param)
    PeriodData <- left_merge(PeriodData, BFOut)
    # map(BFOut$Ground, mean)
    ## runoff ####
    # PeriodData$Ground <- mergeList(PeriodData$Ground, SoilData)
    PeriodData$Ground$Depth <- PeriodData$Ground$ZoneDepth[,1]
    IFROut <- InfiltratRat.GreenAmpt(PeriodData)
    PeriodData <- left_merge(PeriodData, IFROut)
    PeriodData$Ground$MoistureCapacityMax <- (Param$SoilMoistureCapacityB + 1) *
      (PeriodData$Ground$ZoneDepth[,1] + PeriodData$Ground$ZoneDepth[,2]) * PeriodData$SoilData$Porosity
    PeriodData$Ground$MoistureVolume <- PeriodData$Ground$ZoneMoistureVolume[,1] + PeriodData$Ground$ZoneMoistureVolume[,2]

    RFOut <- RUNOFF.Vic(PeriodData, Param)
    PeriodData <- left_merge(PeriodData, RFOut)
    # map(RFOut$Ground, mean)
    ## ground water ####

    GWOut <- GROUNDWATER.Vic(PeriodData, Param)
    PeriodData <- left_merge(PeriodData, GWOut)
    PeriodData$Ground$BaseFlow <- PeriodData$Ground$BaseFlow + PeriodData$Ground$Overflow
    ## for end ##
    # browser()
    StoreData[i] <- PeriodData
  }
  Unit_Trans <- t(array(rep(ModelData$GridData$Area / Param$TimeStepSec / 1000, Param$PeriodN),
                        dim = c(Param$GridN, Param$PeriodN)))
  WaterSource <- map(StoreData$Ground, function(mm_m3_s1) mm_m3_s1 * Unit_Trans)
  ## route ####
  G2AimGAll <- ModelData$Route$G2AimGAll
  ModelData$Route$UHParam <- list(list(StreamLength = G2AimGAll[[1]][,3],
                       RiverheadNumber = Param$UPPaList[1],
                       WaveVelocity = Param$UPPaList[4]),
                  list(StreamLength = G2AimGAll[[1]][,3],
                       RiverheadNumber = Param$UPPaList[2],
                       WaveVelocity = Param$UPPaList[5]),
                  list(StreamLength = G2AimGAll[[2]][,3],
                       RiverheadNumber = Param$UPPaList[3],
                       WaveVelocity = Param$UPPaList[6]),
                  list(StreamLength = G2AimGAll[[3]][,3],
                       RiverheadNumber = Param$UPPaList[3],
                       WaveVelocity = Param$UPPaList[7]))

  ModelData$Route$WaterSource <- WaterSource
  MUOut <- makeUHALL(ModelData, Param)
  ModelData$Route <- left_merge(ModelData$Route, MUOut)

  RTOut <- ROUTE.G2RES(ModelData, Param)

  return(RTOut)
}
