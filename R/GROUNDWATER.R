#' soil paramter caculate
#' @param VolumetricSoilMoistureContent Volumetric Soil Moisture Content
#' @param SoilPorosity Soil Porosity
#' @param SaturatedHydraulicConductivity Saturated Hydraulic Conductivity
#' @param paramClappHornbergerB ClappHornberger B
#' @return HydraulicConductivity
#' @export
fctHydraulicConductivity <- function(VolumetricSoilMoistureContent,
                                     SoilPorosity,
                                     SaturatedHydraulicConductivity,
                                     paramClappHornbergerB){
  HydraulicConductivity <- SaturatedHydraulicConductivity *
    (VolumetricSoilMoistureContent / SoilPorosity)^(2 * paramClappHornbergerB + 3)
  HydraulicConductivity[is.nan(HydraulicConductivity)] = 0.0
  return(HydraulicConductivity)
}


#' soil paramter caculate
#' @param VolumetricSoilMoistureContent Soil Moisture Content
#' @param SoilPorosity SoilPorosity
#' @param SaturatedSoilSuctionHead Saturated Soil SuctionHead
#' @param SaturatedHydraulicConductivity Saturated Hydraulic Conductivity
#' @param paramClappHornbergerB param Clapp Hornberger B
#' @return HydraulicDiffusivity
#' @export
fctHydraulicDiffusivity <- function(VolumetricSoilMoistureContent,
                                    SoilPorosity,
                                    SaturatedSoilSuctionHead,
                                    SaturatedHydraulicConductivity,
                                    paramClappHornbergerB){
  return(paramClappHornbergerB * SaturatedSoilSuctionHead *
           SaturatedHydraulicConductivity / SoilPorosity *
           (VolumetricSoilMoistureContent / SoilPorosity)^(paramClappHornbergerB + 2))
}







#' GROUNDWATER caculate
#' @param InData list of Input data
#' @param ... other Paramater and inputdata
#' @return GROUNDWATER
#' @export
GROUNDWATER <- function(InData, ...) UseMethod("GROUNDWATER", InData)

#' ground water VIC function
#' @references Liang X, Xie Z, Huang M. A new parameterization for surface and groundwater interactions and its impact on water budgets with the variable infiltration capacity[J]. Journal of Geochemical Research, 2003(108):1-17.
#' @param InData 6-list of:
#' \itemize{
#' \item Evapotranspiration
#' \item Interception
#' \item Infiltration
#' \item BaseFlow
#' \item GroundWaterIn
#' \item GridSoilParame
#' }
#' @param Param 1-list of:
#' \itemize{
#' \item paClappHornbergerB
#' }
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @param ... other Parmeters
#' @return INTERCEPTION
#' @export GROUNDWATER.VIC
#' @export
GROUNDWATER.VIC <- function(InData, Param, runMode = "RUN", viewGN = 3, ...){ ## Infiltration = Infiltration - Evapotranspiration

  Evapotranspiration <- InData$ET
  Interception <- InData$Intercept$Interception
  Infiltration <- InData$Infilt$Infiltration
  BaseFlow <- InData$Ground$BaseFlow
  GroundWaterIn <- InData$Ground
  # GroundWaterIn <- split(GroundWaterIn, col(GroundWaterIn))
  # names(GroundWaterIn) <- c("Volum0", "Volum1", "Volum2","Volum3")
  # GridSoilParame <- mergeData(InData$Ground, InData$SoilData)

  paClappHornbergerB <- Param$ClappHornbergerB

  Infiltration0 <- Infiltration
  HydraulicConductivity1_2 <- fctHydraulicConductivity(GroundWaterIn$ZoneMoistureVolume[,1] / GroundWaterIn$ZoneDepth[,1],
                                                       GroundWaterIn$Porosity,
                                                       GroundWaterIn$SaturatedHydraulicConductivity,
                                                       paClappHornbergerB)
  HydraulicConductivity2_3 <- fctHydraulicConductivity(GroundWaterIn$ZoneMoistureVolume[,2] / GroundWaterIn$ZoneDepth[,2],
                                                       GroundWaterIn$Porosity,
                                                       GroundWaterIn$SaturatedHydraulicConductivity,
                                                       paClappHornbergerB)
  HydraulicDiffusivity1_2 <- fctHydraulicDiffusivity(GroundWaterIn$ZoneMoistureVolume[,1] / GroundWaterIn$ZoneDepth[,1],
                                                     GroundWaterIn$SaturatedSoilSuctionHead,
                                                     GroundWaterIn$Porosity,
                                                     GroundWaterIn$SaturatedHydraulicConductivity,
                                                     paClappHornbergerB)  # Unit ist depth: mm ##must translate to a time phase
  HydraulicDiffusivity2_3 <- fctHydraulicDiffusivity(GroundWaterIn$ZoneMoistureVolume[,2] / GroundWaterIn$ZoneDepth[,2],
                                                     GroundWaterIn$SaturatedSoilSuctionHead,
                                                     GroundWaterIn$Porosity,
                                                     GroundWaterIn$SaturatedHydraulicConductivity,
                                                     paClappHornbergerB)  # Unit ist depth: mm ##must translate to a time phase
  InterFlowFlux1 <- HydraulicConductivity1_2 + HydraulicDiffusivity1_2
  InterFlowFlux2 <- HydraulicConductivity2_3 + HydraulicDiffusivity2_3
  InterFlowFlux1[is.na(InterFlowFlux1)] = 0.0
  InterFlowFlux2[is.na(InterFlowFlux2)] = 0.0
  InterFlowFlux1 <- minVector(InterFlowFlux1, GroundWaterIn$ZoneMoistureVolume[,1])
  InterFlowFlux2 <- minVector(InterFlowFlux2, GroundWaterIn$ZoneMoistureVolume[,2])
  # GroundWaterOut <- GroundWaterIn
  GroundWaterOut <- list()
  GroundWaterOut$ZoneMoistureVolume <- GroundWaterIn$ZoneMoistureVolume
  # GroundWaterOut$Volum0 <- maxSVector(0.0, GroundWaterIn$Volum0 + as.matrix(Interception) -
  #                                      as.matrix(Evapotranspiration$EvapC))
  GroundWaterOut$ZoneMoistureVolume[,1] <- maxSVector(0.0, GroundWaterIn$ZoneMoistureVolume[,1] -
                                                        as.matrix(Evapotranspiration$EvaporationLand + Evapotranspiration$Transpiration) +
                                                        Infiltration0 - InterFlowFlux1)
  GroundWaterOut$ZoneMoistureVolume[,2] <- maxSVector(0.0, GroundWaterIn$ZoneMoistureVolume[,2] + InterFlowFlux1 - InterFlowFlux2)
  GroundWaterOut$ZoneMoistureVolume[,3] <- maxSVector(0.0, GroundWaterIn$ZoneMoistureVolume[,3] + InterFlowFlux2 - BaseFlow)
  GroundWaterOut[is.na(GroundWaterOut)] <- 0.0

  return(list(Ground = GroundWaterOut))
}








