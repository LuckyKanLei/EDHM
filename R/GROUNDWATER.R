

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
#' @param InList list of Input data
#' @param ... other Paramater and inputdata
#' @return GROUNDWATER
#' @export
GROUNDWATER <- function(InList, ...) UseMethod("GROUNDWATER", InList)

#' ground water VIC function
#' @references Liang X, Xie Z, Huang M. A new parameterization for surface and groundwater interactions and its impact on water budgets with the variable infiltration capacity[J]. Journal of Geochemical Research, 2003(108):1-17.
#' @param InList 6-list of:
#' \itemize{
#' \item Evapotranspiration
#' \item Interception
#' \item Infiltration
#' \item BaseFlow
#' \item GroundWaterIn
#' \item GridSoilParame
#' }
#' @param PaList 1-list of:
#' \itemize{
#' \item paClappHornbergerB
#' }
#' @param ... other Paramater and inputdata
#' @return INTERCEPTION
#' @export
GROUNDWATER.VIC <- function(InList, PaList, ...){ ## Infiltration = Infiltration - Evapotranspiration

  Evapotranspiration <- InList$Evapotranspiration
  Interception <- InList$Interception
  Infiltration <- InList$Infiltration
  BaseFlow <- InList$BaseFlow
  GroundWaterIn <- InList$GroundWaterIn
  GroundWaterIn <- split(GroundWaterIn, col(GroundWaterIn))
  names(GroundWaterIn) <- c("Volum0", "Volum1", "Volum2","Volum3")
  GridSoilParame <- InList$GridSoilParame

  paClappHornbergerB <- PaList$paClappHornbergerB


  Infiltration0 <- Infiltration
  HydraulicConductivity1_2 <- fctHydraulicConductivity(GroundWaterIn$Volum1 / GridSoilParame$Depth1,
                                                      GridSoilParame$Porosity,
                                                      GridSoilParame$SaturatedHydraulicConductivity,
                                                      paClappHornbergerB)
  HydraulicConductivity2_3 <- fctHydraulicConductivity(GroundWaterIn$Volum2 / GridSoilParame$Depth2,
                                                      GridSoilParame$Porosity,
                                                      GridSoilParame$SaturatedHydraulicConductivity,
                                                      paClappHornbergerB)
  HydraulicDiffusivity1_2 <- fctHydraulicDiffusivity(GroundWaterIn$Volum1 / GridSoilParame$Depth1,
                                                    GridSoilParame$SaturatedSoilSuctionHead,
                                                    GridSoilParame$Porosity,
                                                    GridSoilParame$SaturatedHydraulicConductivity,
                                                    paClappHornbergerB)  # Unit ist depth: mm ##must translate to a time phase
  HydraulicDiffusivity2_3 <- fctHydraulicDiffusivity(GroundWaterIn$Volum2 / GridSoilParame$Depth2,
                                                    GridSoilParame$SaturatedSoilSuctionHead,
                                                    GridSoilParame$Porosity,
                                                    GridSoilParame$SaturatedHydraulicConductivity,
                                                    paClappHornbergerB)  # Unit ist depth: mm ##must translate to a time phase
  InterFlowFlux1 <- HydraulicConductivity1_2 + HydraulicDiffusivity1_2
  InterFlowFlux2 <- HydraulicConductivity2_3 + HydraulicDiffusivity2_3
  InterFlowFlux1[is.na(InterFlowFlux1)] = 0.0
  InterFlowFlux2[is.na(InterFlowFlux2)] = 0.0
  InterFlowFlux1 <- minVector(InterFlowFlux1, GroundWaterIn$Volum1)
  InterFlowFlux2 <- minVector(InterFlowFlux2, GroundWaterIn$Volum2)
  GroundWaterOut <- GroundWaterIn
  GroundWaterOut$Volum0 <- maxSVector(0.0, GroundWaterIn$Volum0 + as.matrix(Interception) -
                                       as.matrix(Evapotranspiration$EvapC))
  GroundWaterOut$Volum1 <- maxSVector(0.0, GroundWaterIn$Volum1 -
                                       as.matrix(Evapotranspiration$EvapS + Evapotranspiration$Transp) +
                                       Infiltration0 - InterFlowFlux1)
  GroundWaterOut$Volum2 <- maxSVector(0.0, GroundWaterIn$Volum2 + InterFlowFlux1 - InterFlowFlux2)
  GroundWaterOut$Volum3 <- maxSVector(0.0, GroundWaterIn$Volum3 + InterFlowFlux2 - BaseFlow)
  GroundWaterOut[is.na(GroundWaterOut)] <- 0.0

  return(as.matrix(as.data.frame(GroundWaterOut)))
  # return(GroundWaterOut)
}








