#' Infiltration caculate
#' @param InList list of Input data
#' @param ... other Paramater and inputdata
#' @return Infiltration
#' @export
fctInfiltration <- function(InList, ...) UseMethod("fctInfiltration", InList)

#' Infiltration with GreenAmpt methond
#' @references Tarboton D G. Rainfall-runoff processes[M]. Utah State University, 2003.
#' @param InList 5-list of:
#' \itemize{
#' \item HydraulicConductivity
#' \item WettingFrontSoilSuction
#' \item SoilMoistureContent
#' \item EffectivePorosity
#' \item SoilMoistureVolume
#' }
#' @param ... other Paramater and inputdata
#' @return Infiltration
#' @export
fctInfiltration.GreenAmpt <- function(InList, ...){
  HydraulicConductivity <- InList$HydraulicConductivity
  WettingFrontSoilSuction <- InList$WettingFrontSoilSuction
  SoilMoistureContent <- InList$SoilMoistureContent
  EffectivePorosity <- InList$EffectivePorosity
  SoilMoistureVolume <- InList$SoilMoistureVolume

  InfiltrationRat <- HydraulicConductivity *
    (1 + WettingFrontSoilSuction * (EffectivePorosity - SoilMoistureContent) / SoilMoistureVolume)
  return(maxSVector(0.0, InfiltrationRat))  ## P > Ks
}



#' soil paramter caculate
#' @param SoilMoistureVolume Soil Moisture Volume
#' @param SoilMoistureCapacityMax Soil Moisture Capacity Max
#' @param paSoilMoistureCapacityB param Soil Moisture Capacity B
#' @return SoilMoistureCapacity
#' @export
fctMoistureCapacity <- function(SoilMoistureVolume, SoilMoistureCapacityMax, paSoilMoistureCapacityB){
  SoilMoistureCapacity <- SoilMoistureCapacityMax *
    (1 - (1 - SoilMoistureVolume * (paSoilMoistureCapacityB + 1) /
            SoilMoistureCapacityMax)^(1 / (paSoilMoistureCapacityB + 1)))  #A is the fraction of an area for which the soil moisture capacity is less than or equal to i
  SoilMoistureCapacity[is.na(SoilMoistureCapacity)] <- (SoilMoistureVolume / SoilMoistureCapacityMax)[is.na(SoilMoistureCapacity)]
  return(SoilMoistureCapacity)
}


#' soil paramter caculate
#' @param SoilMoistureCapacity Soil Moisture Capacity
#' @param SoilMoistureCapacityMax Soil Moisture Capacity Max
#' @param paSoilMoistureCapacityB param Soil Moisture Capacity B
#' @return SaturatedArea
#' @export
fctSaturatedArea <- function(SoilMoistureCapacity, SoilMoistureCapacityMax, paSoilMoistureCapacityB){
  SaturatedArea <- 1.0 - (1 - SoilMoistureCapacity / SoilMoistureCapacityMax)^paSoilMoistureCapacityB
  return(SaturatedArea)
}



#' SoilInfiltration rate
#' @param InList list of Input data
#' @param ... other Paramater and inputdata
#' @return SoilInfiltration
#' @export
fctSoilInfiltration <- function(InList, ...) UseMethod("fctSoilInfiltration", InList)

#' SoilInfiltration rate in Saturation Excess Runoff modell
#' @param InList 3-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureCapacity
#' }
#' @param PaList 1-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' }
#' @param ... other Paramater and inputdata
#' @return SoilInfiltration in SaturationExcessRunoff modell
#' @export
fctSoilInfiltration.SER <- function(InList, PaList, ...){
  PrecipitationHoch <- InList$PrecipitationHoch
  SoilMoistureCapacityMax <- InList$SoilMoistureCapacityMax
  SoilMoistureCapacity <- InList$SoilMoistureCapacity

  paSoilMoistureCapacityB <- PaList$paSoilMoistureCapacityB

  TEMMin <- SoilMoistureCapacityMax / (paSoilMoistureCapacityB + 1) *
    ((1 - SoilMoistureCapacity / SoilMoistureCapacityMax)^(paSoilMoistureCapacityB + 1) -
       (1 - (SoilMoistureCapacity + PrecipitationHoch) /
          SoilMoistureCapacityMax)^(paSoilMoistureCapacityB + 1))
  TEMMax <- SoilMoistureCapacityMax / (paSoilMoistureCapacityB + 1) *
    (1 - SoilMoistureCapacity /
       SoilMoistureCapacityMax)^(paSoilMoistureCapacityB + 1)
  TEMDiff <- PrecipitationHoch - (SoilMoistureCapacityMax - SoilMoistureCapacity)
  TEM <- TEMMin
  TEM[which(TEMDiff > 0.0)] <- TEMMax[which(TEMDiff > 0.0)]
  return(TEM)
}

#' Infiltration rate in Over Infiltration Excess Runoff
#' @param InList 2-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item InfiltrationRateMax
#' }
#' @param PaList 1-list of:
#' \itemize{
#' \item paInfiltrationRateB
#' }
#' @param ... other Paramater and inputdata
#' @return Infiltration rate in Over Infil tration Excess Runoff
#' @export
fctSoilInfiltration.OIER <- function(InList, PaList, ...){
  PrecipitationHoch <- InList$PrecipitationHoch
  InfiltrationRateMax <- InList$InfiltrationRateMax

  paInfiltrationRateB <- PaList$paInfiltrationRateB

  TEMMin <- InfiltrationRateMax / (paInfiltrationRateB + 1) *
    (1 - (1 - PrecipitationHoch / (InfiltrationRateMax + 0.0000000001))^(paInfiltrationRateB + 1))
  TEMMax <- InfiltrationRateMax / (paInfiltrationRateB + 1)
  TEMDiff <- PrecipitationHoch - InfiltrationRateMax
  TEM <- TEMMin
  TEM[which(TEMDiff > 0.0)] <- TEMMax[which(TEMDiff > 0.0)]
  return(TEM)
}






#' RUNOFF
#' @param InList list of Input data
#' @param ... other Paramater and inputdata
#' @return SoilInfiltration
#' @export
RUNOFF <- function(InList, ...) UseMethod("RUNOFF", InList)

#' SoilInfiltration in Saturation ExcessRunoff modell
#' @param InList 3-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureVolum
#' }
#' @param PaList 1-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' }
#' @param ... other Paramater and inputdata
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item Infiltration
#' }
#' @export
RUNOFF.SER <- function(InList, PaList, ...){
  PrecipitationHoch <- InList$PrecipitationHoch
  SoilMoistureCapacityMax <- InList$SoilMoistureCapacityMax
  SoilMoistureVolum <- InList$SoilMoistureVolum

  SoilMoistureCapacity <- fctMoistureCapacity(SoilMoistureVolum, SoilMoistureCapacityMax)
  SIInList <- InList
  SIInList[[3]] <- SoilMoistureCapacity
  class(SIInList) = "SER"
  SoilInfiltrationSER <- fctSoilInfiltration(SIInList, PaList)
  SaturationExcessRunoff <- PrecipitationHoch - SoilInfiltrationSER
  return(list(Runoff = SaturationExcessRunoff, Infiltration = SoilInfiltrationSER))
}

#' Infiltration in OverInfiltrationExcessRunoff
#' @param InList 3-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureVolum
#' }
#' @param PaList 1-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' }
#' @param ... other Paramater and inputdata
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item Infiltration
#' }
#' @export
RUNOFF.OIER <- function(InList, PaList, ...){
  PrecipitationHoch <- InList$PrecipitationHoch
  InfiltrationRateMax <- InList$InfiltrationRateMax
  class(InList) <- "OIER"
  SoilInfiltrationOIER <- fctSoilInfiltration(InList, PaList)
  OverInfiltrationExcessRunoff <- maxSVector(0.0,PrecipitationHoch - SoilInfiltrationOIER)
  return(list(Runoff = OverInfiltrationExcessRunoff, Infiltration = SoilInfiltrationOIER))
}

#' find the rate of SER and OIER
#' @param rate rate procent
#' @param PrecipitationHoch Precipitation Hoch, mm
#' @param SoilMoistureCapacityMax Soil Moisture Capacity Max
#' @param SoilMoistureCapacity Soil Moisture Capacity
#' @param InfiltrationRateMax Infiltration Rate Max
#' @param paSoilMoistureCapacityB param Soil Moisture Capacity B
#' @param paInfiltrationRateB param Infiltration Rate B
#' @return rate procent
#' @export
fctVICRateFind <- function(rate, PrecipitationHoch,
                           SoilMoistureCapacityMax, SoilMoistureCapacity,
                           InfiltrationRateMax, paSoilMoistureCapacityB, paInfiltrationRateB){
  SoilInfiltrationSER <- fctSoilInfiltration.SER(list(PrecipitationHoch = rate * PrecipitationHoch,
                                                      SoilMoistureCapacityMax = SoilMoistureCapacityMax, 
                                                      SoilMoistureCapacity = SoilMoistureCapacity),
                                                 list(paSoilMoistureCapacityB))
  Pr_RunoffSER <- rate * PrecipitationHoch - SoilInfiltrationSER
  SaturatedArea <- fctSaturatedArea(SoilMoistureCapacity + rate * PrecipitationHoch,
                                    SoilMoistureCapacityMax, paSoilMoistureCapacityB)
  SoilInfiltrationOIER <- fctSoilInfiltration.OIER(list(PrecipitationHoch = PrecipitationHoch - Pr_RunoffSER, 
                                                        InfiltrationRateMax = InfiltrationRateMax),
                                                   list(paInfiltrationRateB))
  Pr_RunoffOIER <- PrecipitationHoch - Pr_RunoffSER - SoilInfiltrationOIER
  returnTem <- Pr_RunoffOIER + rate * PrecipitationHoch
  returnTem[is.na(returnTem)] <- PrecipitationHoch
  return(returnTem)
}

#' VIC runoff (OverInfiltrationExcessRunoff and SaturationExcessRunoff)
#' @references Liang X, Xie Z. A new surface runo parameterization with subgrid-scale soil heterogeneity for land surface models[J]. Advances in Water Resources, 2001(24):1173-1193.
#' @import OptimizationPrg
#' @param InList 4-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureVolume
#' \item InfiltrationRateMax
#' }
#' @param PaList 2-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' \item paInfiltrationRateB
#' }
#' @param ... other Paramater and inputdata
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item Infiltration
#' }
#' @export
RUNOFF.VIC <- function(InList, PaList, ...){
  PrecipitationHoch <- InList$PrecipitationHoch
  SoilMoistureCapacityMax <- InList$SoilMoistureCapacityMax
  SoilMoistureVolume <- InList$SoilMoistureVolume
  InfiltrationRateMax <- InList$InfiltrationRateMax

  paSoilMoistureCapacityB <- PaList$paSoilMoistureCapacityB
  paInfiltrationRateB <- PaList$paInfiltrationRateB
  SoilMoistureCapacity <- fctMoistureCapacity(SoilMoistureVolume,
                                              SoilMoistureCapacityMax,
                                              paSoilMoistureCapacityB)

  nVector <- length(PrecipitationHoch)
  RunoffVIC = SoilInfiltrationVIC = SoilInfiltrationVICSER = SoilInfiltrationVICOIER = RatSER = array(0.0, dim = c(nVector,1))
  for (i in 1:nVector) {
    if(PrecipitationHoch[i] == 0.0) {
      SoilInfiltrationVIC[i] = 0.0
      RunoffVIC[i] = 0.0
    }
    else {
      RatSER[i] <- eindim618funk(fctVICRateFind, 0, 1, PrecipitationHoch[i], absolutabweichungfunk, 5,
                                 PrecipitationHoch = PrecipitationHoch[i],
                                 SoilMoistureCapacityMax = SoilMoistureCapacityMax[i],
                                 SoilMoistureCapacity = SoilMoistureCapacity[i],
                                 InfiltrationRateMax = InfiltrationRateMax[i],
                                 paSoilMoistureCapacityB = paSoilMoistureCapacityB,
                                 paInfiltrationRateB = paInfiltrationRateB)
      SoilInfiltrationVICSER[i] <- fctSoilInfiltration.SER(list(PrecipitationHoch = RatSER[i] * PrecipitationHoch[i],
                                                                SoilMoistureCapacityMax = SoilMoistureCapacityMax[i],
                                                                SoilMoistureCapacity = SoilMoistureCapacity[i]),
                                                           PaList)
      SoilInfiltrationVICOIER[i] <- fctSoilInfiltration.OIER(list(PrecipitationHoch = (1 - RatSER[i]) * PrecipitationHoch[i],
                                                                  InfiltrationRateMax = InfiltrationRateMax[i]),
                                                             PaList)
      SoilInfiltrationVIC[i] <- SoilInfiltrationVICSER[i] + SoilInfiltrationVICOIER[i]
      RunoffVIC[i] <- PrecipitationHoch[i] - SoilInfiltrationVIC[i]
    }
  }
  SoilInfiltrationVIC[is.na(SoilInfiltrationVIC)] = fctSoilInfiltration.SER(list(PrecipitationHoch = PrecipitationHoch,
                                                                                 SoilMoistureCapacityMax = SoilMoistureCapacityMax,
                                                                                 SoilMoistureCapacity = SoilMoistureCapacity),
                                                                            PaList)[is.na(SoilInfiltrationVIC)]
  RunoffVIC <- PrecipitationHoch - SoilInfiltrationVIC
  return(list(Runoff = RunoffVIC, Infiltration = SoilInfiltrationVIC, RatSER = RatSER))
}





#' VitcalMisingExcessRunoff runoff (OverInfiltrationExcessRunoff and SaturationExcessRunoff)
#' @param InList 4-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureCapacity
#' \item InfiltrationRateMax
#' }
#' @param PaList 2-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' \item paInfiltrationRateB
#' }
#' @param ... other Paramater and inputdata
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item Infiltration in VitcalMisingExcessRunoff
#' }
#' @export
RUNOFF.VM <- function(InList, PaList, ...){
  PrecipitationHoch <- InList$PrecipitationHoch
  SoilMoistureCapacityMax <- InList$SoilMoistureCapacityMax
  SoilMoistureCapacity <- InList$SoilMoistureCapacity
  InfiltrationRateMax <- InList$InfiltrationRateMax

  SIInList <- list(InList[[1]], InList[[4]])
  class(SIInList) = "OIER"
  SoilInfiltrationOIER <- fctSoilInfiltration(SIInList, PaList)

  OverInfiltrationExcessRunoff <- PrecipitationHoch - SoilInfiltrationOIER

  SIInList <- InList
  SIInList[[1]] <- SoilInfiltrationOIER
  class(SIInList) = "SER"
  SoilInfiltrationSER <- fctSoilInfiltration(SIInList, PaList)
  SaturationExcessRunoff <- SoilInfiltrationOIER - SoilInfiltrationSER
  return(list(Runoff = OverInfiltrationExcessRunoff + SaturationExcessRunoff, Infiltration = SoilInfiltrationSER))
}













