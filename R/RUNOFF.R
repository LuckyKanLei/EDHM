#' Infiltration caculate
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return Infiltration
#' @export
InfiltratRat <- function(InData, ...) UseMethod("InfiltratRat", InData)

#' Infiltration with GreenAmpt methond
#' @references Tarboton D G. Rainfall-runoff processes[M]. Utah State University, 2003.
#' @param InData 5-list of:
#' \itemize{
#' \item HydraulicConductivity
#' \item WettingFrontSoilSuction
#' \item SoilMoistureContent
#' \item EffectivePorosity
#' \item SoilMoistureVolume
#' }
#' @param Param Param
#' @param ... other Parmeters
#' @return Infiltration rat
#' @export InfiltratRat.GreenAmpt
#' @export
InfiltratRat.GreenAmpt <- function(InData, Param, ...){
  HydraulicConductivity <- InData$SoilData$Conductivity
  WettingFrontSoilSuction <- InData$SoilData$WettingFrontSuction
  EffectivePorosity <- InData$SoilData$Porosity
  SoilMoistureVolume <- InData$Ground$MoistureVolume
  SoilMoistureContent <- SoilMoistureVolume / InData$Ground$Depth

  InfiltrationRat <- HydraulicConductivity *
    (1 + WettingFrontSoilSuction * (EffectivePorosity - SoilMoistureContent) / SoilMoistureVolume)
  # browser()
  return(list(Infilt = list(InfiltrationRat = as.array(maxSVector(0.0, minSVector(9999.0, InfiltrationRat)), dim = 1))))  ## P > Ks
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
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return SoilInfiltration
#' @export
Infiltration <- function(InData, ...) UseMethod("Infiltration", InData)

#' SoilInfiltration rate in Saturation Excess Runoff modell
#' @param InData 3-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureCapacity
#' }
#' @param Param 1-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' }
#' @param ... other Parmeters
#' @return SoilInfiltration in SaturationExcessRunoff modell
#' @export Infiltration.SER
#' @export
Infiltration.SER <- function(InData, Param, ...){
  PrecipitationHoch <- InData$Prec$RainFall
  SoilMoistureCapacityMax <- InData$Ground$MoistureCapacityMax
  SoilMoistureCapacity <- InData$Ground$MoistureCapacity

  paSoilMoistureCapacityB <- Param$SoilMoistureCapacityB

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
  return(list(Infilt = list(Infiltration = TEM)))
}

#' Infiltration rate in Over Infiltration Excess Runoff
#' @param InData 2-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item InfiltrationRateMax
#' }
#' @param Param 1-list of:
#' \itemize{
#' \item paInfiltrationRateB
#' }
#' @param ... other Parmeters
#' @return Infiltration rate in Over Infil tration Excess Runoff
#' @export Infiltration.OIER
#' @export
Infiltration.OIER <- function(InData, Param, ...){
  PrecipitationHoch <- InData$Prec$RainFall
  InfiltrationRateMax <- InData$Infilt$InfiltrationRat

  paInfiltrationRateB <- Param$InfiltrationRateB

  TEMMin <- InfiltrationRateMax / (paInfiltrationRateB + 1) *
    (1 - (1 - PrecipitationHoch / (InfiltrationRateMax + 0.0000000001))^(paInfiltrationRateB + 1))
  TEMMax <- InfiltrationRateMax / (paInfiltrationRateB + 1)
  TEMDiff <- PrecipitationHoch - InfiltrationRateMax
  TEM <- TEMMin
  TEM[which(TEMDiff > 0.0)] <- TEMMax[which(TEMDiff > 0.0)]
  return(list(Infilt = list(Infiltration = TEM)))
}






#' RUNOFF
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return SoilInfiltration
#' @export
RUNOFF <- function(InData, ...) UseMethod("RUNOFF", InData)

#' SoilInfiltration in Saturation ExcessRunoff modell
#' @param InData 3-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureVolum
#' }
#' @param Param 1-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' }
#' @param ... other Parmeters
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item Infiltration
#' }
#' @export RUNOFF.SER
#' @export
RUNOFF.SER <- function(InData, Param, ...){
  PrecipitationHoch <- InData$Prec$RainFall
  SoilMoistureCapacityMax <- InData$Ground$MoistureCapacityMax
  SoilMoistureVolum <- InData$Ground$MoistureVolume

  SoilMoistureCapacity <- fctMoistureCapacity(SoilMoistureVolum, SoilMoistureCapacityMax)
  SIInData <- InData
  SIInData[[3]] <- SoilMoistureCapacity
  class(SIInData) = "SER"
  SoilInfiltrationSER <- Infiltration(SIInData, Param)
  SaturationExcessRunoff <- PrecipitationHoch - SoilInfiltrationSER
  return(list(Ground = list(Runoff = SaturationExcessRunoff), Infilt = list(Infiltration = SoilInfiltrationSER)))
}

#' Infiltration in OverInfiltrationExcessRunoff
#' @param InData 3-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureVolum
#' }
#' @param Param 1-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' }
#' @param ... other Parmeters
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item Infiltration
#' }
#' @export RUNOFF.OIER
#' @export
RUNOFF.OIER <- function(InData, Param, ...){
  PrecipitationHoch <- InData$Prec$RainFall
  InfiltrationRateMax <- InData$Infilt$InfiltrationRateMax
  class(InData) <- "OIER"
  SoilInfiltrationOIER <- Infiltration(InData, Param)
  OverInfiltrationExcessRunoff <- maxSVector(0.0,PrecipitationHoch - SoilInfiltrationOIER)
  return(list(Ground = list(Runoff = OverInfiltrationExcessRunoff), Infilt = list(Infiltration = SoilInfiltrationOIER)))
}

#' find the rate of SER and OIER
#' @param rate rate procent
#' @param PrecipitationHoch Precipitation Hoch, mm
#' @param SoilMoistureCapacityMax Soil Moisture Capacity Max
#' @param SoilMoistureCapacity Soil Moisture Capacity
#' @param InfiltrationRateMax Infiltration Rate Max
#' @param Param 2-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' \item paInfiltrationRateB
#' }
#' @return rate procent
#' @export
fctVICRateFind <- function(rate, PrecipitationHoch,
                           SoilMoistureCapacityMax, SoilMoistureCapacity,
                           InfiltrationRateMax, Param){
  SoilInfiltrationSER <- (Infiltration.SER(list(Prec = list(RainFall = rate * PrecipitationHoch),
                                                Ground = list(MoistureCapacityMax = SoilMoistureCapacityMax,
                                                              MoistureCapacity = SoilMoistureCapacity)),
                                           Param))$Infilt$Infiltration
  Pr_RunoffSER <- rate * PrecipitationHoch - SoilInfiltrationSER
  SaturatedArea <- fctSaturatedArea(SoilMoistureCapacity + rate * PrecipitationHoch,
                                    SoilMoistureCapacityMax, Param$SoilMoistureCapacityB)
  SoilInfiltrationOIER <- (Infiltration.OIER(list(Prec = list(RainFall = PrecipitationHoch - Pr_RunoffSER),
                                                  Infilt = list(InfiltrationRat = InfiltrationRateMax)),
                                             Param))$Infilt$Infiltration
  Pr_RunoffOIER <- PrecipitationHoch - Pr_RunoffSER - SoilInfiltrationOIER
  returnTem <- Pr_RunoffOIER + rate * PrecipitationHoch
  returnTem[is.na(returnTem)] <- PrecipitationHoch
  return(returnTem)
}

#' VIC runoff (OverInfiltrationExcessRunoff and SaturationExcessRunoff)
#' @references Liang X, Xie Z. A new surface runo parameterization with subgrid-scale soil heterogeneity for land surface models[J]. Advances in Water Resources, 2001(24):1173-1193.
#' @import HMtools
#' @param InData 4-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureVolume
#' \item InfiltrationRateMax
#' }
#' @param Param 2-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' \item paInfiltrationRateB
#' }
#' @param ... other Parmeters
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item Infiltration
#' }
#' @export RUNOFF.Vic
#' @export
RUNOFF.Vic <- function(InData, Param, ...){
  PrecipitationHoch <- InData$Prec$RainFall
  SoilMoistureCapacityMax <- InData$Ground$MoistureCapacityMax
  SoilMoistureVolume <- InData$Ground$MoistureVolume
  InfiltrationRateMax <- minVector(InData$Infilt$InfiltrationRat, SoilMoistureCapacityMax)

  paSoilMoistureCapacityB <- Param$SoilMoistureCapacityB
  paInfiltrationRateB <- Param$InfiltrationRateB
# browser()
  SoilMoistureCapacity <- fctMoistureCapacity(SoilMoistureVolume,
                                              SoilMoistureCapacityMax,
                                              paSoilMoistureCapacityB)

  nVector <- length(PrecipitationHoch)
  RunoffVIC = SoilInfiltrationVIC = SoilInfiltrationVICSER = SoilInfiltrationVICOIER = RatSER = array(0.0, dim = c(nVector))
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
                                 Param = Param)
      SoilInfiltrationVICSER[i] <- (Infiltration.SER(list(Prec = list(RainFall = RatSER[i] * PrecipitationHoch[i]),
                                                          Ground = list(MoistureCapacityMax = SoilMoistureCapacityMax[i],
                                                                        MoistureCapacity = SoilMoistureCapacity[i])),
                                                     Param))$Infilt$Infiltration
      SoilInfiltrationVICOIER[i] <- (Infiltration.OIER(list(Prec = list(RainFall = (1 - RatSER[i]) * PrecipitationHoch[i]),
                                                            Infilt = list(InfiltrationRat = InfiltrationRateMax[i])),
                                                       Param))$Infilt$Infiltration
      SoilInfiltrationVIC[i] <- SoilInfiltrationVICSER[i] + SoilInfiltrationVICOIER[i]
      RunoffVIC[i] <- PrecipitationHoch[i] - SoilInfiltrationVIC[i]
    }
  }
  SoilInfiltrationVIC[is.na(SoilInfiltrationVIC)] = (Infiltration.SER(list(Prec = list(RainFall = PrecipitationHoch),
                                                                           Ground = list(MoistureCapacityMax = SoilMoistureCapacityMax,
                                                                                         MoistureCapacity = SoilMoistureCapacity)),
                                                                      Param))$Infilt$Infiltration[is.na(SoilInfiltrationVIC)]
  RunoffVIC <- PrecipitationHoch - SoilInfiltrationVIC
  return(list(Ground = list(Runoff = RunoffVIC), Infilt = list(Infiltration = SoilInfiltrationVIC)))
}





#' VitcalMisingExcessRunoff runoff (OverInfiltrationExcessRunoff and SaturationExcessRunoff)
#' @param InData 4-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureCapacityMax
#' \item SoilMoistureCapacity
#' \item InfiltrationRateMax
#' }
#' @param Param 2-list of:
#' \itemize{
#' \item paSoilMoistureCapacityB
#' \item paInfiltrationRateB
#' }
#' @param ... other Parmeters
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item Infiltration in VitcalMisingExcessRunoff
#' }
#' @export RUNOFF.VM
#' @export
RUNOFF.VM <- function(InData, Param, ...){
  PrecipitationHoch <- InData$Prec$RainFall
  SoilMoistureCapacity <- InData$Ground$MoistureCapacity
  SoilMoistureCapacityMax <- InData$Ground$MoistureCapacityMax
  InfiltrationRateMax <- InData$Infilt$InfiltrationRateMax

  SIInData <- list(InData[[1]], InData[[4]])
  class(SIInData) = "OIER"
  SoilInfiltrationOIER <- Infiltration(SIInData, Param)

  OverInfiltrationExcessRunoff <- PrecipitationHoch - SoilInfiltrationOIER

  SIInData <- InData
  SIInData[[1]] <- SoilInfiltrationOIER
  class(SIInData) = "SER"
  SoilInfiltrationSER <- Infiltration(SIInData, Param)
  SaturationExcessRunoff <- SoilInfiltrationOIER - SoilInfiltrationSER
  return(list(Ground = list(Runoff = OverInfiltrationExcessRunoff + SaturationExcessRunoff), Infilt = list(Infiltration = SoilInfiltrationSER)))
}


#' VIC runoff (OverInfiltrationExcessRunoff and SaturationExcessRunoff)
#' @references https://webgr.inrae.fr/en/models/daily-hydrological-model-gr4j/description-of-the-gr4j-model/
#' @references Perrin, C., Michel, C. and Andréassian, V., 2003. Improvement of a parsimonious model for streamflow simulation. Journal of Hydrology, 279 : 275-289, DOI: 10.1016/S0022-1694(03)00225-7
#' @import HMtools
#' @param InData 4-list of:
#' \itemize{
#' \item PrecipitationHoch
#' \item SoilMoistureVolume
#' \item AET
#' }
#' @param Param 2-list of:
#' \itemize{
#' \item Gr4j_X1
#' }
#' @param ... other Parmeters
#' @return 2-list of:
#' \itemize{
#' \item Runoff
#' \item MoistureVolume
#' }
#' @export RUNOFF.Gr4j
#' @export
RUNOFF.Gr4j <- function(InData, Param, ...){
  X1 <- Param$Gr4j_X1

  Pn <- InData$Prec$Precipitation
  Es <- InData$Evatrans$AET
  S <- InData$Ground$MoistureVolume

  Ps <- X1 * (1- (S / X1) * (S / X1)) * tanh(Pn / X1) / (1 + S / X1 * tanh(Pn / X1))
  S <- S - Es + Ps
  Perc <- S * (1 - (1+ (4/9 * S / X1)^4)^(-0.25))
  S <- S - Perc
  Pr <- Perc + (Pn - Ps)
  return(list(Ground = list(Runoff = Pr, MoistureVolume = S)))

}











