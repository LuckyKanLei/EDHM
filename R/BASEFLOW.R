#' caculate Base flow
#' @param InList list of Input data
#' @param ... other Paramater and inputdata
#' @return BASEFLOW
#' @export
BASEFLOW <- function(InList, ...) UseMethod("BASEFLOW", InList)

#' baseflow
#' @references Arnold J G, Srinivasan R, Muttiah R S et al. Large area hydrologic modeling and assessmentpart I: Model Develoment [J]. Journal of the American Water Resources Association, 1998(34):73-89.
#' @param InList 2-list of:
#' \itemize{
#' \item SoilMoistureVolume
#' \item SoilMoistureVolumeMax
#' }
#' @param PaList 4-list of:
#' \itemize{
#' \item paDrainageLossMax
#' \item paDrainageLossMin
#' \item paExponentARNOBase
#' \item paSoilMoistureVolumeARNOBaseThresholdRadio
#' }
#' @param ... other Paramater and inputdata
#' @return baseflow
#' @export
BASEFLOW.ARNO <- function(InList, PaList, ...){

  SoilMoistureVolume <- InList$SoilMoistureVolume
  SoilMoistureVolumeMax <- InList$SoilMoistureVolumeMax

  paExponentARNOBase <- PaList$paExponentARNOBase
  paSoilMoistureVolumeARNOBaseThresholdRadio <- PaList$paSoilMoistureVolumeARNOBaseThresholdRadio
  paDrainageLossMax <- PaList$paDrainageLossMax
  paDrainageLossMin <- PaList$paDrainageLossMin

  SoilMoistureVolumeARNOBaseThreshold <- paSoilMoistureVolumeARNOBaseThresholdRadio * SoilMoistureVolumeMax
  TEMMin <- paDrainageLossMin * SoilMoistureVolume / SoilMoistureVolumeMax
  TEMMax <- TEMMin + (paDrainageLossMax - paDrainageLossMin) *
    ((SoilMoistureVolume - SoilMoistureVolumeARNOBaseThreshold) /
       (SoilMoistureVolumeMax - SoilMoistureVolumeARNOBaseThreshold))^paExponentARNOBase
  TEMDiff <- SoilMoistureVolume - SoilMoistureVolumeARNOBaseThreshold
  TEM <- TEMMin
  TEM[which(TEMDiff > 0.0)] <- TEMMax[which(TEMDiff > 0.0)]
  return(minVector(SoilMoistureVolume, TEM))
}

