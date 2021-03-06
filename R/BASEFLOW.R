#' caculate Base flow
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return BASEFLOW
#' @export
BASEFLOW <- function(InData, ...) UseMethod("BASEFLOW", InData)

#' baseflow
#' @references Arnold J G, Srinivasan R, Muttiah R S et al. Large area hydrologic modeling and assessmentpart I: Model Develoment [J]. Journal of the American Water Resources Association, 1998(34):73-89.
#' @param InData 2-list of:
#' \itemize{
#' \item SoilMoistureVolume
#' \item SoilMoistureVolumeMax
#' }
#' @param Param 4-list of:
#' \itemize{
#' \item paDrainageLossMax
#' \item paDrainageLossMin
#' \item paExponentARNOBase
#' \item paSoilMoistureVolumeARNOBaseThresholdRadio
#' }
#' @param ... other Parmeters
#' @return baseflow
#' @export BASEFLOW.ARNO
#' @export
BASEFLOW.ARNO <- function(InData, Param, ...){

  SoilMoistureVolume <- InData$Ground$MoistureVolume
  SoilMoistureVolumeMax <- InData$Ground$MoistureCapacityMax

  paExponentARNOBase <- Param$ExponentARNOBase
  paSoilMoistureVolumeARNOBaseThresholdRadio <- Param$ARNOBaseThresholdRadio
  paDrainageLossMax <- Param$DrainageLossMax
  paDrainageLossMin <- Param$DrainageLossMin

  SoilMoistureVolumeARNOBaseThreshold <- paSoilMoistureVolumeARNOBaseThresholdRadio * SoilMoistureVolumeMax
  TEMMin <- paDrainageLossMin * SoilMoistureVolume / SoilMoistureVolumeMax
  TEMMax <- TEMMin + (paDrainageLossMax - paDrainageLossMin) *
    ((SoilMoistureVolume - SoilMoistureVolumeARNOBaseThreshold) /
       (SoilMoistureVolumeMax - SoilMoistureVolumeARNOBaseThreshold))^paExponentARNOBase
  TEMDiff <- SoilMoistureVolume - SoilMoistureVolumeARNOBaseThreshold
  TEM <- TEMMin
  TEM[which(TEMDiff > 0.0)] <- TEMMax[which(TEMDiff > 0.0)]
  # browser()
  return(list(Ground = list(BaseFlow = minVector(SoilMoistureVolume, TEM))))
}

