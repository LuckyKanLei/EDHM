#' INTERCEPTION caculate
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return INTERCEPTION
#' @export
INTERCEPTION <- function(InData, ...) UseMethod("INTERCEPTION", InData)

#' INTERCEPTION whith Gash methond
#' @references Gash J H C. An analytical model of rainfall interception by forests[J]. Quarterly Journal of the Royal Meteorological Society, 1978(105):43-55.
#' @param InData 4-list of:
#' \itemize{
#' \item Volum,
#' \item CanopyStorageCapacity,
#' \item RainfallDuringSaturation,
#' \item Evaporation
#' }
#' @param Param 1-list of:
#' \itemize{
#' \item paCoefficientFreeThroughfall
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
#' @export INTERCEPTION.Gash
#' @export
INTERCEPTION.Gash <- function(InData, Param, runMode = "RUN", viewGN = 3, ...){
  CanopyStorageCapacity <- InData$Canopy$StorageCapacity - InData$Intercept$Interception
  RainfallDuringSaturation <- InData$Prec$Precipitation
  Evaporation <- InData$ET$EvaporationCanopy

  paCoefficientFreeThroughfall <- Param$CoefficientFreeThroughfall

  NecessaryToSaturateCanopy <- (-1 * RainfallDuringSaturation *
                                  CanopyStorageCapacity / (Evaporation + DBL_EPSILON)) *
    log(1 - Evaporation / (RainfallDuringSaturation *
                             (1 - paCoefficientFreeThroughfall) + DBL_EPSILON))
  returnTem <- minVector(RainfallDuringSaturation,
                         minVector(CanopyStorageCapacity, NecessaryToSaturateCanopy))
  returnTem[is.na(returnTem)] <- 0.0
  return(list(Intercept = list(Interception = InData$Intercept$Interception + returnTem - Evaporation),
              Prec = list(Precipitation = RainfallDuringSaturation - returnTem)))
}





