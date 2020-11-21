#' INTERCEPTION caculate
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return INTERCEPTION
#' @export
INTERCEPTION <- function(InData, ...) UseMethod("INTERCEPTION", InData)
#' @title Gash s4 class
#' @importFrom methods new
#' @export Gash
Gash <- setClass("Gash", contains = "HM.Data")
CanopyGash <- setClass("CanopyGash",
                       slots = c(StorageCapacity = "array"))
InterceptGash <- setClass("InterceptGash",
                          slots = c(Interception = "array"))
PrecGash <- setClass("PrecGash",
                     slots = c(Precipitation = "array"))
EvatransGash <- setClass("EvatransGash",
                         slots = c(EvaporationCanopy = "array"))

InGash <- setClass("InGash",
                   slots = c(Canopy = "CanopyGash",
                             Evatrans = "EvatransGash",
                             Intercept = "InterceptGash",
                             Prec = "PrecGash"),
                   contains = "Gash")
OutGash <- setClass("OutGash",
                    slots = c(Intercept = "InterceptGash",
                              Prec = "PrecGash"),
                    contains = "HM.Data")

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
  CanopyStorageCapacity <- InData@Canopy@StorageCapacity - InData@Intercept@Interception
  RainfallDuringSaturation <- InData@Prec@Precipitation
  Evaporation <- InData@Evatrans@EvaporationCanopy

  paCoefficientFreeThroughfall <- Param@CoefficientFreeThroughfall

  NecessaryToSaturateCanopy <- (-1 * RainfallDuringSaturation *
                                  CanopyStorageCapacity / (Evaporation + DBL_EPSILON)) *
    log(1 - Evaporation / (RainfallDuringSaturation *
                             (1 - paCoefficientFreeThroughfall) + DBL_EPSILON))
  returnTem <- minVector(RainfallDuringSaturation,
                         minVector(CanopyStorageCapacity, NecessaryToSaturateCanopy))
  returnTem[is.na(returnTem)] <- 0.0
  Out <- OutGash()
  Out@Intercept@Interception <- InData@Intercept@Interception + returnTem - Evaporation
  Out@Prec@Precipitation <- RainfallDuringSaturation - returnTem
  return(Out)
}





