#' INTERCEPTION caculate
#' @param InList list of Input data
#' @param ... other Paramater and inputdata
#' @return INTERCEPTION
#' @export
INTERCEPTION <- function(InList, ...) UseMethod("INTERCEPTION", InList)

#' INTERCEPTION whith Gash methond
#' @references Gash J H C. An analytical model of rainfall interception by forests[J]. Quarterly Journal of the Royal Meteorological Society, 1978(105):43-55.
#' @param InList 4-list of:
#' \itemize{
#' \item Volum,
#' \item CanopyStorageCapacity,
#' \item RainfallDuringSaturation,
#' \item Evaporation
#' }
#' @param PaList 1-list of:
#' \itemize{
#' \item paCoefficientFreeThroughfall
#' }
#' @param ... other Paramater and inputdata
#' @return INTERCEPTION
#' @export
INTERCEPTION.Gash <- function(InList, PaList , ...){
  constZero <- 0.000000000001
  Volum <- InList$Volum
  CanopyStorageCapacity <- InList$CanopyStorageCapacity
  RainfallDuringSaturation <- InList$RainfallDuringSaturation
  Evaporation <- InList$Evaporation

  paCoefficientFreeThroughfall <- PaList$paCoefficientFreeThroughfall

  NecessaryToSaturateCanopy <- (-1 * RainfallDuringSaturation *
                                  CanopyStorageCapacity / (Evaporation + constZero)) *
                                  log(1 - Evaporation / (RainfallDuringSaturation *
                                  (1 - paCoefficientFreeThroughfall) + constZero))
  returnTem <- minVector(RainfallDuringSaturation,
                         minVector(CanopyStorageCapacity, NecessaryToSaturateCanopy))
  returnTem[is.na(returnTem)] <- 0.0
  return(returnTem)
}





