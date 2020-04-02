#' Instantaneous unit hydrograph
#' @param t numrica,time
#' @param InList list of Input data
#' @return Instantaneous unit hydrograph
#' @export
fctIUH <- function(t, InList) UseMethod("fctIUH", InList)

#' Instantaneous unit hydrograph in Nash
#' @references Nash J E, Sutcliffe J V. River flow forecasting through conceptual models part I — A discussion of principles[J]. Journal of Hydrology, 1970(10):282-290.
#' @param t numrica,time
#' @param InList list of Input data
#' @return Instantaneous unit hydrograph
#' @export
fctIUH.Nash <- function(t, InList){
  coefficientStorage <- InList[[1]]
  paramShape <- InList[[2]]
  ut = 1.0 / (coefficientStorage * gamma(paramShape)) * (t / coefficientStorage)^(paramShape - 1) *
    exp(-1.0 * t / coefficientStorage)
  return(ut)
}

#' Instantaneous unit hydrograph in ShiPeng
#' @references 芮孝芳, 石朋. 基于地貌扩散和水动力扩散的流域瞬时单位线研究[J]. 水科学进展, 2002(13):439-444.
#' @param t numrica,time
#' @param InList list of Input data
#' @return Instantaneous unit hydrograph
#' @export
fctIUH.Shipeng <- function(t, InList){
  paramRiverheadNumber <- InList[[1]]
  paramStreamLength <- InList[[2]]
  WaveVelocity <- InList[[3]]
  ut = t / (2 * paramRiverheadNumber * (paramStreamLength / WaveVelocity)^2) *
    exp(-1.0 * WaveVelocity^2 * t^2 / (4 * paramRiverheadNumber * paramStreamLength^2))
  return(ut)
}

#' Instantaneous unit hydrograph in RUI XioaFang
#' @references 芮孝芳, 石朋. 基于地貌扩散和水动力扩散的流域瞬时单位线研究[J]. 水科学进展, 2002(13):439-444.
#' @param t numrica,time
#' @param InList list of Input data
#' @return Instantaneous unit hydrograph
#' @export
fctIUH.RuiXF<-function(t, InList){
  RiverLength <- InList[[1]]
  AttenuationCoeffcient <- InList[[2]]
  WaveVelocity <- InList[[3]]
  ut <- RiverLength / (4 * pi * AttenuationCoeffcient * t^3)^0.5 *
    exp(-1.0 * (WaveVelocity * t - RiverLength)^2/(4 * AttenuationCoeffcient * t)) #km2/h
  return(ut)
}

#' make Param list for UH
#' @importFrom purrr reduce
#' @param PaInList list of parameter for every IUH
#' @param methond IUH methond name
#' @return Parameter for UH mit class name
#' @export
fctMakeUHParam <- function(PaInList, methond){
  PaOut <- reduce(PaInList, cbind)
  class(PaOut) <- methond
  return(PaOut)
}

#' make all Param list for UH all
#' @param PaInList list of parameter for every IUH
#' @param methondList list of IUH methond name
#' @return list of Parameter for all UH mit class name
#' @export
fctMakeUHParamAll <- function(PaInList, methondList){
  return(map2(PaInList, methondList, fctMakeUHParam))
}

#' make Discrete unit hydrograph durch Instantaneous unit hydrograph
#' @importFrom stats integrate
#' @param G2AimGPa vector of Paramater for IUH
#' @param UHPeriodN howmany Discrete have
#' @return matrix, Discrete unit hydrograph
#' @export
fctMakeUH <- function(G2AimGPa, UHPeriodN){
  GN <- dim(G2AimGPa)[1]
  UH <- matrix(0.0, UHPeriodN, GN)
  for(j in 1:GN){
    UHInList <- as.list(G2AimGPa[j,])
    class(UHInList) = class(G2AimGPa)

    bondh = bondl = 0.0
    for(i in 1:UHPeriodN){
      bondh <- bondl + 1
      Term <- integrate(fctIUH, bondl, bondh, InList = UHInList)$value
      UH[i,j] <- round(Term,7)
      bondl <- bondh
    }
  }
  return(UH)
}

#' make all Discrete unit hydrograph durch Instantaneous unit hydrograph
#' @importFrom purrr map
#' @param G2AimGList list of all vector for IUH
#' @param UHPeriodN howmany Discrete have
#' @return matrix, Discrete unit hydrograph
#' @export
fctUHALLMake <- function(G2AimGList, UHPeriodN = 180){
  return(map(G2AimGList, fctMakeUH, UHPeriodN))
}








