#' Instantaneous unit hydrograph
#' @param tt numrica,time
#' @param InList list of Input data
#' @return Instantaneous unit hydrograph
#' @export
fctIUH <- function(tt, InList) UseMethod("fctIUH", InList)

#' Instantaneous unit hydrograph in Nash
#' @references Nash J E, Sutcliffe J V. River flow forecasting through conceptual models part I — A discussion of principles[J]. Journal of Hydrology, 1970(10):282-290.
#' @param tt numrica,time
#' @param InList list of Input data
#' @return Instantaneous unit hydrograph
#' @export fctIUH.Nash
#' @export
fctIUH.Nash <- function(tt, InList){
  coefficientStorage <- InList$IUHNashStorage
  paramShape <- InList$IUHNashShape
  ut = 1.0 / (coefficientStorage * gamma(paramShape)) * (tt / coefficientStorage)^(paramShape - 1) *
    exp(-1.0 * tt / coefficientStorage)
  return(ut)
}

#' Instantaneous unit hydrograph in ShiPeng
#' @references 芮孝芳, 石朋. 基于地貌扩散和水动力扩散的流域瞬时单位线研究[J]. 水科学进展, 2002(13):439-444.
#' @param tt numrica,time
#' @param InList list of Input data
#' @return Instantaneous unit hydrograph
#' @export fctIUH.Shipeng
#' @export
fctIUH.Shipeng <- function(tt, InList){
  tN <- length(tt)
  gN <- length(InList$StreamLength)
  n1 <- length(InList$RiverheadNumber)
  n2 <- length(InList$WaveVelocity)
  paramStreamLength <- matrix(rep(InList$StreamLength, tN), tN, gN, byrow = T)
  paramRiverheadNumber <- ifelse(n1 == 1,
                                 matrix(rep(InList$RiverheadNumber, tN * gN), tN, gN),
                                 matrix(rep(InList$RiverheadNumber, tN), tN, gN, byrow = T))
  # paramRiverheadNumber <- matrix(rep(InList$RiverheadNumber, tN), tN, gN, byrow = T)
  WaveVelocity <- ifelse(n2 == 1,
                         matrix(rep(InList$WaveVelocity, tN * gN), tN, gN),
                         matrix(rep(InList$WaveVelocity, tN), tN, gN, byrow = T))
  # WaveVelocity <- matrix(rep(InList$WaveVelocity, tN), tN, gN, byrow = T)
  tt <- matrix(rep(tt, gN), tN, gN)

  ut = tt / (2 * paramRiverheadNumber * (paramStreamLength / WaveVelocity)^2) *
    exp(-1.0 * WaveVelocity^2 * tt^2 / (4 * paramRiverheadNumber * paramStreamLength^2))
  return(ut)
}

#' Instantaneous unit hydrograph in RUI XioaFang
#' @references 芮孝芳, 石朋. 基于地貌扩散和水动力扩散的流域瞬时单位线研究[J]. 水科学进展, 2002(13):439-444.
#' @param tt numrica,time
#' @param InList list of Input data
#' @return Instantaneous unit hydrograph
#' @export fctIUH.RuiXF
#' @export
fctIUH.RuiXF<-function(tt, InList){
  RiverLength <- InList$RiverLength
  AttenuationCoeffcient <- InList$IUHRuiXFAttenuation
  WaveVelocity <- InList$WaveVelocity
  ut <- RiverLength / (4 * pi * AttenuationCoeffcient * tt^3)^0.5 *
    exp(-1.0 * (WaveVelocity * tt - RiverLength)^2/(4 * AttenuationCoeffcient * tt)) #km2/h
  return(ut)
}


#' make period UH from IUH
#' @param f list of parameter for every IUH
#' @param lower lower list of IUH methond name
#' @param upper upper boundry
#' @param ... other parmters for IUH
#' @param subdivisions subdivisions
#' @return list of Parameter for all UH mit class name
#' @export
PUH <- function(f, lower, upper, ...,subdivisions = 100L){
  f <- match.fun(f)
  wd <- (upper - lower) / (subdivisions - 1)
  LowUpp <- seq(lower, upper, by = wd)
  MatOut <- f(LowUpp, ...)
  MatOut <- as.matrix(MatOut)
  Out <- (colSums(MatOut) - (MatOut[1,] + MatOut[subdivisions,]) / 2) * wd
  return(Out)
}

#' make Discrete unit hydrograph durch Instantaneous unit hydrograph
#' @importFrom stats integrate
#' @param UHMethond list of UH Methond
#' @param UHParam indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return matrix, Discrete unit hydrograph
#' @export
makeUH <- function(UHMethond, UHParam, Param){
  UHPeriodN <- Param$UHPeriodN
  GN <- length(UHParam[[1]])
  InList <- UHParam
  fctIUH <- paste0("fctIUH.",UHMethond)
  UH <- matrix(0.0, UHPeriodN, GN)
  bondh = bondl = 0.0
  for(i in 1:UHPeriodN){
    bondh <- bondl + 1
    Term <- PUH(fctIUH, bondl, bondh, InList = InList)
    # UH[i,] <- round(Term,7)
    UH[i,] <- Term
    bondl <- bondh
  }
  return(UH)
}

#' make all Discrete unit hydrograph durch Instantaneous unit hydrograph
#' @importFrom purrr map2
#' @param InData indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @param ... other Parmeters
#' @return matrix, Discrete unit hydrograph
#' @export
makeUHALL <- function(InData, Param, runMode = "RUN", viewGN = 3, ...){
  return(UHAll = map2(InData$IUH$UHMethond, InData$IUH$UHParam, makeUH, Param))
}








