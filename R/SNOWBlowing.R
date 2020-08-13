#' @title SNOWBlowing
#' @description Calculate energy of sublimation from blowing snow.
#' Calculate sublimation from blowing snow
#' @importFrom stats runif
#' @importFrom  HMtools mergeData deleteData viewArgum checkData putUnit
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWIntercept(runMode = "VIEW") view the structure
#' and use SNOWBlowing(runMode = "VIEW") view the options structure and set the options
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use SNOWIntercept(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOWBlowing <- function(InData, Param, runMode = "RUN", viewGN = 3) {
  ## "VIEW" and "CHECK" mode ####
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOWBlowing"

    Snow <- data.frame(Depth = rep(0, viewGN), LastSnow = rep(1, viewGN), SurfWater = rep(0, viewGN))
    Snow <- putUnit(Snow, c("m", "timeStep", "m"))
    Energy <- data.frame(TSnow = rep(0, viewGN))
    Aerodyna <- data.frame(ReferHeightSnow = runif(viewGN, 2, 2), RoughCanopy = runif(viewGN, 0.02, 0.52),
                           RoughSnow = runif(viewGN, 0.002, 0.0052), DisplacCanopy = runif(viewGN, 0.2, 1.7), WindSpeedSnow = runif(viewGN, 2, 17))
    MetData <- data.frame(TAir = rep(0, viewGN), AirDensity = runif(viewGN, 1, 2), VaporPressure = runif(viewGN, 0.01, 0.2))
    MetData <- putUnit(MetData, c("Cel", "kg/m3", "kPa"))
    VegData <- data.frame(Fetch = runif(viewGN, 0.1, 0.3))
    VegData <- putUnit(VegData, c("100%"))
    LandData <- data.frame(LagOneSlope = runif(viewGN, 0.5, 0.5), SigmaSlope = runif(viewGN, 0.6, 0.8))
    LandData <- putUnit(LandData, c("100%", "100%"))


    InData0 <- list(Snow = Snow, Energy = Energy, Aerodyna = Aerodyna,
                    MetData = MetData, VegData = VegData, LandData = LandData)
    Param0 <- list(BLOWING_CSALT = ParamAll$BLOWING_CSALT, BLOWING_KA = ParamAll$BLOWING_KA,
                   BLOWING_NUMINCS = ParamAll$BLOWING_NUMINCS,
                   BLOWING_MAX_ITER = ParamAll$BLOWING_MAX_ITER, BLOWING_SETTLING = ParamAll$BLOWING_SETTLING,
                   BLOWING_UTHRESH = ParamAll$BLOWING_UTHRESH, BLOWING_KIN_VIS = ParamAll$BLOWING_KIN_VIS,
                   SVP_A = ParamAll$SVP_A, SVP_B = ParamAll$SVP_B, SVP_C = ParamAll$SVP_C,
                   GridN = viewGN,
                   TimeStepSec = 3600,
                   BLOWING_CALC_PROB = TRUE, BLOWING_FETCH = TRUE, BLOWING_SIMPLE = TRUE,
                   BLOWING_SPATIAL_WIND = TRUE, BLOWING_VAR_THRESHOLD = TRUE)
    Arguments <- list(InData = InData0, Param = Param0)
    if(runMode == "VIEW"){
      vw <- viewArgum(fcName, Arguments)
      return(list(Arguments = Arguments, Out = vw))
    } else {
      ck <- checkData(Arguments, list(InData = InData, Param = Param), "Arguments")
      return()
    }
  }



  ## "RUN" mode ####
  ## initialize variable ####
  Tair <- InData$MetData$TAir
  AirDens <- InData$MetData$AirDensity
  EactAir <- InData$MetData$VaporPressure

  lag_one <- InData$LandData$LagOneSlope
  sigma_slope <- InData$LandData$SigmaSlope

  fe <- InData$VegData$Fetch

  displacement <- InData$Aerodyna$DisplacCanopy
  roughness <- InData$Aerodyna$RoughCanopy
  Wind <- InData$Aerodyna$WindSpeedSnow
  ZO <- InData$Aerodyna$RoughSnow
  Zrh <- InData$Aerodyna$ReferHeightSnow

  Tsnow <- InData$Energy$TSnow

  snowdepth <- InData$Snow$Depth
  LastSnow <- InData$Snow$LastSnow
  SurfaceLiquidWater <- InData$Snow$SurfWater

  Dt <- Param$TimeStepSec

  InData$Energy$SublimatLatentHeat <- calc_latent_heat_of_sublimation(Tair)
  Ls <- InData$Energy$SublimatLatentHeat

  #### Calculate some general variables, that don't depend on wind speed. ####
  #### Age in hours ####
  Age <- LastSnow * Dt / SEC_PER_HOUR

  #### Saturation density of water vapor, Liston A-8 ####
  es <- calc_saturated_vapor_pressure(Tair, Param)

  Tk <- Tair + CONST_TKFRZ

  Ros <- CONST_EPS * es / (CONST_RDAIR * Tk)

  #### Diffusivity in m2/s, Liston eq. A-7 ####
  Diffusivity <- (2.06e-5) * (Tk / 273.)^1.75

  #### Essery et al. 1999, eq. 6 (m*s/kg)
  FT <- (Ls / (Param$BLOWING_KA * Tk)) * (Ls * Tk / CONST_RDAIR - 1.)
  FT <- FT + 1. / (Diffusivity * Ros)

  #### grid cell 10 m wind speed <- 50th percentile wind ####
  #### Wind speed at 2 m above snow was passed to this function. ####

  wind10 <- Wind * log(10. / (ZO + DBL_EPSILON)) / log((2 + ZO) / (ZO + DBL_EPSILON))

  #### Check for bare soil case. ####
  # if (iveg == Nveg) {
  #     fe <- 1500
  #     sigma_slope <- .0002
  # }
  #### sigma_w/uo:
  ratio <- (2.44 - (0.43) * lag_one) * sigma_slope

  sigma_w <- wind10 * ratio
  Uo <- wind10
  #### Parameters for roughness above snow. ####
  hv <- (3. / 2.) * displacement
  Nd <- (4. / 3.) * (roughness / displacement)

  #### Begin loop through wind probability function. ####

  area <- 1. / Param$BLOWING_NUMINCS

  U10 <- Uo
  #### Calculate parameters for probability of blowing snow occurence. ####
  #### ( Li and Pomeroy 1997) ####
  judgeHV <- (snowdepth < hv)
  Uveg <- (U10 / sqrt(1. + 170 * Nd * (abs(hv - snowdepth)))) * judgeHV + U10 * (!judgeHV)
  prob_occurence <- get_prob(Tair, Age, SurfaceLiquidWater, Uveg)
  if(!Param$BLOWING_CALC_PROB) prob_occurence <- 1.
  #### Calculate threshold shear stress. Send 0 for constant or  ####
  #### 1 for variable threshold after Li and Pomeroy (1997)      ####

  utshear <- get_thresh(Tair, SurfaceLiquidWater, ZO)
  if(!Param$BLOWING_VAR_THRESHOLD) utshear <- Param$BLOWING_UTHRESH

  #### Iterate to find actual shear stress during saltation. ####

  ShearOut <- shear_stress(Uo, ZO, utshear, Param$BLOWING_MAX_ITER)
  ushear <- ShearOut$ushear
  Zo_salt <- ShearOut$Zo_salt
  judgeUT <- (ushear > utshear)
  SubFluxOut <- CalcSubFlux(EactAir, es, Zrh, AirDens, utshear,
                            ushear, fe, Uo, Zo_salt, FT, Param)
  SubFlux <- (SubFluxOut$SubFlux) * judgeUT
  Transport <- SubFluxOut$Transport
  Transport[which(!judgeUT)] <- 0.0
  Total <- SubFlux * prob_occurence
  TotalTransport <- Transport * prob_occurence

  if (Param$BLOWING_SPATIAL_WIND) {
    dim2 <- Param$BLOWING_NUMINCS
    indexHalb <- as.integer(dim2 / 2)
    SubFlux <- LowBundry <- UppBundry <- matrix(0.0, sum(sigma_w != 0.), dim2)
    indexW0 <- which(sigma_w != 0.)
    #### Find the limits of integration. ####
    LowBundry[, 1] <- -9999
    UppBundry[, 1] <- Uo + sigma_w * log(2. * area)
    LowBundry[, 2:indexHalb] <- mapply(function(p) Uo + sigma_w * log(2. * (p - 1) * area), 2:indexHalb)
    UppBundry[, 2:indexHalb] <- mapply(function(p) Uo + sigma_w * log(2. * p * area), 2:indexHalb)
    LowBundry[, (indexHalb + 1): (dim2 - 1)] <- mapply(function(p) Uo - sigma_w * log(2. - 2. * ((p - 1) * area)), (indexHalb + 1): (dim2 - 1))
    UppBundry[, (indexHalb + 1): (dim2 - 1)] <- mapply(function(p) Uo - sigma_w * log(2. - 2. * (p * area)), (indexHalb + 1): (dim2 - 1))
    LowBundry[, dim2] <- Uo - sigma_w * log(2. - 2. * area)
    UppBundry[, dim2] <- 9999

    #### Find expected value of wind speed for the interval. ####
    U10 <- UoTem <- colRep(Uo[indexW0], dim2)
    sigma_w <- colRep(sigma_w[indexW0], dim2)
    snowdepth <- colRep(snowdepth[indexW0], dim2)
    hv <- colRep(hv[indexW0], dim2)
    Nd <- colRep(Nd[indexW0], dim2)
    Tair <- colRep(Tair[indexW0], dim2)
    Age <- colRep(Age[indexW0], dim2)
    SurfaceLiquidWater <- colRep(SurfaceLiquidWater[indexW0], dim2)
    ZO <- colRep(ZO[indexW0], dim2)
    ushear <- colRep(ushear[indexW0], dim2)
    Zo_salt <- colRep(Zo_salt[indexW0], dim2)
    EactAir <- colRep(EactAir[indexW0], dim2)
    es <- colRep(es[indexW0], dim2)
    Zrh <- colRep(Zrh[indexW0], dim2)
    AirDens <- colRep(AirDens[indexW0], dim2)
    fe <- colRep(fe[indexW0], dim2)
    Tsnow <- colRep(Tsnow[indexW0], dim2)
    FT <- colRep(FT[indexW0], dim2)
    Transport <- colRep(Transport[indexW0], dim2)


    U10[which(LowBundry >= UoTem)] <- (-0.5 * ((UppBundry + sigma_w) * exp((-1. / sigma_w) * (UppBundry - UoTem)) -
                                                 (LowBundry + sigma_w) *
                                                 exp((-1. / sigma_w) * (LowBundry - UoTem))) / area)[which(LowBundry >= UoTem)]
    U10[which(LowBundry >= UoTem)] <- (0.5 * ((UppBundry - sigma_w) * exp((1. / sigma_w) * (UppBundry - UoTem)) -
                                                (LowBundry - sigma_w) *
                                                exp((1. / sigma_w) * (LowBundry - UoTem))) / area)[which(LowBundry >= UoTem)]

    U10 <- maxSVector(0.4, minSVector(25,U10))
    #### Calculate parameters for probability of blowing snow occurence. ####
    #### ( Li and Pomeroy 1997) ####
    judgeHV <- (snowdepth < hv)
    Uveg <- (U10 / sqrt(1. + 170 * Nd * abs(hv - snowdepth))) * judgeHV + U10 * (!judgeHV)


    prob_occurence <- get_prob(Tair, Age, SurfaceLiquidWater, Uveg)
    if(!Param$BLOWING_CALC_PROB) prob_occurence <- 1.

    #### Calculate threshold shear stress. Send 0 for constant or  ####
    #### 1 for variable threshold after Li and Pomeroy (1997)      ####

    utshear <- get_thresh(Tair, SurfaceLiquidWater, ZO)
    if(!Param$BLOWING_VAR_THRESHOLD) utshear <- Param$BLOWING_UTHRESH

    #### Iterate to find actual shear stress during saltation. ####
    U10[which(is.na(U10))] <- 0.
    ShearOut <- shear_stress(U10, ZO, utshear, Param$BLOWING_MAX_ITER) ##?##
    ushear <- ShearOut$ushear
    Zo_salt <- ShearOut$Zo_salt

    judgeUU <- (ushear > utshear)
    SubFluxOut2 <- CalcSubFlux(EactAir, es, Zrh, AirDens, utshear,
                               ushear, fe, U10, Zo_salt, FT, Param)
    SubFlux <- (SubFluxOut2$SubFlux) * judgeUU
    Transport <- SubFluxOut2$Transport
    Transport[which(!judgeUU)] <- 0.

    Total[indexW0] = rowSums((1. / Param$BLOWING_NUMINCS) * SubFlux * prob_occurence)
    TotalTransport[indexW0] = rowSums((1. / Param$BLOWING_NUMINCS) * Transport * prob_occurence)

  }


  Total <- maxSVector(-.00005, Total)

  judgeSD <- (snowdepth > 0.0)
  TotalTransport <- TotalTransport * judgeSD
  Total <- Total * judgeSD
  TotalTransport <- rowSums(TotalTransport)
  Total <- rowSums(Total)
  TotalTransport[which(abs(TotalTransport) < DBL_EPSILON)] <- 0.
  Total[which(abs(Total) < DBL_EPSILON)] <- 0.
  return(list(Atmos = list(PVegVaporFlux = TotalTransport * MM_PER_M,
              Blowing = Total * MM_PER_M)))
}



#' @title rtnewt
#' @description Newton-Raphson method.
#' the function modify from VIC original rescouce,
#' check the rescouce code in https://github.com/UW-Hydro/VIC
#' @param x1 not klar
#' @param x2 not klar
#' @param acc not klar
#' @param Ur not klar
#' @param Zr not klar
#' @param BLOWING_MAX_ITER not klar
#' @return not klar
#' @export
rtnewt <- function(x1,
                   x2,
                   acc,
                   Ur,
                   Zr,
                   BLOWING_MAX_ITER) {
  fl <- (get_shear(x1, Ur, Zr))$f
  fh <- (get_shear(x2, Ur, Zr))$f
  Ret <- Ur
  Judge <- rep(0.0, length(Ret))

  if (sum((fl > 0.0 & fh > 0.0) | (fl < 0.0 & fh < 0.0)) > 0) errorCondition("Root must be bracketed")
  Ret[which(fl == 0.0)] <- x1[which(fl == 0.0)]
  Ret[which(fh == 0.0)] <- x2[which(fh == 0.0)]
  Judge[which(fl == 0.0 | fh == 0.0)] <- 1.
  xl <- x1
  xh <- x2

  indexFLS <- which((fl < 0.0))
  indexFHS <- which((fh < 0.0))
  xl[indexFLS] <- x1[indexFLS]
  xh[indexFLS] <- x2[indexFLS]
  xh[indexFHS] <- x1[indexFHS]
  xl[indexFHS] <- x2[indexFHS]

  rts <- 0.5 * (x1 + x2)
  dxold <- abs(x2 - x1)
  dx <- dxold
  sherOut <- get_shear(rts, Ur, Zr)
  df <- sherOut$df
  f <- sherOut$f

  for (j in 1 : BLOWING_MAX_ITER) {
    judgeRT <- ((((rts - xh) * df - f) * ((rts - x1) * df - f) > 0.0) |
                  (abs(2.0 * f) > abs(dxold * df)))
    dxold <- dx
    dx <- (0.5 * (xh - xl)) * judgeRT + (f / df) * (!judgeRT)
    rts <- (xl + dx) * judgeRT + (rts - dx) * (!judgeRT)
    indexR <- which(((judgeRT & (xl == rts)) | (!judgeRT & f == 0.)) & (Judge == 0.))
    Ret[indexR] <- rts[indexR]
    Judge[indexR] <- 1.
    indexR2 <- (abs(dx) < acc) & (Judge == 0.)
    Ret[indexR2] <- rts[indexR2]
    Judge[indexR2] <- 1.

    #### if(rts < .025) rts=.025
    f <- (get_shear(rts, Ur, Zr))$f
    df <- (get_shear(rts, Ur, Zr))$df
    xl[which(f < 0.0)] <- rts[which(f < 0.0)]
    xh[which(!(f < 0.0))] <- rts[which(!(f < 0.0))]
  }
  if (sum(Judge) != length(Judge)) errorCondition("Maximum number of iterations exceeded")
  return(Ret)
}

#' @title get_shear
#' @description    This routine resets the values of all output variables to 0.
#' the function modify from VIC original rescouce,
#' check the rescouce code in https://github.com/UW-Hydro/VIC
#' @param x not klar
#' @param Ur not klar
#' @param Zr not klar
#' @return the values of all output variables to 0
#' @export
get_shear <- function(x,
                      Ur,
                      Zr)
{
  f <- log(2. * CONST_G * Zr / .12) + log(1 / (x * x)) - CONST_KARMAN * Ur / x
  df <- CONST_KARMAN * Ur / (x * x) - 2. / x
  f[which(is.na(f))] <- 0
  df[which(is.na(df))] <- 0
  return(list(f = f, df = df))
}

#' @title sub_with_height
#' @description    Calculate the sublimation rate for a given height above the boundary layer.
#' @param z not klar
#' @param es not klar
#' @param Wind not klar
#' @param EactAir not klar
#' @param FT not klar
#' @param hsalt not klar
#' @param phi_r not klar
#' @param ushear not klar
#' @param Param not klar
#' @return the sublimation rate for a given height above the boundary layer
#' @export
sub_with_height <- function(z,
                            es,
                            Wind,
                            EactAir,
                            FT,
                            hsalt,
                            phi_r,
                            ushear,
                            Param){
  #### Calculate sublimation loss rate (1/s)
  Rrz <- 4.6e-5 * z^-.258
  ALPHAz <- 4.08 + 12.6 * z
  Mz <- (4. / 3.) * CONST_PI * CONST_RHOICE * Rrz * Rrz * Rrz *
    (1. + (3. / ALPHAz) + (2. / (ALPHAz * ALPHAz)))

  Rmean <- ((3. * Mz) / (4. * CONST_PI * CONST_RHOICE))^(1. / 3.)

  #### Pomeroy and Male 1986
  terminal_v <- 1.1e7 * Rmean^1.8

  #### Pomeroy (1988)
  fluctuat_v <- 0.005 * Wind^1.36

  #### Ventilation velocity for turbulent suspension Lee (1975)
  Vtz <- terminal_v + 3. * fluctuat_v * cos(CONST_PI / 4.)

  Re <- 2. * Rmean * Vtz / Param$BLOWING_KIN_VIS
  Nu <- 1.79 + 0.606 * Re^0.5

  #### LCB: found error in rh calc, 1/20/04, check impact
  sigz <- ((EactAir / es) - 1.) * (1.019 + .027 * log(z))

  dMdt <- 2 * CONST_PI * Rmean * sigz * Nu / FT
  #### sublimation loss rate coefficient (1/s)

  psi_t <- dMdt / Mz

  #### Concentration of turbulent suspended snow Kind (1992)

  temp <- (0.5 * ushear * ushear) / (Wind * Param$BLOWING_SETTLING)
  phi_t <- phi_r * ((temp + 1.) * (z / hsalt)^((-1. * Param$BLOWING_SETTLING) / (CONST_KARMAN * ushear)) - temp)

  return (psi_t * phi_t)
}

#' @title get_prob
#' @description Calculate parameters for probability of blowing snow occurence.
#' @references see Li and Pomeroy 1997
#' @param Tair Temperatur of air
#' @param Age age of snow in hour
#' @param SurfaceLiquidWater SurfaceLiquidWater
#' @param U10 not klar
#' @return parameters for probability of blowing snow occurence
#' @export
get_prob <- function(Tair,
                     Age,
                     SurfaceLiquidWater,
                     U10) {
  judgeSW <- (SurfaceLiquidWater < 0.001)
  mean_u_occurence <- (11.2 + 0.365 * Tair + 0.00706 * Tair * Tair + 0.9 * log(Age)) * judgeSW + 21. * (!judgeSW)
  sigma_occurence <- (4.3 + 0.145 * Tair + 0.00196 * Tair * Tair) * judgeSW + 7. * (!judgeSW)
  prob_occurence <- 1. / (1. + exp(sqrt(CONST_PI) * (mean_u_occurence - U10) / sigma_occurence))
  prob_occurence[which(prob_occurence < 0.0)] <- 0.0
  prob_occurence[which(prob_occurence > 1.0)] <- 1.0
  return(prob_occurence)
}

#' @title get_thresh
#' @description     Calculate threshold shear stress.
#' @param Tair Temperatur of air
#' @param SurfaceLiquidWater SurfaceLiquidWater
#' @param Zo_salt not klar
#' @return threshold shear stress
#' @export
get_thresh <- function(Tair,
                       SurfaceLiquidWater,
                       Zo_salt) {
  judgeWS <- (SurfaceLiquidWater < 0.001)
  ut10 <- (9.43 + .18 * Tair + .0033 * Tair * Tair) * judgeWS + 9.9 * (!judgeWS)

  return (CONST_KARMAN * ut10 / log(10. / Zo_salt))
}

#' @title shear_stress
#' @description    Iterate to find actual shear stress during saltation.
#' @param U10 not klar
#' @param ZO not klar
#' @param utshear not klar
#' @param BLOWING_MAX_ITER not klar
#' @return threshold shear stress
#' @export
shear_stress <- function(U10,
                         ZO,
                         utshear,
                         BLOWING_MAX_ITER) {
  #### Find min & max shear stress to bracket value. ####
  umin <- utshear
  umax <- CONST_KARMAN * U10
  xacc <- 0.10 * umin

  #### Check to see if value is bracketed. ####
  fl <- (get_shear(umin, U10, 10.))$f
  fh <- (get_shear(umax, U10, 10.))$f
  judgeFB <- (fl > 0.0 & fh > 0.0)
  if (sum(fl < 0.0 & fh < 0.0) > 0)  stop("Solution surpasses upper boundary.",
                                          "fl(%f)=%f, fh(%f)=%f", umin, fl, umax, fh)
  ushear <- (CONST_KARMAN * U10 / log(10. / ZO)) * judgeFB + rtnewt(umin, umax, xacc, U10, 10., BLOWING_MAX_ITER) * (!judgeFB)
  Zo_salt <- ZO * judgeFB + (0.12 * ushear * ushear / (2. * CONST_G)) * (!judgeFB)
  return(list(ushear = ushear, Zo_salt = Zo_salt))
}

#' @title transport_with_height
#' @description     Calculate the transport rate for a given height above the boundary layer.
#' @param z not klar
#' @param Wind not klar
#' @param ZO not klar
#' @param hsalt not klar
#' @param phi_r not klar
#' @param ushear not klar
#' @param Param not klar
#' @return transport rate for a given height above the boundary layer
#' @export
transport_with_height <- function(z,
                                  Wind,
                                  ZO,
                                  hsalt,
                                  phi_r,
                                  ushear,
                                  Param) {
  #### Find wind speed at current height

  u_z <- ushear * log(z / ZO) / CONST_KARMAN

  #### Concentration of turbulent suspended snow Kind (1992)

  temp <- (0.5 * ushear * ushear) / (Wind * Param$BLOWING_SETTLING)
  phi_t <- phi_r * ((temp + 1.) * (z / hsalt)^((-1. * Param$BLOWING_SETTLING) / (CONST_KARMAN * ushear)) - temp)

  return(u_z * phi_t)
}

#' @title CalcSubFlux
#' @description  Calculate the sublimation flux.
#' @param EactAir not klar
#' @param es not klar
#' @param Zrh not klar
#' @param AirDens not klar
#' @param utshear not klar
#' @param ushear not klar
#' @param fe not klar
#' @param U10 not klar
#' @param Zo_salt not klar
#' @param FT not klar
#' @param Param parameters list
#' @return sublimation flux
#' @export
CalcSubFlux <- function(EactAir,
                        es,
                        Zrh,
                        AirDens,
                        utshear,
                        ushear,
                        fe,
                        U10,
                        Zo_salt,
                        FT,
                        Param) {


  particle <- utshear * 2.8
  #### SBSM:
  if (Param$BLOWING_SIMPLE) {
    b <- .25
    undersat_2 <- (((EactAir / es) - 1.) * (1. - .027 * log(Zrh) + 0.027 * log(2))) * (EactAir < es)
    SubFlux <- b * undersat_2 * U10^5. / FT
    Transport <- 0.0 * SubFlux
  } else {
    #### Sublimation flux (kg/m2*s) <- mass-concentration * sublimation rate * height
    #### for both the saltation layer and the suspension layer
    #### Saltation layer is assumed constant with height
    #### Maximum saltation transport rate (kg/m*s)
    #### Liston and Sturm 1998, eq. 6
    Qsalt <- (Param$BLOWING_CSALT * AirDens / CONST_G) *
      (utshear / ushear) * (ushear * ushear - utshear * utshear)
    if (Param$BLOWING_FETCH) {
      Qsalt <- Qsalt * (1. + (500. / (3. * fe)) * (exp(-3. * fe / 500.) - 1.))
    }

    #### Pomeroy and Male (1992)
    hsalt <- 0.08436 * ushear^1.27

    #### Saltation layer mass concentration (kg/m3)
    phi_s <- Qsalt / (hsalt * particle)

    TT <- 0.5 * (ushear * ushear) / (U10 * Param$BLOWING_SETTLING)
    ztop <- hsalt * (TT / (TT + 1.))^((CONST_KARMAN * ushear) / (-1. * Param$BLOWING_SETTLING))

    #### Sublimation loss-rate for the saltation layer (s-1)
    psi_s <- sub_with_height(hsalt / 2., es, U10,
                             EactAir, FT, hsalt,
                             phi_s, ushear,
                             Param)

    #### Sublimation from the saltation layer in kg/m2*s
    SubFlux <- phi_s * psi_s * hsalt

    #### Suspension layer must be integrated

    SubFlux <- SubFlux + integrateVector(sub_with_height, hsalt, ztop,
                                         es, U10,
                                         EactAir, FT, hsalt,
                                         phi_s, ushear,
                                         Param)
    SubFlux <- SubFlux * (EactAir < es)


    #### Transport out of the domain by saltation Qs(fe) (kg/m*s), eq 10 Liston and Sturm
    saltation_transport <- Qsalt * (1 - exp(-3. * fe / 500.))

    #### Transport in the suspension layer
    suspension_transport <- integrateVector(transport_with_height, Zrh, hsalt, ztop,
                                            es, U10, AirDens, Zo_salt,
                                            EactAir, FT, hsalt, phi_s, ushear,
                                            Param)

    #### Transport at the downstream edge of the fetch in kg/m*s
    Transport <- (suspension_transport + saltation_transport)
    if (Param$BLOWING_FETCH) {
      Transport <- Transport / fe
    }

  }
  return(list(SubFlux = SubFlux, Transport = Transport))


}

