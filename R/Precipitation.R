#' @title calc_rainonly
#' @description  Determines from the air temperature what fraction of incoming
#' precipitation is frozen and unfrozen (snow and rain).
#' @param air_temp air_temp
#' @param prec precptation
#' @param MAX_SNOW_TEMP MAX_SNOW_TEMP
#' @param MIN_RAIN_TEMP MIN_RAIN_TEMP
#' @return rainfall only
#' @export
calc_rainonly <- function(air_temp,
                          prec,
                          MAX_SNOW_TEMP,
                          MIN_RAIN_TEMP) {
  if (MAX_SNOW_TEMP <= MIN_RAIN_TEMP) {
    stop("MAX_SNOW_TEMP must be greater then MIN_RAIN_TEMP")
  }
  judgeTMM <- (air_temp < MAX_SNOW_TEMP & air_temp > MIN_RAIN_TEMP)
  judgeMax <- (air_temp >= MAX_SNOW_TEMP)
  rainonly <- ((air_temp - MIN_RAIN_TEMP) / (MAX_SNOW_TEMP - MIN_RAIN_TEMP) * prec) * judgeTMM + prec * judgeMax
  return(rainonly)
}

#' @title Precipitation Divid
#' @description  Determines from the air temperature what fraction of incoming
#' precipitation is frozen and unfrozen (snow and rain).
#' @importFrom stats runif
#' @param InData indata list, use SNOWDivid(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWDivid(runMode = "VIEW") view the structure
#' @return use SNOWDivid(runMode = "VIEW") view the outputs and theirs structure
#' @export
PRECDivid <- function(InData, Param) {
  # if(runMode == "VIEW" | runMode == "CHECK"){
  #   fcName <- "PRECDivid"
  #   MetData <- data.frame(Precipitation = runif(viewGN, 0, 90), TAir = runif(viewGN, -30, 50))
  #   MetData <- putUnit(MetData, c("mm", "Cel"))
  #
  #   InData0 <- list(MetData = MetData)
  #   Param0 <- list(SNOW_MAX_SNOW_TEMP = ParamAll$SNOW_MAX_SNOW_TEMP, SNOW_MIN_RAIN_TEMP = ParamAll$SNOW_MIN_RAIN_TEMP)
  #   Arguments <- list(InData = InData0, Param = Param0)
  #
  #   if(runMode == "VIEW"){
  #     vw <- viewArgum(fcName, Arguments)
  #     return(list(Arguments = Arguments, Out = vw))
  #   } else {
  #     ck <- checkData(Arguments, list(InData = InData, Param = Param), "Arguments")
  #     return()
  #   }
  # }

  prec <- InData$Prec$Precipitation
  air_temp <- InData$MetData$TAir
  MAX_SNOW_TEMP <- Param$Max_Snow_T
  MIN_RAIN_TEMP <- Param$Min_Snow_T
  #### Calculate Fraction of Precipitation that falls as Rain ####
  rainonly <- calc_rainonly(air_temp, prec, MAX_SNOW_TEMP, MIN_RAIN_TEMP)
  snowfall <- prec - rainonly
  snowfall <- snowfall * (!(snowfall < 1e-5))
  rainfall <- prec - snowfall
  return(list(Prec = list(RainFall = rainfall, SnowFall = snowfall)))
}
