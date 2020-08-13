#' @title SNOWEnergy
#' @description Calculate the surface energy balance for the snow pack.
#' @importFrom stats runif
#' @importFrom  HMtools mergeData deleteData viewArgum checkData putUnit
#' @param InData indata list, use SNOWEnergy(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOWEnergy(runMode = "VIEW") view the structure
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use SNOWEnergy(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOWEnergy <- function(InData, Param, runMode = "RUN", viewGN = 3) {
  ## "VIEW" and "CHECK" mode ####
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOWEnergy"
    Snow <- data.frame(Coverage = rep(0, viewGN), LastSnow = rep(1, viewGN),
                       MELTING = rep(F, viewGN), SWQ = rep(0, viewGN))
    Energy <- data.frame(ColdContent = runif(viewGN, 0, 5), TCanopy = rep(0, viewGN),
                         TSurf = rep(0, viewGN), TSnow = rep(0, viewGN))

    SnowFall <- runif(viewGN, 0, 50)

    VegData <- data.frame(WindAttenuation = runif(viewGN, 0.4, 0.6), Albedo = runif(viewGN, 0.1, 0.2), IsOverstory = rep(T, viewGN))

    MetData <- data.frame(ShortWave = runif(viewGN, 1, 2), LongWave = runif(viewGN, 80, 85), TAir = runif(viewGN, -30, 50))
    MetData <- putUnit(MetData, c("W/m2", "W/m2", "Cel"))
    InData0 <- list(Snow = Snow, Energy = Energy, Prec = data.frame(SnowFall = SnowFall),
                    VegData = VegData, MetData = MetData)
    Param00 <- (snow_albedo(runMode = "VIEW"))$Arguments$Param
    Param0 <- mergeData(Param00, list(GridN = viewGN, TimeStepSec = 3600))
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
  BareAlbedo <- InData$VegData$Albedo
  overstory <- InData$VegData$IsOverstory
  surf_atten <- InData$VegData$WindAttenuation

  Snow <- InData$Snow
  SnowFall <- InData$Prec$SnowFall

  shortwave <- ShortUnderIn <- InData$MetData$ShortWave

  dt <- Param$TimeStepSec

  Energy <- list()

  InData$Energy$TAir <- Energy$TAir <- InData$MetData$TAir
  Energy$TCanopy <- (InData$Energy$TCanopy + InData$Energy$TAir) / 2. ##*## lk selbst geschrieben.
  Energy$TGrnd <- (InData$Energy$TSurf + InData$Energy$TAir) / 2. ##*## lk selbst geschrieben.
  Energy$OldTSurf <- Energy$TSurf
  Energy$TSurf <- Energy$TGrnd
  Energy$LongSnowIn <- Energy$LongOverIn <- InData$MetData$LongWave
  IsSnow <- (Snow$SWQ > 0.0 | SnowFall > 0)
  indexOS <- which(IsSnow & overstory)
  Energy$TSurf[indexOS] <- InData$Energy$TSnow[indexOS]
  Energy$VaporizLatentHeat <- calc_latent_heat_of_vaporization(InData$Energy$TSurf)
  surf_atten[which(!overstory)] <- 1. #### understory covered by snow
  coverage <- Snow$Coverage
  #### Compute canopy interception of precipitation ####
  ShortUnderIn[indexOS] <- (ShortUnderIn * surf_atten)[indexOS] #### SW transmitted through canopy
  ShortOverIn <- (1. - surf_atten) * shortwave #### canopy incident SW
  Energy$ShortOverIn <- ShortOverIn
  Energy$ShortUnderIn <- ShortUnderIn
  #### compute understory albedo and net shortwave radiation ####
  judgeLA <- (Snow$SWQ > 0 & SnowFall == 0)
  Snow$LastSnow <- (Snow$LastSnow + 1) * judgeLA + 1
  SnowAlbedo <- (snow_albedo(dt,
                             SnowFall, Snow$SWQ,
                             InData$Energy$ColdContent,
                             Snow$LastSnow, Snow$MELTING,
                             Param)) * judgeLA + Param$SNOW_NEW_SNOW_ALB * (!judgeLA)
  AlbedoUnder <- (coverage * SnowAlbedo + (1. - coverage) * BareAlbedo) * judgeLA + SnowAlbedo * (!judgeLA)
  NetShortSnow <- (1.0 - AlbedoUnder) * (ShortUnderIn)
  Energy$NetShortSnow <- NetShortSnow
  return(list(Energy = as.data.frame(Energy),
              Snow = data.frame(LastSnow = Snow$LastSnow)))
}

