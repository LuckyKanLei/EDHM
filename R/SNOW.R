#' @title SNOW
#' @description This routine was written to handle the various calls and data
#' handling needed to solve the various components of the new VIC
#' snow code for both the full_energy and water_balance models.
#' @importFrom  HMtools mergeData deleteData viewArgum checkData
#' @param InData indata list, use SNOW(runMode = "VIEW") view the variables and theirs structures
#' @param Options Optionslist, alle the options for model is TURE seted as default,
#' and use SNOW(runMode = "VIEW") view the options structure and set the options
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' the other parameters depednd on the actuell model, eg. TimeStepSec, gridN.
#' and use SNOW(runMode = "VIEW") view the structure
#' @param runMode mode to run the function, there three mode:
#' \itemize{
#' \item "RUN": default, run the function like general faunction
#' \item "VIEW": view the structures of Arguments and Output(return)
#' \item "CHECK": chek the structure of the Arguments
#' }
#' @param viewGN grid nummer for "VIEW" mode.
#' @return use SNOW(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOW <- function(InData, Param, Options, runMode = "RUN", viewGN = 3) {
  ## "VIEW" and "CHECK" mode ####
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOW"

    ## SNOWBlowing ####
    SBOut0 <- SNOWBlowing(runMode = "VIEW", viewGN = viewGN)
    InDataSB <- SBOut0$Arguments$InData
    ParamSB <- SBOut0$Arguments$Param
    OptionsSB <- SBOut0$Arguments$Options
    OutSB <- SBOut0$Out

    ## SNOWIntercept ####
    SIOut0 <- SNOWIntercept(runMode = "VIEW", viewGN = viewGN)
    InDataSI <- SIOut0$Arguments$InData
    ParamSI <- SIOut0$Arguments$Param
    OptionsSI <- SIOut0$Arguments$Options
    OutSI <- SIOut0$Out

    ## SNOWEnergy ####
    SEOut0 <- SNOWEnergy(runMode = "VIEW", viewGN = viewGN)
    InDataSE <- SEOut0$Arguments$InData
    ParamSE <- SEOut0$Arguments$Param
    OptionsSE <- SEOut0$Arguments$Options
    OutSE <- SEOut0$Out

    ## SNOWMelt ####
    SMOut0 <- SNOWMelt(runMode = "VIEW", viewGN = viewGN)
    InDataSM <- SMOut0$Arguments$InData
    ParamSM <- SMOut0$Arguments$Param
    OptionsSM <- SMOut0$Arguments$Options
    OutSM <- SMOut0$Out

    ## SNOWState ####
    SSOut0 <- SNOWState(runMode = "VIEW", viewGN = viewGN)
    InDataSS <- SSOut0$Arguments$InData
    ParamSS <- SSOut0$Arguments$Param
    OptionsSS <- SSOut0$Arguments$Options
    OutSS <- SSOut0$Out
    InData0 <- InDataSB
    InData0 <- mergeData(InData0, deleteData(InDataSI,  OutSB)) ##OutSD no output
    InData0 <- mergeData(InData0, deleteData(InDataSE,  OutSI)) ##OutSI not data,frame
    InData0 <- mergeData(InData0, deleteData(InDataSM,  OutSE))
    InData0 <- mergeData(InData0, deleteData(InDataSS,  OutSM))


    Param0 <- reduce(list(ParamSB, ParamSI, ParamSE, ParamSM, ParamSS), mergeData)
    Options0 <- reduce(list(OptionsSB, OptionsSI, OptionsSE, OptionsSM, OptionsSS), mergeData)
    Arguments0 <- list(InData = InData0, Param = Param0, Options = Options0)
    if(runMode == "VIEW"){
      vw <- viewArgum(fcName, Arguments0)
      return(list(Arguments = Arguments0, Out = vw))
    } else {
      message("The above information can be ignored.\n")
      ck <- checkData(Arguments0, list(InData = InData, Param = Param, Options = Options), "Arguments")
      return()
    }
  }

  ## "RUN" mode ####
  ## SNOW Blowing ####
  SBOut <- SNOWBlowing(InData, Param, Options)
  InData <- mergeData(InData, SBOut)
  ## SNOW Intercept ####
  SIOut <- SNOWIntercept(InData, Param)
  InData <- mergeData(InData, SIOut)
  ## SNOW Energy ####
  SEOut <- SNOWEnergy(InData, Param)
  InData <- mergeData(InData, SEOut)
  ## SNOW Melt ####
  SMOut <- SNOWMelt(InData, Param, Options)
  InData <- mergeData(InData, SMOut)
  ## SNOW State ####
  SSOut <- SNOWState(InData, Param, Options)

  Out <- reduce(list(SBOut, SIOut, SEOut, SMOut, SSOut), mergeData)
  return(Out)
}

