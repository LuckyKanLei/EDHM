#' @title SNOW
#' @description This routine was written to handle the various calls and data
#' handling needed to solve the various components of the new VIC
#' snow code for both the full_energy and water_balance models.
#' @importFrom  HMtools mergeData deleteData viewArgum checkData
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... other arguments
#' @return use SNOW(runMode = "VIEW") view the outputs and theirs structure
#' @export
SNOW <- function(InData, ...) UseMethod("SNOW", InData)

#' @title SNOW
#' @description This routine was written to handle the various calls and data
#' handling needed to solve the various components of the new VIC
#' snow code for both the full_energy and water_balance models.
#' @importFrom HMtools mergeData deleteData viewArgum checkData
#' @importFrom purrr reduce
#' @param InData indata list, use SNOW(runMode = "VIEW") view the variables and theirs structures
#' param Options Optionslist, alle the options for model is TURE seted as default,
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
#' @param ... other Parmeters
#' @return use SNOW(runMode = "VIEW") view the outputs and theirs structure
#' @export SNOW.VIC
#' @export
SNOW.VIC <- function(InData, Param, runMode = "RUN", viewGN = 3, ...) {
  ## "VIEW" and "CHECK" mode ####
  if(runMode == "VIEW" | runMode == "CHECK"){
    fcName <- "SNOW.VIC"

    ## SNOWBlowing ####
    SBOut0 <- SNOWBlowing(runMode = "VIEW", viewGN = viewGN)
    InDataSB <- SBOut0$Arguments$InData
    ParamSB <- SBOut0$Arguments$Param
    OutSB <- SBOut0$Out

    ## SNOWIntercept ####
    SIOut0 <- INTERCEPTION.SNOWVIC(runMode = "VIEW", viewGN = viewGN)
    InDataSI <- SIOut0$Arguments$InData
    ParamSI <- SIOut0$Arguments$Param
    OutSI <- SIOut0$Out

    ## SNOWEnergy ####
    SEOut0 <- SNOWEnergy(runMode = "VIEW", viewGN = viewGN)
    InDataSE <- SEOut0$Arguments$InData
    ParamSE <- SEOut0$Arguments$Param
    OutSE <- SEOut0$Out

    ## SNOWMelt ####
    SMOut0 <- SNOWMelt(runMode = "VIEW", viewGN = viewGN)
    InDataSM <- SMOut0$Arguments$InData
    ParamSM <- SMOut0$Arguments$Param
    OutSM <- SMOut0$Out

    ## SNOWState ####
    SSOut0 <- SNOWState(runMode = "VIEW", viewGN = viewGN)
    InDataSS <- SSOut0$Arguments$InData
    ParamSS <- SSOut0$Arguments$Param
    OutSS <- SSOut0$Out

    InData0 <- InDataSB
    Out0 <- OutSB
    InData0 <- mergeData(InData0, deleteData(InDataSI,  Out0)) ##OutSD no output
    Out0 <- mergeData(Out0, OutSI)
    InData0 <- mergeData(InData0, deleteData(InDataSE,  Out0)) ##OutSI not data,frame
    Out0 <- mergeData(Out0, OutSE)
    InData0 <- mergeData(InData0, deleteData(InDataSM,  Out0))
    Out0 <- mergeData(Out0, OutSM)
    InData0 <- mergeData(InData0, deleteData(InDataSS,  Out0))


    Param0 <- reduce(list(ParamSB, ParamSI, ParamSE, ParamSM, ParamSS), mergeData)
    Arguments0 <- list(InData = InData0, Param = Param0)
    if(runMode == "VIEW"){
      vw <- viewArgum(fcName, Arguments0)
      return(list(Arguments = Arguments0, Out = vw))
    } else {
      message("The above information can be ignored.\n")
      ck <- checkData(Arguments0, list(InData = InData, Param = Param), "Arguments")
      return()
    }
  }
  ## "RUN" mode ####
  ## SNOW Blowing ####
  SBOut <- SNOWBlowing(InData, Param)
  InData <- mergeData(InData, SBOut)
  ## SNOW Intercept ####
  SIOut <- INTERCEPTION.SNOWVIC(InData, Param)
  InData <- mergeData(InData, SIOut)
  ## SNOW Energy ####
  SEOut <- SNOWEnergy(InData, Param)
  InData <- mergeData(InData, SEOut)
  ## SNOW Melt ####
  SMOut <- SNOWMelt(InData, Param)
  InData <- mergeData(InData, SMOut)
  ## SNOW State ####
  SSOut <- SNOWState(InData, Param)

  Out <- reduce(list(SBOut, SIOut, SEOut, SMOut, SSOut), mergeData)
  return(Out)
}





