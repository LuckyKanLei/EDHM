#' find the data structure of a module or model
#' @param funcName function(model or module complete name), in character e.g. "Model.CVIC"
#' @param data  "All" is default, or:"InData", "Param", "OutData",
#' @return the data structure of the function's data
#' @export
get_ModuleData <- function(funcName, data = "All"){
  if(data == "All") return(eval(parse(text =
                                        paste0("Data_", funcName))))
  else return(eval(parse(text =
                           paste0("Data_", funcName, "$", data))))
}

#' find the data structure of a model, that new created
#' @param Modules a list, names are the module name and the values are the process methond
#' @return the data structure of the model's data
#' \itemize{
#' \item InputData, the data that from outside of the model, must prepared
#' \item In_InitialData, the data that must judge, wether they from the outside or the state data
#' \item InData, all InData of all modules
#' \item OutData, all the OutData of all modules 
#' \item Param, all parameters that the model needed
#' }
#' @export
get_ModelData <- function(Modules) {
  moduleN <- length(Modules)
  moduleName <- names(Modules)
  dataName <- paste0("Data_", moduleName, ".", Modules)

  InData <- t_vari.hm.list()
  OutData <- t_vari.hm.list()
  Param <- list()
  for (i in 1:moduleN) {
    eval(parse(text =
                 paste0("InData <- left_merge(InData,",
                        dataName[i],
                        "$InData)")))
    eval(parse(text =
                 paste0("OutData <- left_merge(OutData,",
                        dataName[i],
                        "$OutData)")))
    eval(parse(text =
                 paste0("Param <- left_merge(Param,",
                        dataName[i],
                        "$Param)")))
  }
  for (i in 1:moduleN) {
    eval(parse(text =
                 paste0("InData <- set_judge(InData, 1,",
                        dataName[i],
                        "$InData)")))
    eval(parse(text =
                 paste0("InData <- set_judge(InData, 2,",
                        dataName[i],
                        "$OutData)")))
  }

  InputData <- InData
  In_InitialData <- InData
  InName <- names(InData)
  for (i in InName) {
    SubName <- names(InData[[i]])
    for (j in SubName) {
      if((InData[[i]][[j]])[[3]] == 1){
        InputData[[i]][j] <- NULL
        In_InitialData[[i]][j] <- NULL
      } else if((InData[[i]][[j]])[[2]] == 1){
        InputData[[i]][j] <- NULL
      } else In_InitialData[[i]][j] <- NULL
    }
    if(length(InputData[[i]]) == 0) InputData[[i]] <- NULL
    if(length(In_InitialData[[i]]) == 0) In_InitialData[[i]] <- NULL
  }

  return(list(InputData = InputData,
              In_InitialData = In_InitialData,
              InData = InData,
              OutData = OutData,
              Param = Param))
}

# indaText <- read.table(text = "  PrecipitationHoch <- InData$Prec$Precipitation
#   SoilMoistureCapacity <- InData$Ground$MoistureCapacity
#   SoilMoistureCapacityMax <- InData$Ground$MoistureCapacityMax
#   InfiltrationRateMax <- InData$Infilt$InfiltrationRateMax
# ", sep = "$")
#
# paText <- read.table(text = "  paSoilMoistureCapacityB <- Param$SoilMoistureCapacityB
#   paInfiltrationRateB <- Param$InfiltrationRateB
# ", sep = "$")
#
# textBoom(indaText, paText)
# textBoom <- function(indaText, paText){
#   subList <- table(indaText[,2])
#   subN <- length(subList)
#   subdf <- as.data.frame(table(indaText[,2]))
#   for(i in 1:subN){
#     subdf[i,2] <- paste0(indaText[which(indaText[,2] == subdf[i,1]),3], " = 0", collapse = ",\n")
#   }
#   inda1 <- "Data_ <- \nlist(InData = left_merge(hm.list(),\nlist("
#   inda <- paste0(inda1, paste0(subdf[,1], " = list(", subdf[,2], ")", collapse = ",\n"),")),\nParam = list(")
#   pa <- paste0(paText[,2], " = 0", collapse = ",")
#   out <- "),\nOutData = left_merge(hm.list(),\nlist()))"
#   cat(paste0(inda, pa, out))
# }














