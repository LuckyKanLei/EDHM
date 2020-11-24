#' find the data structure of a module or model
#' @param funcName function(model or module complete name), in character e.g. "Model.CVIC"
#' @param data  "All" is default, or:"InData", "Param", "OutData",
#' @return the data structure of the function's data
#' @export
mod_data <- function(funcName, data = "All"){
  if(data == "All") return(eval(parse(text =
                                        paste0("Data_", funcName))))
  else return(eval(parse(text =
                           paste0("Data_", funcName, "$", data))))
}

indaText <- read.table(text = "  PrecipitationHoch <- InData$Prec$Precipitation
  SoilMoistureCapacity <- InData$Ground$MoistureCapacity
  SoilMoistureCapacityMax <- InData$Ground$MoistureCapacityMax
  InfiltrationRateMax <- InData$Infilt$InfiltrationRateMax
", sep = "$")

paText <- read.table(text = "  paSoilMoistureCapacityB <- Param$SoilMoistureCapacityB
  paInfiltrationRateB <- Param$InfiltrationRateB
", sep = "$")

textBoom(indaText, paText)
textBoom <- function(indaText, paText){
  subList <- table(indaText[,2])
  subN <- length(subList)
  subdf <- as.data.frame(table(indaText[,2]))
  for(i in 1:subN){
    subdf[i,2] <- paste0(indaText[which(indaText[,2] == subdf[i,1]),3], " = 0", collapse = ",\n")
  }
  inda1 <- "Data_ <- \nlist(InData = left_merge(hm.list(),\nlist("
  inda <- paste0(inda1, paste0(subdf[,1], " = list(", subdf[,2], ")", collapse = ",\n"),")),\nParam = list(")
  pa <- paste0(paText[,2], " = 0", collapse = ",")
  out <- "),\nOutData = left_merge(hm.list(),\nlist()))"
  cat(paste0(inda, pa, out))
}















