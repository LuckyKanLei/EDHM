#' @title Get an efficiency coefficient from the parameter group
#' @import hydroGOF
#' @param Paramter_Calibrate parameter for the parameter that need to calibrate
#' @param fct_Run_Model
#' @param TimeVariData Time Vari Data
#' @param TimeInvariData Time invariabel Data
#' @param Paramter_fest parameter for the parameter that don't need to calibrate
#' @param Observe the observe data
#' @param warmUPN = 730 the warm up period, they will not be evaluated
#' @param fct_Evaluate = NSE(Nash effect) The function to calculate the efficiency coefficient. It is recommended to be found in the "hydroGOF" R-package or written by yourself.
#' @return coefficient Wert bei selecte coefficient function and Model
#' @export
EVALUATE <- function(Paramter_Calibrate, fct_Run_Model,
                     TimeVariData, TimeInvariData, Paramter_fest,
                     Observe,
                     warmUPN = 300, fct_Evaluate = NSE){

  Result_ <- fct_Run_Model(TimeVariData, TimeInvariData, Paramter_fest, Paramter_Calibrate)
  coefficientWert = fct_Evaluate(Result_[-(1: warmUPN),], as.matrix(Observe)[-(1: warmUPN),])
  message("--------------------model end.\n", "Coefficient is: ", coefficientWert)
  return(coefficientWert)
}
