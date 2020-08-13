#' #' @title Get an efficiency coefficient from the parameter group
#' #' @import hydroGOF
#' #' @param ParamterCalibrate parameter group
#' #' @param fctEvaluate = NSE(Nash effect) The function to calculate the efficiency coefficient. It is recommended to be found in the "hydroGOF" R-package or written by yourself.
#' #' @param InList InList for hydro Model
#' #' @param Observe the observe data
#' #' @param warmUPN = 730 the warm up period, they will not be evaluated
#' #' @return coefficient Wert bei selecte coefficient function and Model
#' #' @export
#' CALIBRATE <- function(ParamterCalibrate, fctEvaluate = NSE, InList, Observe, warmUPN = 730){
#'   class(ParamterCalibrate) <- class(InList)
#'   PaList <- PaListMake(ParamterCalibrate)
#'   Result <- MODELL(InList, PaList)
#'   coefficientWert = fctEvaluate(Result[-(1: warmUPN),], as.matrix(Observe)[-(1: warmUPN),])
#'   message("--------------------model end.\n", "Coefficient is: ", coefficientWert)
#'   return(coefficientWert)
#' }
