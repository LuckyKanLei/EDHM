#' @title Model GR4J
#' @references https://webgr.inrae.fr/en/models/daily-hydrological-model-gr4j/description-of-the-gr4j-model/
#' @references Perrin, C., Michel, C. and Andréassian, V., 2003. Improvement of a parsimonious model for streamflow simulation. Journal of Hydrology, 279 : 275-289, DOI: 10.1016/S0022-1694(03)00225-7
#' @importFrom HMtools maxSVector
#' @param TimeVariData Time Vari Data
#' @param TimeInvariData Time invariabel Data
#' @param Param Param
#' @param ... other arguments
#' @return Q
#' @export MODEL.GR4J
#' @export
MODEL.GR4J <- function(TimeVariData, TimeInvariData = NULL, Param, ...){
  PeriodN <- Param$PeriodN
  GridN <- Param$GridN
  X1 <- Param$Gr4j_X1
  X2 <- Param$Gr4j_X2
  X3 <- Param$Gr4j_X3
  X4 <- Param$Gr4j_X4
  UH1_ <- Param$Gr4j_UH1
  UH2_ <- Param$Gr4j_UH2
  InData <- TimeVariData
  E_all <- InData$Evatrans$RET
  P_all <- InData$Prec$Precipitation
  S <- rep(0, GridN)
  Q <- rep(0, PeriodN)
  Pr_all <- matrix(0, PeriodN, GridN)

  for(time_step_i in 1: PeriodN){
    P <- P_all[time_step_i, ]
    E <- E_all[time_step_i, ]
    judge_PbE <- P > E
    Pn <- judge_PbE * (P - E)
    En <- (!judge_PbE) * (E - P)
    Es <- S * (2 - S / X1) * tanh(En / X1) / (1 + (1 - S / X1) * tanh(En / X1))

    Ps <- X1 * (1- (S / X1) * (S / X1)) * tanh(Pn / X1) / (1 + S / X1 * tanh(Pn / X1))
    S <- S - Es + Ps
    Perc <- S * (1 - (1+ (4/9 * S / X1)^4)^(-0.25))
    S <- S - Perc
    Pr_all[time_step_i, ] <- Perc + (Pn - Ps)

    UH1_n <- ceiling(X4)
    UH2_n <- ceiling(2 * X4)
    if(time_step_i < (UH2_n + 1)) {
      UH1_ <- c(UH1_, UH1_ * 0)
      UH1_ <- UH1_[1:time_step_i,]
      UH2_ <- UH2_[1:time_step_i,]
      Q9_ori <- 0.9 * Pr_all[(time_step_i) : 1,]
      Q1_ori <- 0.1 * Pr_all[(time_step_i) : 1,]
      Q9 <- sum(UH1_ * Q9_ori)
      Q1 <- sum(UH2_ * Q1_ori)

    } else {
      Q9_ori <- 0.9 * Pr_all[(time_step_i) : (time_step_i - UH1_n),]
      Q1_ori <- 0.1 * Pr_all[(time_step_i) : (time_step_i - UH2_n),]
      browser()
      Q9 <- sum(UH1_ * Q9_ori)
      Q1 <- sum(UH2_ * Q1_ori)
    }
    R_ <- InData$Route$Store
    F_ <- X2 * (R_ / X3)^3.5
    R_ <- maxSVector(0, R_ + Q9 + F_)
    Qr <- R_ * (1 - (1 + (R_ / X3)^4)^(-0.25))
    R_ <- R_ - Qr
    Qd <- maxSVector(0, Q1 + F)
    Q[time_step_i] <- Qr + Qd
  }
  return(Q)

}
