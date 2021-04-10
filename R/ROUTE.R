#' caculate route with UH
#' @param InData indata list, use SNOWIntercept(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @return StationFlow Q
#' @export
CONFLUENCE <- function(InData, ...) UseMethod("CONFLUENCE", InData)
#' caculate route with UH
#' @param InData indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures
#' RunoffFlowList list of Inputdata(Runoff)
#' UHAll all of the UH
#' TypeGridID all of Grid type in geological matrix
#' TransAll all of translate matrix
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @param ... other Parmeters
#' @return StationFlow Q
#' @export CONFLUENCE.G2RES
#' @export
CONFLUENCE.G2RES <- function(InData, Param, ...){
  RunoffFlowList <- InData$Confluence$WaterSource
  UHAll <- InData$Confluence$UHAll
  TypeGridID <- InData$Confluence$TypeGridID
  TransAll <- InData$Confluence$TransAll

  SfcFlowAllGrid <- RunoffFlowList[[1]]
  BasFlowAllGrid <- RunoffFlowList[[2]]
  CutGridSfc <- fctCutGridFlow(SfcFlowAllGrid, TypeGridID[[1]], TypeGridID[[2]])
  ScfRiverFlow <- fctUHConfluence(CutGridSfc$FlowOtherGrid, UHAll[[1]]) %*% TransAll[[1]] + CutGridSfc$FlowAimGrid

  CutGridBas <- fctCutGridFlow(BasFlowAllGrid, TypeGridID[[1]], TypeGridID[[2]])
  BasRiverFlow <- fctUHConfluence(CutGridBas$FlowOtherGrid, UHAll[[2]]) %*% TransAll[[1]] + CutGridBas$FlowAimGrid

  RiverFlow <- ScfRiverFlow + BasRiverFlow

  CutRiver <- fctCutGridFlow(RiverFlow, TypeGridID[[2]], TypeGridID[[3]])
  EstuaryFlow <- fctUHConfluence(CutRiver$FlowOtherGrid, UHAll[[3]]) %*% TransAll[[2]] + CutRiver$FlowAimGrid

  CutEstuary <- fctCutGridFlow(EstuaryFlow, TypeGridID[[3]], TypeGridID[[4]])
  StationFlow <- fctUHConfluence(CutEstuary$FlowOtherGrid, UHAll[[4]]) %*% TransAll[[3]] + CutEstuary$FlowAimGrid

  return(list(Route = list(StaFlow = StationFlow)))
}
#' caculate route with UH
#' @param InData indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures
#' RunoffFlowList list of Inputdata(Runoff)
#' UHAll all of the UH
#' TypeGridID all of Grid type in geological matrix
#' TransAll all of translate matrix
#' WSN	IUH the number of water source
#' GSN	IUH  the number of step from grid to station
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @param ... other Parmeters
#' @return StationFlow Q
#' @export CONFLUENCE.WSnGSmGn
#' @export
CONFLUENCE.WSnGSmGn <- function(InData, Param, ...){
  WaterSource <- InData$Confluence$WaterSource
  UHAll <- InData$IUH$UHAll
  TypeGridID <- InData$Confluence$TypeGridID
  TransAll <- InData$Confluence$TransAll
  WSN <- Param$WSN
  GSN <- Param$GSN
  RiverFlow <- 0.
  for (i in 1:WSN) {
    CutGrid <- fctCutGridFlow(WaterSource[[i]], TypeGridID[[1]], TypeGridID[[2]])
    G2RFlow <- fctUHConfluence(CutGrid$FlowOtherGrid, UHAll[[i]]) %*% TransAll[[1]] + CutGrid$FlowAimGrid
    RiverFlow <- RiverFlow + G2RFlow
  }

  for (i in 2:GSN) {
    CutRiver <- fctCutGridFlow(RiverFlow, TypeGridID[[i]], TypeGridID[[i+1]])
    RiverFlow <- fctUHConfluence(CutRiver$FlowOtherGrid, UHAll[[WSN+i-1]]) %*% TransAll[[i]] + CutRiver$FlowAimGrid
  }

  return(list(Route = list(StaFlow = RiverFlow)))
}

#' cut GridFlow from AllGridFlow to AimGridFlow and OtherGridFlow, from 1 cut to 2. Get list with $OtherGridFlow and $AimGridFlow
#' @importFrom plyr join
#' @param OriginalGridFlow flow of origal Grid
#' @param GridID origal Grid ID
#' @param AimID aim Grid ID
#' @return 2-list of:
#' \itemize{
#' \item FlowOtherGrid
#' \item FlowAimGrid
#' }
#' @export
fctCutGridFlow <- function(OriginalGridFlow, GridID, AimID){
  infoPeriodN <- dim(OriginalGridFlow)[1]
  FlowTem <- as.data.frame(cbind(GridID, t(OriginalGridFlow)))
  names(FlowTem) <- c("id",as.character(seq(1,infoPeriodN,1)))
  DiffID <- as.data.frame(setdiff(as.matrix(GridID), as.matrix(AimID)))
  names(DiffID) <- "id"
  DFAimID <- as.data.frame(AimID)
  names(DFAimID) = "id"

  JoinTem <- join(DiffID, FlowTem)
  FlowOtherGrid <- t(as.matrix(JoinTem[,-1]))

  JoinTem <- join(DFAimID, FlowTem)
  FlowAimGrid <- t(as.matrix(JoinTem[,-1]))    #the surfaceflow from VIC with subday just only other grids

  return(list(FlowOtherGrid = FlowOtherGrid, FlowAimGrid = FlowAimGrid))
}

#' caculate the Runoff mit selbst UH, but not have to the AimGrid, just know how many Confluence have to its AimGrid
#' @importFrom HMtools discrete_convolution
#' @param Flow flow of origal Grid
#' @param UH Discrete unit hydrograph
#' @return FlowResult
#' @export
fctUHConfluence <- function(Flow, UH){
  infoPeriodN <- dim(Flow)[1]
  GridN <- dim(Flow)[2]
  FlowResult <- matrix(0.0,infoPeriodN,GridN)    #the result of baseflow confluence
  for(k in 1:GridN){
    FlowResult[,k] <- discrete_convolution(UH[,k], Flow[,k])[1:infoPeriodN]
  }
  return(FlowResult)
}

#' make the TranslateMatrix, which will mit Acculate from GridList to AimGridList.
#' @param Grid2AimGrid matrix of origal Grid to aim Grid
#' @param AimID aim Grid ID
#' @return TranslateMatrix
#' @export
fctMakeGridTranslateMatrix <- function(Grid2AimGrid, AimID){
  GridN <- dim(Grid2AimGrid)[1]
  AimGridN <- length(AimID)
  TranslateMatrix <- matrix(0.0, GridN, AimGridN)
  for(k in 1:GridN) {
    TranslateMatrix[k, match(Grid2AimGrid[k,2],as.matrix(AimID))] = 1
  }
  return(TranslateMatrix)
}

#' find every Grid which Grid will go (confluence way) prepare for UH make
#' @param l num
#' @param m num
#' @param AimGridID Aim
#' @param GridID orignal
#' @param FlowDirection FlowDirection
#' @return vector of fluss
#' @export
fctGrid2AimGridFind <- function(l,m, AimGridID, GridID, FlowDirection){
  Dir_Code <- attr(FlowDirection, "Dir_Code")
  n = 0
  j = l
  k = m
  while(!is.na(GridID[j,k])){
    f = FlowDirection[j,k]
    if(f == Dir_Code[1]) {
      j = j-1
      k = k
      n = n+1
    }
    if(f == Dir_Code[2]) {
      j = j-1
      k = k+1
      n = n+1.42
    }
    if(f == Dir_Code[3]) {
      k = k+1
      j = j
      n = n+1
    }
    if(f == Dir_Code[4]) {
      j = j+1
      k = k+1
      n = n+1.42
    }
    if(f == Dir_Code[5]) {
      j = j+1
      k = k
      n = n+1
    }
    if(f == Dir_Code[6]) {
      j = j+1
      k = k-1
      n = n+1.42
    }
    if(f == Dir_Code[7]) {
      k = k-1
      j = j
      n = n+1
    }
    if(f == Dir_Code[8]) {
      j = j-1
      k = k-1
      n = n+1.42
    }
    j = j
    k = k
    if(GridID[j,k] %in% as.matrix(AimGridID))  break
  }

  tem <- array(0,dim = c(3))
  tem[1] <- j
  tem[2] <- k
  tem[3] <- n
  return(tem)
}

#' caculate Grid2AimGrid
#' @param OriginalGridID Original Grid ID
#' @param AimGridID Aim Grid ID
#' @param GridID ID in geological Grid
#' @param FlowDirection Flow Direction in geological Grid
#' @param GridDEM DEM in geological Grid
#' @return 2-Matrix with:
#' \itemize{
#' \item 1. GridID,
#' \item 2.AimGridID,
#' \item 3.length(Unit whith not m/km, but ist 1Unit = Grid length),
#' \item 4.DiffElevation(Unit is m)
#' }
#' @export
fctGrid2AimGrid <- function(OriginalGridID, AimGridID, GridID, FlowDirection, GridDEM){
  Grid2AimGrid <- matrix(0, length(OriginalGridID) - length(AimGridID), 4)
  infoGridRowN <- attr(GridID, "nrows")  #the rows number of FLOWDRIC
  infoGridColN <- attr(GridID, "ncols")   #the clows number of FLOWDRIC

  i = 1
  for(m in 1:infoGridColN){
    for(l in 1:infoGridRowN){
      judge <- GridID[l,m]
      if((judge %in% setdiff(as.matrix(OriginalGridID), as.matrix(AimGridID))) == F) next
      Grid2AimGrid[i,1] <- GridID[l,m]
      demHigh <- GridDEM[l,m]
      tem <- fctGrid2AimGridFind(l,m, AimGridID, GridID, FlowDirection)
      Grid2AimGrid[i,2] <- GridID[tem[1],tem[2]]
      demLow <- GridDEM[tem[1],tem[2]]
      Grid2AimGrid[i,3] <- tem[3]
      Grid2AimGrid[i,4] <- max(0.00000001, (demHigh - demLow))
      i = i+1
    }
  }
  return(Grid2AimGrid)
}

#' caculate all Grid to next father Grid
#' @importFrom purrr map2
#' @param TypeGridID all of Grid type in geological matrix
#' @param GridID ID in geological Grid
#' @param FlowDirection Flow Direction in geological Grid
#' @param GridDEM DEM in geological Grid
#' @return list of TypeGridN - 1
#' @export
fctG2AimGAll <- function(TypeGridID, GridID, FlowDirection, GridDEM){
  TypeGridIDAim <- TypeGridID
  TypeGridIDAim[[1]] = NULL
  TypeGridID[[length(TypeGridID)]] = NULL
  G2AimGList <- map2(TypeGridID, TypeGridIDAim, fctGrid2AimGrid, GridID, FlowDirection, GridDEM)
  return(G2AimGList)
}

#' caculate all Grid to next father Grid
#' @importFrom purrr map2
#' @param G2AimGList all of matrix of origal Grid to aim Grid
#' @param TypeGridID all of Grid type in geological matrix
#' @return list of TypeGridN - 1
#' @export
fctGTransMatAll <- function(G2AimGList, TypeGridID){
  TypeGridID[[1]] = NULL
  GTransMat <- map2(G2AimGList, TypeGridID, fctMakeGridTranslateMatrix)
}

#' @title from gridflow to station flow
#' @param InData indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures
#' @param ... paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @export
ROUTE <- function(InData, ...) UseMethod("ROUTE", InData)

#' @title use IUH calculate flow from grid to station
#' @param InData indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @param ... other Parmeters
#' @return station flow
#' @export ROUTE.IUHG2RES
#' @export
ROUTE.IUHG2RES <- function(InData, Param, ...){
  InData$Confluence$UHAll <- makeUHALL(InData, Param)
  return(CONFLUENCE.G2RES(InData, Param))
}



#' @title route from GR4J
#' @references https://webgr.inrae.fr/en/models/daily-hydrological-model-gr4j/description-of-the-gr4j-model/
#' @references Perrin, C., Michel, C. and Andréassian, V., 2003. Improvement of a parsimonious model for streamflow simulation. Journal of Hydrology, 279 : 275-289, DOI: 10.1016/S0022-1694(03)00225-7
#' @importFrom HMtools maxSVector
#' @param InData indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures
#' @param Param paramlist, in this R packege ParamAll dataset there are alredy most parameters,
#' @param ... other Parmeters
#' @return station flow
#' @export ROUTE.Gr4j
#' @export
ROUTE.Gr4j <- function(InData, Param, ...){
  X2 <- Param$Gr4j_X2
  X3 <- Param$Gr4j_X3
  X4 <- Param$Gr4j_X4
  time_step_i <- Param$time_step_i
  UH1_ <- Param$Gr4j_UH1
  UH2_ <- Param$Gr4j_UH2

  UH1_n <- ceiling(X4)
  UH2_n <- ceiling(2 * X4)
  if(time_step_i < (UH2_n + 1)) {
    UH1_ <- c(UH1_, UH1_ * 0)
    UH1_ <- UH1_[1:time_step_i,]
    UH2_ <- UH2_[1:time_step_i,]
    Q9_ori <- 0.9 * InData$Route$WaterSource[(time_step_i) : 1,]
    Q1_ori <- 0.1 * InData$Route$WaterSource[(time_step_i) : 1,]
    Q9 <- sum(UH1_ * Q9_ori)
    Q1 <- sum(UH2_ * Q1_ori)

  } else {
    Q9_ori <- 0.9 * InData$Route$WaterSource[(time_step_i) : (time_step_i - UH1_n),]
    Q1_ori <- 0.1 * InData$Route$WaterSource[(time_step_i) : (time_step_i - UH2_n),]
    Q9 <- sum(UH1_ * Q9_ori)
    Q1 <- sum(UH2_ * Q1_ori)
  }
  R_ <- InData$Route$Store
  F_ <- X2 * (R_ / X3)^3.5
  R_ <- maxSVector(0, R_ + Q9 + F_)
  Qr <- R_ * (1 - (1 + (R_ / X3)^4)^(-0.25))
  R_ <- R_ - Qr
  Qd <- maxSVector(0, Q1 + F)
  return(list(Route = list(StaFlow = Qr + Qd, Store = R_)))
}

