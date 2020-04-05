#' MetData list, num, list of metrol data. For VIC Model should contain at least the following 8 data:
#' @format a list of 8
#' \describe{
#'  \item{NOTE}{The field names in these lists must be consistent and will be the only index.}
#'  \item{Tmean}{mean tempratur}
#'  \item{Tmax}{max tempratur}
#'  \item{Tmin}{min tempratur}
#'  \item{WindSpeed}{Wind Speed}
#'  \item{WindH}{Wind messure Hight}
#'  \item{SunHour}{Sun Hour in procent}
#'  \item{RelativeHumidity}{Relative Humidity}
#'  \item{PrecipitationHoch}{Precipitation Hoch}
#' }
#'     Each data is a 2-array(periodN, gridN).
"MetroList"

#' GeoData list, geological data,  For VIC Model should contain at least the following 4 data:
#' @format a list of 4
#' \describe{
#'  \item{NOTE}{The field names in these lists must be consistent and will be the only index.}
#'  \item{Evalution}{1-array(gridN). Evalution of all grids.}
#'  \item{Location}{data.frame:	gridN obs. of  3 variables. ID, Latitude, Longitude.}
#'  \item{SoilParam}{data.frame':	gridN obs. of  some variables.}
#'  \item{LanduseParam}{data.frame':	gridN obs. of  some variables.}
#' }
#'     Location field names, for VIC Model should contain at least the following 2 data:
#' \describe{
#'  \item{NOTE}{The field names in these lists must be consistent and will be the only index.}
#'  \item{Latitude}{Latitude}
#'  \item{Longitude}{Longitude}
#' }
#'     SoilParam field names, for VIC Model should contain at least the following 12 data
#' \describe{
#'  \item{NOTE}{The field names in these lists must be consistent and will be the only index.}
#'  \item{T_Porosity_}{top soil Porosity}
#'  \item{T_FieldCapacity_}{top soil Porosity FieldCapacity}
#'  \item{T_WiltingPoint_}{top soil Porosity WiltingPoint}
#'  \item{T_WettingFrontSoilSuctionHead_mm}{top soil Porosity WettingFrontSoilSuctionHead in mm}
#'  \item{T_SaturatedSoilSuctionHead_mm}{top soil Porosity SaturatedSoilSuctionHead in mm}
#'  \item{T_SaturatedHydraulicConductivity_mm_day}{top soil Porosity SaturatedHydraulicConductivity in mm per day}
#'  \item{S_Porosity_}{sub soil Porosity}
#'  \item{S_FieldCapacity_}{sub soil Porosity FieldCapacity}
#'  \item{S_WiltingPoint_}{sub soil Porosity WiltingPoint}
#'  \item{S_WettingFrontSoilSuctionHead_mm}{sub soil Porosity WettingFrontSoilSuctionHead in mm}
#'  \item{S_SaturatedSoilSuctionHead_mm}{sub soil Porosity SaturatedSoilSuctionHead in mm}
#'  \item{S_SaturatedHydraulicConductivity_mm_day}{sub soil Porosity SaturatedHydraulicConductivity in mm per day}
#' }
#'  LanduseParam field names, for VIC Model should contain at least the following 28 data
#' \describe{
#'  \item{NOTE}{The field names in these lists must be consistent and will be the only index.}
#'  \item{SL_mm}{SL in mm}
#'  \item{root_depth_mm}{root depth in mm}
#'  \item{rarc}{jetz nict klar}
#'  \item{rmin}{jetz nict klar}
#'  \item{ROU.JAN - ROU.DEC (*12)}{ROU from JAN to DEC}
#'  \item{DIS.JAN - DIS.DEC (*12)}{DIS from JAN to DEC}
#' }
"GeoList"

#' GridData 4-list of grid-data used to calculate confluence(route)
#' @format a list of 4
#' \describe{
#'  \item{NOTE}{The field names in these lists must be consistent and will be the only index.}
#'  \item{TypeGridID}{4-list. Type of each grid point.}
#'  \item{GridID}{matrix:	ID in grid-data, be sure to mark}{attr(GridID, "Null"), what the Null-Wert in grid-data.}
#'  \item{FlowDirection}{matrix':	FlowDirection in grid-data, be sure to mark}{attr(GridID, "Null"), what the Null-Wert in grid-data.}
#'  \item{GridDEM}{matrix':	DEM in grid-data, be sure to mark}{attr(GridID, "Null"), what the Null-Wert in grid-data.}
#' }
#' TypeGridID: 4-list
#' \describe{
#'  \item{NOTE}{The field names in these lists must be consistent and will be the only index.}
#'  \item{GridGridID}{all grid ID in 1-array(gridN)}
#'  \item{RiverGridID}{river grid ID in 1-array, Estuary and HydroStation belong to river}
#'  \item{EstuaryID}{Estuary ID in 1-array, HydroStation belong to Estuary}
#'  \item{HydroStationID}{HydroStation ID in 1-array}
#' }
"GridList"




