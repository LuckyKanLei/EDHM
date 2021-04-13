# ## ET ####
# Data_ReferenceET.PenMon <-
#   list(ref_title = 'Step by Step Calculation of the Penman-Monteith Evapotranspiration (FAO-56)',
#        ref_bib = 'LincolnZotarelli.2014',
#        InData = left_merge(t_vari.hm.list(),
#                            list(MetData = list(TAir = t_vari.array(0, c(3)),
#                                                TMax = t_vari.array(0, c(3)),
#                                                TMin = t_vari.array(0, c(3)),
#                                                RelativeHumidity = t_vari.array(0, c(3)),
#                                                WindSpeed = t_vari.array(0, c(3)),
#                                                WindH = t_vari.array(0, c(3)),
#                                                SunHour = t_vari.array(0, c(3))),
#                                 GeoData = list(Latitude = t_vari.array(0, c(3)),
#                                                Elevation = t_vari.array(0, c(3))),
#                                 TimeData = list(NDay = t_vari.array(0, c(3))))),
#        Param = list(PeriodN = c(1, 9999, '--', 'The number of Step'),
#                     GridN = c(1, 9999, '--', 'The nummber of effektive Grids')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(RET = t_vari.array(0, c(3))))))
# Data_ReferenceET.Hargreaves <-
#   list(ref_title = 'Reference Crop Evapotranspiration from Temperature',
#        ref_bib = 'GeorgeH.Hargreaves.1985',
#        InData = left_merge(t_vari.hm.list(),
#                            list(MetData = list(TAir = t_vari.array(0, c(3)),
#                                                TMax = t_vari.array(0, c(3)),
#                                                TMin = t_vari.array(0, c(3))),
#                                 GeoData = list(Latitude = t_vari.array(0, c(3))),
#                                 TimeData = list(NDay = t_vari.array(0, c(3))))),
#        Param = list(PeriodN = c(1, 9999, '--', 'The number of Step'),
#                     GridN = c(1, 9999, '--', 'The nummber of effektive Grids')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(RET = t_vari.array(0, c(3))))))
#
# Data_ReferenceET.Linacre <-
#   list(ref_title = 'A simple formula for estimating evaporation rates in various climates, using temperature data alone',
#        ref_bib = 'Linacre.1977',
#        InData = left_merge(t_vari.hm.list(),
#                            list(MetData = list(TAir = t_vari.array(0, c(3)),
#                                                Actual_vapor_press = t_vari.array(0, c(3))),
#                                 GeoData = list(Latitude = t_vari.array(0, c(3)),
#                                                Elevation = t_vari.array(0, c(3))),
#                                 TimeData = list(NDay = t_vari.array(0, c(3))))),
#        Param = list(PeriodN = c(1, 9999, '--', 'The number of Step'),
#                     GridN = c(1, 9999, '--', 'The nummber of effektive Grids')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(RET = t_vari.array(0, c(3))))))
#
#
# Data_ActualET.Vic <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Aerodyna = list(AerodynaResist = t_vari.array(0, c(3)),
#                                                 ArchitecturalResist = t_vari.array(0, c(3)),
#                                                 StomatalResist = t_vari.array(0, c(3))),
#                                 Canopy = list(StorageCapacity = t_vari.array(0, c(3))),
#                                 Evatrans = list(RET = t_vari.array(0, c(3))),
#                                 Ground = list(MoistureVolume = t_vari.array(0, c(3)),
#                                               MoistureCapacityMax = t_vari.array(0, c(3))),
#                                 Intercept = list(Interception = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(EvaporationCanopy = t_vari.array(0, c(3)),
#                                                  Transpiration = t_vari.array(0, c(3)),
#                                                  EvaporationLand = t_vari.array(0, c(3))))))
# Data_ActualET.Gr4j <-
#   list(ref_title = 'Improvement of a parsimonious model for streamflow simulation',
#        ref_bib = 'Perrin.2003',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Evatrans = list(RET = t_vari.array(0, c(3))),
#                                 Ground = list(MoistureVolume = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(Gr4j_X1 = c(0.1, 9.99, 'mm')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(AET = t_vari.array(0, c(3))),
#                                  Prec = list(Precipitation = t_vari.array(0, c(3))))))
#
# ## Base ####
# Data_BASEFLOW.ARNO <-
#   list(ref_title = 'LARGE AREA HYDROLOGIC MODELING AND ASSESSMENT PART I: MODEL DEVELOPMENT',
#        ref_bib = 'Arnold.1998',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureVolume = t_vari.array(0, c(3)),
#                                               MoistureCapacityMax = t_vari.array(0, c(3))))),
#        Param = list(ExponentARNOBase = c(0, 0, '--', '--'),
#                     ARNOBaseThresholdRadio = c(0, 0, '--', '--'),
#                     DrainageLossMax = c(0, 0, '--', '--'),
#                     DrainageLossMin = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(BaseFlow = t_vari.array(0, c(3))))))
# ## Interception ####
# Data_INTERCEPTION.Gash <-
#   list(ref_title = 'An analytical model of rainfall interception by forests',
#        ref_bib = 'Gash.1979',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Canopy = list(StorageCapacity = t_vari.array(0, c(3))),
#                                 Evatrans = list(EvaporationCanopy = t_vari.array(0, c(3))),
#                                 Intercept = list(Interception = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(CoefficientFreeThroughfall = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Intercept = list(Interception = t_vari.array(0, c(3))),
#                                  Prec = list(Precipitation = t_vari.array(0, c(3))))))
#
# ## runoff ####
# Data_InfiltratRat.GreenAmpt <-
#   list(ref_title = 'Drainage to a water table analysed by the Green-Ampt approach',
#        ref_bib = 'Youngs.1976',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureVolume = t_vari.array(0, c(3)),
#                                               Depth = t_vari.array(0, c(3))),
#                                 SoilData = list(Conductivity = t_vari.array(0, c(3)),
#                                                 WettingFrontSuction = t_vari.array(0, c(3)),
#                                                 Porosity = t_vari.array(0, c(3))))),
#        Param = list(GridN = c(1, 9999, '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Infilt = list(InfiltrationRat = t_vari.array(0, c(3))))))
#
# Data_Infiltration.SER <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureCapacityMax = t_vari.array(0, c(3)),
#                                               MoistureCapacity = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
# Data_Infiltration.OIER <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Infilt = list(InfiltrationRat = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(InfiltrationRateB = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
# Data_RUNOFF.SER <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureCapacityMax = t_vari.array(0, c(3)),
#                                               MoistureVolume = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3))),
#                                  Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
# Data_RUNOFF.OIER <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Infilt = list(InfiltrationRateMax = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(InfiltrationRateB = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3))),
#                                  Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
#
# Data_RUNOFF.Vic <-
#   list(ref_title = 'A new surface runoff parameterization with subgrid-scale soil heterogeneity for land surface models',
#        ref_bib = 'Liang.2001',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureCapacityMax = t_vari.array(0, c(3)),
#                                               MoistureVolume = t_vari.array(0, c(3))),
#                                 Infilt = list(InfiltrationRat = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = c(0, 0, '--', '--'),
#                     InfiltrationRateB = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3))),
#                                  Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
# Data_RUNOFF.VM <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureCapacity = t_vari.array(0, c(3)),
#                                               MoistureCapacityMax = t_vari.array(0, c(3))),
#                                 Infilt = list(InfiltrationRateMax = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = c(0, 0, '--', '--'),
#                     InfiltrationRateB = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3))),
#                                  Infilt = list(Infiltration = t_vari.array(0, c(3))))))
# Data_RUNOFF.Gr4j <-
#   list(ref_title = 'Improvement of a parsimonious model for streamflow simulation',
#        ref_bib = 'Perrin.2003',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureVolume = t_vari.array(0, c(3))),
#                                 Evatrans = list(AET = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(Gr4j_X1 = c(0.1, 9.99, 'mm')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3)),
#                                                MoistureVolume = t_vari.array(0, c(3))))))
# ## Snow ####
# Data_SNOW.Ddf <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureVolume = t_vari.array(0, c(3))),
#                                 Snow = list(Volume = t_vari.array(0, c(3))),
#                                 Prec = list(SnowFall = t_vari.array(0, c(3)),
#                                             RainFall = t_vari.array(0, c(3))))),
#        Param = list(Factor_Day_degree = c(0, 0, '--', '--')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Snow = list(Volume = t_vari.array(0, c(3))),
#                                  Prec = list(Precipitation = t_vari.array(0, c(3))))))
#
# Data_SNOW.17 <-
#   list(ref_title = 'National Weather Service river forecast system: snow accumulation and ablation model',
#        ref_bib = 'Anderson.1973',
#        InData = left_merge(t_vari.hm.list(),
#                            list(MetData = list(TAir = t_vari.array(0, c(3))),
#                                 Snow = list(Ice_Volume = t_vari.array(0, c(3)),
#                                             Liquid_Volume = t_vari.array(0, c(3)),
#                                             SN17_ATI = t_vari.array(0, c(3)),
#                                             SN17_HD = t_vari.array(0, c(3))),
#                                 Prec = list(SnowFall = t_vari.array(0, c(3)),
#                                             RainFall = t_vari.array(0, c(3))),
#                                 GeoData = list(Elevation = t_vari.array(0, c(3))),
#                                 TimeData = list(NDay = t_vari.array(0, c(3))))),
#        Param = list(SN17_SCF = c(0.7, 1.4, '--', 'Snowfall correction factor'),
#                     SN17_MFMAX = c(0.5, 2.0, 'mm/6hCel', 'Maximum melt factor considered to occur on June 21'),
#                     SN17_MFMIN = c(0.05, 0.49, 'mm/6hCel', 'Minimum melt factor considered to occur on December 21'),
#                     SN17_UADJ = c(0.03, 0.19, 'mm/6hCel', 'The average wind function during rain-on-snow periods'),
#                     SN17_NMF = c(0.05, 0.50, 'mm/6hCel', 'Maximum negative melt factor'),
#
#                     SN17_TIPM = c(0.1, 1.0, '--', 'Antecedent snow temperature index'),
#                     SN17_PXTEMP = c(-2.0, 2.0, 'Cel', 'Temperature that separates rain from snow'),
#                     SN17_MBASE = c(0, 1.0, 'Cel', 'Base temperature for non-rain melt factor'),
#                     SN17_PLWHC = c(0.02, 0.3, '--', 'Percent of liquid–water capacity'),
#                     SN17_DAYGM = c(0, 0.3, 'mm/d', 'Daily melt at snow–soil interface'),
#                     TimeStepSec = c(1, 9999, 's', 'Second pro Step')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Snow = list(Ice_Volume = t_vari.array(0, c(3)),
#                                              Liquid_Volume = t_vari.array(0, c(3)),
#                                              SN17_ATI = t_vari.array(0, c(3)),
#                                              SN17_HD = t_vari.array(0, c(3))),
#                                  Prec = list(Precipitation = t_vari.array(0, c(3))))))
# ## Groundwater ####
# Data_GROUNDWATER.Vic <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(ZoneMoistureVolume = t_vari.array(0, c(3)),
#                                               ZoneDepth = t_vari.array(0, c(3)),
#                                               BaseFlow = t_vari.array(0, c(3))),
#                                 Infilt = list(Infiltration = t_vari.array(0, c(3))),
#                                 Intercept = list(Interception = t_vari.array(0, c(3))),
#                                 SoilData = list(Porosity = t_vari.array(0, c(3)),
#                                                 SaturatedConductivity = t_vari.array(0, c(3))))),
#        Param = list(GridN = c(1, 9999, '--', 'The nummber of effektive Grids')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Overflow = t_vari.array(0, c(3)),
#                                                ZoneMoistureVolume = t_vari.array(0, c(3))))))
#
# ## Route ####
# Data_ROUTE.Gr4j <-
#   list(ref_title = 'Improvement of a parsimonious model for streamflow simulation',
#        ref_bib = 'Perrin.2003',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Route = list(WaterSource = t_vari.array(0, c(3)),
#                                              Store = t_vari.array(0, c(3)),
#                                              Gr4j_UH1 = t_vari.array(0, c(3)),
#                                              Gr4j_UH2 = t_vari.array(0, c(3))))),
#        Param = list(Gr4j_X2 = c(0.1, 9.99, 'mm/Step', 'The catchment water exchange coe icient'),
#                     Gr4j_X3 = c(0.1, 9.99, 'mm', 'The one-day maximal capacity of the routing reservoir'),
#                     Gr4j_X4 = c(1, 9.99, 'mm/Step', 'The HU1 unit hydrograph time base'),
#                     time_step_i = c(1, 9999, '--', 'The time Step index')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Route = list(StaFlow = t_vari.array(0, c(3)),
#                                               Store = t_vari.array(0, c(3))))))
# Data_ROUTE.G2RES <-
#   list(ref_title = 'none',
#        ref_bib = 'none',
#        InData = left_merge(t_vari.hm.list(),
#                            list(Route = list(WaterSource = t_vari.array(0, c(3)),
#                                              UHAll = t_vari.array(0, c(3)),
#                                              TypeGridID = t_vari.array(0, c(3)),
#                                              TransAll = t_vari.array(0, c(3))))),
#        Param = list(PeriodN = c(1, 9999, '--', 'The number of Step'),
#                     GridN = c(1, 9999, '--', 'The nummber of effektive Grids')),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Route = list(StaFlow = t_vari.array(0, c(3))))))
#
#
# ## write data.R ####
# write(paste0('#\' ', ls(), '\n#\' @format list of function data(InData, Param, OutData)\n', '\"', ls(), '\"\n'),
#       "R\\data.R")
# ## Pre_dataset ####
# # library(HMtools)
# eval(parse(text = paste0("Pre_dataset <- merge_Data_Modul(",paste0(ls(), collapse = ", "), ")")))
# Pre_dataset <- left_merge(Pre_dataset$InData, Pre_dataset$OutData)
# ## write in Package ####
# ## go to Unit_Data.R and Des_Data.R ####
# source("R\\Unit_Data.R")
# source("R\\Des_Data.R")
#
# eval(parse(text = paste0("use_data(",paste0(ls(), collapse = ", "), ", internal = F, overwrite = T)")))
#
# write(paste0('\n#\' ', c('Pre_dataset', 'Des_dataset', 'Unit_dataset'), '\n#\' @format list of all variable\n\"', c('Pre_dataset', 'Des_dataset', 'Unit_dataset'),'\"'), "R\\data.R", append = T)
