# ## ET ####
# Data_ReferenceET.PenMon <-
#   list(InData = left_merge(t_vari.hm.list(),
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
#        Param = list(PeriodN = t_vari.array(0, c(3)),
#                     GridN = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(RET = t_vari.array(0, c(3))))))
# Data_ReferenceET.Hargreaves <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(MetData = list(TAir = t_vari.array(0, c(3)),
#                                                TMax = t_vari.array(0, c(3)),
#                                                TMin = t_vari.array(0, c(3))),
#                                 GeoData = list(Latitude = t_vari.array(0, c(3))),
#                                 TimeData = list(NDay = t_vari.array(0, c(3))))),
#        Param = list(PeriodN = t_vari.array(0, c(3)),
#                     GridN = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(RET = t_vari.array(0, c(3))))))
#
# Data_ReferenceET.Linacre <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(MetData = list(TAir = t_vari.array(0, c(3)),
#                                                Actual_vapor_press = t_vari.array(0, c(3))),
#                                 GeoData = list(Latitude = t_vari.array(0, c(3)),
#                                                Elevation = t_vari.array(0, c(3))),
#                                 TimeData = list(NDay = t_vari.array(0, c(3))))),
#        Param = list(PeriodN = t_vari.array(0, c(3)),
#                     GridN = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(RET = t_vari.array(0, c(3))))))
#
#
# Data_ActualET.Vic <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Aerodyna = list(AerodynaResist = t_vari.array(0, c(3)),
#                                                 ArchitecturalResist = t_vari.array(0, c(3)),
#                                                 StomatalResist = t_vari.array(0, c(3))),
#                                 Canopy = list(StorageCapacity = t_vari.array(0, c(3))),
#                                 Evatrans = list(RET = t_vari.array(0, c(3))),
#                                 Ground = list(MoistureVolume = t_vari.array(0, c(3)),
#                                               MoistureCapacityMax = t_vari.array(0, c(3))),
#                                 Intercept = list(Interception = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(EvaporationCanopy = t_vari.array(0, c(3)),
#                                                  Transpiration = t_vari.array(0, c(3)),
#                                                  EvaporationLand = t_vari.array(0, c(3))))))
# Data_ActualET.Gr4j <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Evatrans = list(RET = t_vari.array(0, c(3))),
#                                 Ground = list(MoistureVolume = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(Gr4j_X1 = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Evatrans = list(AET = t_vari.array(0, c(3))),
#                                  Prec = list(Precipitation = t_vari.array(0, c(3))))))
#
# ## Base ####
# Data_BASEFLOW.ARNO <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureVolume = t_vari.array(0, c(3)),
#                                               MoistureCapacityMax = t_vari.array(0, c(3))))),
#        Param = list(ExponentARNOBase = t_vari.array(0, c(3)),ARNOBaseThresholdRadio = t_vari.array(0, c(3)),DrainageLossMax = t_vari.array(0, c(3)),DrainageLossMin = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(BaseFlow = t_vari.array(0, c(3))))))
# ## Interception ####
# Data_INTERCEPTION.Gash <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Canopy = list(StorageCapacity = t_vari.array(0, c(3))),
#                                 Evatrans = list(EvaporationCanopy = t_vari.array(0, c(3))),
#                                 Intercept = list(Interception = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(CoefficientFreeThroughfall = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Intercept = list(Interception = t_vari.array(0, c(3))),
#                                  Prec = list(Precipitation = t_vari.array(0, c(3))))))
#
# ## runoff ####
# Data_InfiltratRat.GreenAmpt <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(Conductivity = t_vari.array(0, c(3)),
#                                               WettingFrontSuction = t_vari.array(0, c(3)),
#                                               Porosity = t_vari.array(0, c(3)),
#                                               MoistureVolume = t_vari.array(0, c(3)),
#                                               Depth = t_vari.array(0, c(3))))),
#        Param = list(),
#        OutData = left_merge(t_vari.hm.list(),
#                             list()))
#
# Data_Infiltration.SER <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureCapacityMax = t_vari.array(0, c(3)),
#                                               MoistureCapacity = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
# Data_Infiltration.OIER <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Infilt = list(InfiltrationRat = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(InfiltrationRateB = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
# Data_RUNOFF.SER <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureCapacityMax = t_vari.array(0, c(3)),
#                                               MoistureVolume = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3))),
#                                  Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
# Data_RUNOFF.OIER <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Infilt = list(InfiltrationRateMax = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(InfiltrationRateB = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3))),
#                                  Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
#
# Data_RUNOFF.Vic <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureCapacityMax = t_vari.array(0, c(3)),
#                                               MoistureVolume = t_vari.array(0, c(3))),
#                                 Infilt = list(InfiltrationRat = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = t_vari.array(0, c(3)),InfiltrationRateB = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3))),
#                                  Infilt = list(Infiltration = t_vari.array(0, c(3))))))
#
# Data_RUNOFF.VM <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureCapacity = t_vari.array(0, c(3)),
#                                               MoistureCapacityMax = t_vari.array(0, c(3))),
#                                 Infilt = list(InfiltrationRateMax = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(SoilMoistureCapacityB = t_vari.array(0, c(3)),InfiltrationRateB = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3))),
#                                  Infilt = list(Infiltration = t_vari.array(0, c(3))))))
# Data_RUNOFF.Gr4j <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureVolume = t_vari.array(0, c(3))),
#                                 Evatrans = list(AET = t_vari.array(0, c(3))),
#                                 Prec = list(Precipitation = t_vari.array(0, c(3))))),
#        Param = list(Gr4j_X1 = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Runoff = t_vari.array(0, c(3)),
#                                                MoistureVolume = t_vari.array(0, c(3))))))
# ## Snow ####
# Data_SNOW.Ddf <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(MoistureVolume = t_vari.array(0, c(3))),
#                                 Snow = list(Volume = t_vari.array(0, c(3))),
#                                 Prec = list(SnowFall = t_vari.array(0, c(3)),
#                                             RainFall = t_vari.array(0, c(3))))),
#        Param = list(Factor_Day_degree = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Snow = list(Volume = t_vari.array(0, c(3))),
#                                  Prec = list(Precipitation = t_vari.array(0, c(3))))))
#
# Data_SNOW.17 <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(MetData = list(TAir = t_vari.array(0, c(3))),
#                                 Snow = list(Ice_Volume = t_vari.array(0, c(3)),
#                                             Liquid_Volume = t_vari.array(0, c(3)),
#                                             SN17_ATI = t_vari.array(0, c(3)),
#                                             SN17_HD = t_vari.array(0, c(3))),
#                                 Prec = list(SnowFall = t_vari.array(0, c(3)),
#                                             RainFall = t_vari.array(0, c(3))),
#                                 GeoData = list(Elevation = t_vari.array(0, c(3))),
#                                 TimeData = list(NDay = t_vari.array(0, c(3))))),
#        Param = list(SN17_SCF = t_vari.array(0, c(3)),
#                     SN17_MFMAX = t_vari.array(0, c(3)),
#                     SN17_MFMIN = t_vari.array(0, c(3)),
#                     SN17_UADJ = t_vari.array(0, c(3)),
#                     SN17_NMF = t_vari.array(0, c(3)),
#                     SN17_TIPM = t_vari.array(0, c(3)),
#                     SN17_PXTEMP = t_vari.array(0, c(3)),
#                     SN17_MBASE = t_vari.array(0, c(3)),
#                     SN17_PLWHC = t_vari.array(0, c(3)),
#                     SN17_DAYGM = t_vari.array(0, c(3)),
#                     TimeStepSec = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Snow = list(Ice_Volume = t_vari.array(0, c(3)),
#                                              Liquid_Volume = t_vari.array(0, c(3)),
#                                              SN17_ATI = t_vari.array(0, c(3)),
#                                              SN17_HD = t_vari.array(0, c(3))),
#                                  Prec = list(Precipitation = t_vari.array(0, c(3))))))
# ## Groundwater ####
# Data_GROUNDWATER.Vic <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(Ground = list(ZoneMoistureVolume = t_vari.array(0, c(3)),
#                                               ZoneDepth = t_vari.array(0, c(3)),
#                                               BaseFlow = t_vari.array(0, c(3))),
#                                 Infilt = list(Infiltration = t_vari.array(0, c(3))),
#                                 Intercept = list(Interception = t_vari.array(0, c(3))),
#                                 SoilData = list(Porosity = t_vari.array(0, c(3)),
#                                                 SaturatedConductivity = t_vari.array(0, c(3))))),
#        Param = list(),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Ground = list(Overflow = t_vari.array(0, c(3)),
#                                                ZoneMoistureVolume = t_vari.array(0, c(3))))))
#
# ## Route ####
# Data_ROUTE.Gr4j <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(WaterSource = t_vari.array(0, c(3)),
#                                 Store = t_vari.array(0, c(3)),
#                                 Gr4j_UH1 = t_vari.array(0, c(3)),
#                                 Gr4j_UH2 = t_vari.array(0, c(3)))),
#        Param = list(Gr4j_X2 = t_vari.array(0, c(3)),
#                     Gr4j_X3 = t_vari.array(0, c(3)),
#                     Gr4j_X4 = t_vari.array(0, c(3)),
#                     time_step_i = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Route = list(StaFlow = t_vari.array(0, c(3)),
#                                               Store = t_vari.array(0, c(3))))))
# Data_ROUTE.G2RES <-
#   list(InData = left_merge(t_vari.hm.list(),
#                            list(WaterSource = t_vari.array(0, c(3)),
#                                 UHAll = t_vari.array(0, c(3)),
#                                 TypeGridID = t_vari.array(0, c(3)),
#                                 TransAll = t_vari.array(0, c(3)))),
#        Param = list(Gr4j_X2 = t_vari.array(0, c(3)),
#                     Gr4j_X3 = t_vari.array(0, c(3)),
#                     Gr4j_X4 = t_vari.array(0, c(3)),
#                     time_step_i = t_vari.array(0, c(3))),
#        OutData = left_merge(t_vari.hm.list(),
#                             list(Route = list(StaFlow = t_vari.array(0, c(3))))))
#
#
# ## Pre_dataset ####
# library(HMtools)
# eval(parse(text = paste0("Pre_dataset <- merge_Data_Modul(",paste0(ls(), collapse = ", "), ")")))
#
# ## write in Package ####
# eval(parse(text = paste0("use_data(",paste0(ls(), collapse = ", "), ", internal = F, overwrite = T)")))
# ## for data.R ####
# write(paste0('#\' ', ls(), '\n#\' @format list of function data(InData, Param, OutData)\n', '\"', ls(), '\"\n'),
#       "R\\data.R")
