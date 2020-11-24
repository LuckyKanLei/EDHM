## ET
##
Data_ReferenceET.PenMon <-
  list(InData = left_merge(hm.list(),
                           list(MetData = list(TAir = 0,
                                               TMax = 0,
                                               TMin = 0,
                                               RelativeHumidity = 0,
                                               WindSpeed = 0,
                                               WindH = 0,
                                               SunHour = 0),
                                GeoData = list(Latitude = 0,
                                               Elevation = 0),
                                TimeData = list(NDay = 0))),
       Param = list(PeriodN = 0,
                    GridN = 0),
       OutData = left_merge(hm.list(),
                            list(Evatrans = list(RET = 0))))
Data_ReferenceET.Hargreaves <-
  list(InData = left_merge(hm.list(),
                           list(MetData = list(TAir = 0,
                                               TMax = 0,
                                               TMin = 0),
                                GeoData = list(Latitude = 0),
                                TimeData = list(NDay = 0))),
       Param = list(PeriodN = 0,
                    GridN = 0),
       OutData = left_merge(hm.list(),
                            list(Evatrans = list(RET = 0))))
Data_ActualET.Vic <-
  list(InData = left_merge(hm.list(),
                           list(Aerodyna = list(AerodynaResist = 0,
                                                ArchitecturalResist = 0,
                                                StomatalResist = 0),
                                Canopy = list(StorageCapacity = 0),
                                Evatrans = list(RET = 0),
                                Ground = list(MoistureVolume = 0,
                                              MoistureCapacityMax = 0),
                                Intercept = list(Interception = 0),
                                Prec = list(Precipitation = 0))),
       Param = list(SoilMoistureCapacityB = 0),
       OutData = left_merge(hm.list(),
                            list(Evatrans = list(EvaporationCanopy = 0,
                                                 Transpiration = 0,
                                                 EvaporationLand = 0))))

## Base
Data_BASEFLOW.ARNO <-
  list(InData = left_merge(hm.list(),
                           list(Ground = list(MoistureVolume = 0,
                                              MoistureCapacityMax = 0))),
       Param = list(ExponentARNOBase = 0,ARNOBaseThresholdRadio = 0,DrainageLossMax = 0,DrainageLossMin = 0),
       OutData = left_merge(hm.list(),
                            list(Ground = list(BaseFlow = 0))))
## infil
Data_INTERCEPTION.Gash <-
  list(InData = left_merge(hm.list(),
                           list(Canopy = list(StorageCapacity = 0),
                                ET = list(EvaporationCanopy = 0),
                                Intercept = list(Interception = 0),
                                Prec = list(Precipitation = 0))),
       Param = list(CoefficientFreeThroughfall = 0),
       OutData = left_merge(hm.list(),
                            list(Intercept = list(Interception = 0),
                                 Prec = list(Precipitation = 0))))

## runoff
Data_InfiltratRat.GreenAmpt <-
  list(InData = left_merge(hm.list(),
                           list(Ground = list(Conductivity = 0,
                                              WettingFrontSuction = 0,
                                              Porosity = 0,
                                              MoistureVolume = 0,
                                              Depth = 0))),
       Param = list(),
       OutData = left_merge(hm.list(),
                            list()))

Data_Infiltration.SER <-
  list(InData = left_merge(hm.list(),
                           list(Ground = list(MoistureCapacityMax = 0,
                                              MoistureCapacity = 0),
                                Prec = list(Precipitation = 0))),
       Param = list(SoilMoistureCapacityB = 0),
       OutData = left_merge(hm.list(),
                            list(Infilt = list(Infiltration = TEM))))

Data_Infiltration.OIER <-
  list(InData = left_merge(hm.list(),
                           list(Infilt = list(InfiltrationRat = 0),
                                Prec = list(Precipitation = 0))),
       Param = list(InfiltrationRateB = 0),
       OutData = left_merge(hm.list(),
                            list(Infilt = list(Infiltration = TEM))))

Data_RUNOFF.SER <-
  list(InData = left_merge(hm.list(),
                           list(Ground = list(MoistureCapacityMax = 0,
                                              MoistureVolume = 0),
                                Prec = list(Precipitation = 0))),
       Param = list(SoilMoistureCapacityB = 0),
       OutData = left_merge(hm.list(),
                            list(Ground = list(Runoff = 0),
                                 Infilt = list(Infiltration = 0))))

Data_RUNOFF.OIER <-
  list(InData = left_merge(hm.list(),
                           list(Infilt = list(InfiltrationRateMax = 0),
                                Prec = list(Precipitation = 0))),
       Param = list(InfiltrationRateB = 0),
       OutData = left_merge(hm.list(),
                            list(Ground = list(Runoff = 0),
                                 Infilt = list(Infiltration = 0))))


Data_RUNOFF.Vic <-
  list(InData = left_merge(hm.list(),
                           list(Ground = list(MoistureCapacityMax = 0,
                                              MoistureVolume = 0),
                                Infilt = list(InfiltrationRat = 0),
                                Prec = list(Precipitation = 0))),
       Param = list(SoilMoistureCapacityB = 0,InfiltrationRateB = 0),
       OutData = left_merge(hm.list(),
                            list(Ground = list(Runoff = 0),
                                 Infilt = list(Infiltration = 0))))

Data_RUNOFF.VM <-
  list(InData = left_merge(hm.list(),
                           list(Ground = list(MoistureCapacity = 0,
                                              MoistureCapacityMax = 0),
                                Infilt = list(InfiltrationRateMax = 0),
                                Prec = list(Precipitation = 0))),
       Param = list(SoilMoistureCapacityB = 0,InfiltrationRateB = 0),
       OutData = left_merge(hm.list(),
                            list(Ground = list(Runoff = 0),
                                 Infilt = list(Infiltration = 0))))



