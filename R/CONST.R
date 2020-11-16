# ##**** Time Conversions ****##
# DAYS_PER_360DAY_YEAR <- 360 ##*< days in 360day year ##
# DAYS_PER_YEAR <- 365 ##*< days in nonleap year ##
# DAYS_PER_LYEAR <- 366 ##*< days in leap year ##
# DAYS_PER_JYEAR <- 365.25 ##* days in Julian year ##
# HOURS_PER_DAY <- 24 ##*< hours per day ##
# MONTHS_PER_YEAR <- 12 ##*< months per year ##
# MIN_PER_HOUR <- 60 ##*< minutes per hour ##
# MIN_PER_DAY <- (MIN_PER_HOUR * HOURS_PER_DAY) ##*< hours per day ##
# SEC_PER_MIN <- 60 ##*< seconds per minutes ##
# SEC_PER_HOUR <- (SEC_PER_MIN * MIN_PER_HOUR) ##*< seconds per hour ##
# SEC_PER_DAY <- (SEC_PER_HOUR * HOURS_PER_DAY) ##*< hours per day ##
#
# ##**** Unit Conversions ****##
# JOULES_PER_CAL <- 4.1868 ##*< Joules per calorie ##
# GRAMS_PER_KG <- 1000 ##*< grams per kilogram ##
# PA_PER_KPA <- 1000 ##*< Pa per kPa ##
# BAR_PER_KPA <- 100 ##*< bars per kPa ##
# RAD_PER_DEG <- 0.017453293 ##*< radians per degree ##
# M_PER_KM <- 1000 ##*< meters per kilometer ##
# MM_PER_M <- 1000 ##*< milimeters per meter ##
# CM_PER_M <- 100 ##*< centimeters per meter ##
# MM_PER_CM <- 10 ##*< milimeters per centimeter ##
# MM_PER_IN <- 25.4 ##*< milimeters per inch ##
# IN_PER_M <- (MM_PER_M / MM_PER_IN) ##*< inches per meter ##
# MOLE_PER_KMOLE <- 1000 ##*< moles per kilomole ##
# FRACT_TO_PERCENT <- 100
# PPM_to_MIXRATIO <- 1.00E-06
#
# ##**** Mathematical Constants ****##
# CONST_PI <- 3.141592654
#
# ##**** Time Constants ****##
# CONST_CDAY <- 86400 ##*< seconds in calendar day ~ sec ##
# CONST_SDAY <- 86164 ##*< seconds in siderial day ~ sec ##
# CONST_DDAYS_PER_YEAR <- 365.2425 ##*< decimal days in year ##
#
# ##**** Oribital Constants ****##
# CONST_OMEGA <- (2.0 * CONST_PI / CONST_SDAY)
# CONST_SECPERRAD <- 13750.9871 ##*< seconds per radian of hour angle ##
# CONST_RADPERDAY <- 0.017214 ##*< radians of Earth orbit per julian day ##
# CONST_RADPERDEG <- 0.01745329 ##*< radians per degree ##
# CONST_MINDECL <- -0.4092797 ##*< minimum declination (radians) ##
# CONST_DAYSOFF <- 11.25 ##*< julian day offset of winter solstice ##
#
# ##**** Physical Constants ****##
# CONST_REARTH <- 6.37E+06 ##*< radius of the earth ~ m ##
# CONST_G <- 9.80616 ##*< (m s-2) standard gravitational accel. ##
# CONST_STEBOL <- 5.67E-08 ##*< Stefan-Boltzmann constant ~ W/m^2/K^4 ##
# CONST_BOLTZ <- 1.38E-23 ##*< Boltzmann's constant ~ J/K/molecule ##
# CONST_AVOGAD <- 6.02E+26 ##*< Avogadro's number ~ molecules/kmole ##
# CONST_KARMAN <- 0.4 ##*< Von Karman constant ##
# ##* molecular weights ##
# CONST_MWDAIR <- 28.966 ##*< molecular weight of dry air ~ kg/kmole ##
# CONST_MWWV <- 18.016 ##*< molecular weight of water vapor ~ kg/kmole ##
# CONST_MWCO2 <- 44.011 ##*< molecular weight of CO2 ~ kg/kmole ##
# CONST_MWAIR <- 28.97 ##*< molecular weight of air ~ kg/kmole ##
# CONST_MWC <- 12.01 ##*< molecular weight of carbon ~ kg/kmole ##
# ##* gas constants ##
# CONST_RGAS <- (CONST_AVOGAD * CONST_BOLTZ) ##*< Universal gas constant ~ J/K/kmole ##
# CONST_RDAIR <- (CONST_RGAS / CONST_MWDAIR) ##*< Dry air gas constant ~ J/K/kg ##
# CONST_RWV <- (CONST_RGAS / CONST_MWWV) ##*< Water vapor gas constant ~ J/K/kg ##
# CONST_EPS <- (CONST_MWWV / CONST_MWAIR) ##*< Ratio of molecular weights ##
# ##* temperatures ##
# CONST_TKTRIP <- 273.16 ##*< triple point of fresh water ~ K ##
# CONST_TKFRZ <- 273.15 ##*< freezing T of fresh water ~ K ##
# CONST_TKCEL <- 273.15 ##*< The temperature T in degrees Celsius (°C) is equal to the temperature T in Kelvin (K) minus 273.15 ##
# ##* standard temperature and pressure ##
# CONST_PSTD <- 101325 ##*< (Pa) standard pressure at 0 m elevation ##
# CONST_TSTD <- (CONST_TKFRZ + 15.0) ##*< (K) standard temp at 0 m elevation ##
# ##* densities ##
# CONST_RHODAIR <- (CONST_PSTD / (CONST_RDAIR * CONST_TKFRZ)) ##*< density of dry air at STP ~ kg/m^3 ##
# CONST_RHOFW <- 1.00E+03 ##*< density of fresh water ~ kg/m^3 ##
# CONST_RHOICE <- 9.17E+02 ##*< density of ice ~ kg/m^3 ##
# ##* specific heats ##
# CONST_CPDAIR <- 1.00E+03 ##*< (J kg-1 K-1) specific heat of air ##
# CONST_CPMAIR <- 1.01E+03 ##*< (J kg-1 K-1) specific heat of moist air ##
# CONST_CPWV <- 1.81E+03 ##*< specific heat of water vap ~ J/kg/K ##
# CONST_CPFW <- 4.19E+03 ##*< specific heat of fresh h2o ~ J/kg/K ##
# CONST_CPFWICE <- 4.20E+03 ##*< specific heat of fresh h2o ~ J/kg/K ##
# CONST_CPICE <- 2.12E+03 ##*< specific heat of fresh ice ~ J/kg/K ##
# ##* volumetric heats ##
# CONST_VCPICE_WQ <- (CONST_CPICE * CONST_RHOFW) ##*< heat capacity of fresh ice per volume of water equivalent ~ J/m^3/K ##
# ##* latent heats ##
# CONST_LATICE <- 3.34E+05 ##*< latent heat of fusion ~ J/kg ##
# CONST_LATVAP <- 2.50E+06 ##*< latent heat of evaporation ~ J/kg ##
# CONST_LATSUB <- (CONST_LATICE + CONST_LATVAP) ##*< latent heat of sublimation ~ J/kg ##
#
# ## atmosphere ##
# CONST_Gas_R <- 287 ## J/kg/K
# ##*< special values ##
# CONST_SPVAL <- 1.00E+30
# DBL_EPSILON <- 0.000000000001
# MISSING <- -999
#
#
#
# #### put in rad ####
# use_data(DAYS_PER_360DAY_YEAR,
#          DAYS_PER_YEAR,
#          DAYS_PER_LYEAR,
#          DAYS_PER_JYEAR,
#          HOURS_PER_DAY,
#          MONTHS_PER_YEAR,
#          MIN_PER_HOUR,
#          MIN_PER_DAY,
#          SEC_PER_MIN,
#          SEC_PER_HOUR,
#          SEC_PER_DAY,
#          JOULES_PER_CAL,
#          GRAMS_PER_KG,
#          PA_PER_KPA,
#          BAR_PER_KPA,
#          RAD_PER_DEG,
#          M_PER_KM,
#          MM_PER_M,
#          CM_PER_M,
#          MM_PER_CM,
#          MM_PER_IN,
#          IN_PER_M,
#          MOLE_PER_KMOLE,
#          FRACT_TO_PERCENT,
#          PPM_to_MIXRATIO,
#          CONST_PI,
#          CONST_CDAY,
#          CONST_SDAY,
#          CONST_DDAYS_PER_YEAR,
#          CONST_OMEGA,
#          CONST_SECPERRAD,
#          CONST_RADPERDAY,
#          CONST_RADPERDEG,
#          CONST_MINDECL,
#          CONST_DAYSOFF,
#          CONST_REARTH,
#          CONST_G,
#          CONST_STEBOL,
#          CONST_BOLTZ,
#          CONST_AVOGAD,
#          CONST_KARMAN,
#          CONST_MWDAIR,
#          CONST_MWWV,
#          CONST_MWCO2,
#          CONST_MWAIR,
#          CONST_MWC,
#          CONST_RGAS,
#          CONST_RDAIR,
#          CONST_RWV,
#          CONST_EPS,
#          CONST_TKTRIP,
#          CONST_TKFRZ,
#          CONST_PSTD,
#          CONST_TSTD,
#          CONST_RHODAIR,
#          CONST_RHOFW,
#          CONST_RHOICE,
#          CONST_CPDAIR,
#          CONST_CPMAIR,
#          CONST_CPWV,
#          CONST_CPFW,
#          CONST_CPFWICE,
#          CONST_CPICE,
#          CONST_VCPICE_WQ,
#          CONST_LATICE,
#          CONST_LATVAP,
#          CONST_LATSUB,
#          CONST_SPVAL,
#          DBL_EPSILON,
#          MISSING,
#          CONST_Gas_R,
#          ParamAll,
#          internal = T,
#          overwrite = T)
