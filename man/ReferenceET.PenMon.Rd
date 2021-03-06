% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ET.R
\name{ReferenceET.PenMon}
\alias{ReferenceET.PenMon}
\title{ReferenceET with PenmanMonteith methond}
\usage{
\method{ReferenceET}{PenMon}(InData, Param, ...)
}
\arguments{
\item{InData}{list of:
\itemize{
\item JDay, date
\item Elevation, m
\item Latitude
\item Tmax, oC, Celius
\item Tmin, oC, Celius
\item Tmean, oC, Celius
\item Wind, speed, m/s
\item WindH, hoch, speed messeur
\item SunHour,
\item RelativeHumidity,
}}

\item{Param}{paramlist, in this R packege ParamAll dataset there are alredy most parameters,}

\item{...}{other Parmeters}
}
\value{
Reference ET with PenmanMonteith methond
}
\description{
ReferenceET with PenmanMonteith methond
}
\references{
Pengman H L. Estimating evaporation[J]. American Geophysical Union, 1956(1):43-50.
}
