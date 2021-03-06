% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RUNOFF.R
\name{RUNOFF.SER}
\alias{RUNOFF.SER}
\title{SoilInfiltration in Saturation ExcessRunoff modell}
\usage{
\method{RUNOFF}{SER}(InData, Param, ...)
}
\arguments{
\item{InData}{3-list of:
\itemize{
\item PrecipitationHoch
\item SoilMoistureCapacityMax
\item SoilMoistureVolum
}}

\item{Param}{1-list of:
\itemize{
\item paSoilMoistureCapacityB
}}

\item{...}{other Parmeters}
}
\value{
2-list of:
\itemize{
\item Runoff
\item Infiltration
}
}
\description{
SoilInfiltration in Saturation ExcessRunoff modell
}
