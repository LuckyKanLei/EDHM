% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IUH.R
\name{makeUH}
\alias{makeUH}
\title{make Discrete unit hydrograph durch Instantaneous unit hydrograph}
\usage{
makeUH(UHMethond, UHParam, Param)
}
\arguments{
\item{UHMethond}{list of UH Methond}

\item{UHParam}{indata list, use snow_density(runMode = "VIEW") view the variables and theirs structures}

\item{Param}{paramlist, in this R packege ParamAll dataset there are alredy most parameters,}
}
\value{
matrix, Discrete unit hydrograph
}
\description{
make Discrete unit hydrograph durch Instantaneous unit hydrograph
}
