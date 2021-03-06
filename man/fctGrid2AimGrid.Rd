% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROUTE.R
\name{fctGrid2AimGrid}
\alias{fctGrid2AimGrid}
\title{caculate Grid2AimGrid}
\usage{
fctGrid2AimGrid(OriginalGridID, AimGridID, GridID, FlowDirection, GridDEM)
}
\arguments{
\item{OriginalGridID}{Original Grid ID}

\item{AimGridID}{Aim Grid ID}

\item{GridID}{ID in geological Grid}

\item{FlowDirection}{Flow Direction in geological Grid}

\item{GridDEM}{DEM in geological Grid}
}
\value{
2-Matrix with:
\itemize{
\item 1. GridID,
\item 2.AimGridID,
\item 3.length(Unit whith not m/km, but ist 1Unit = Grid length),
\item 4.DiffElevation(Unit is m)
}
}
\description{
caculate Grid2AimGrid
}
