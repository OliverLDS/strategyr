% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_technical_indicators.R
\name{.build_zones_from_pivots}
\alias{.build_zones_from_pivots}
\title{Build Support/Resistance Zones from Pivot Points}
\usage{
.build_zones_from_pivots(df, k = 6, tol_mult = 0.15, atr_col = "atr_14")
}
\arguments{
\item{df}{Data frame containing columns: high, low, ATR column, is_piv_hi, is_piv_lo.}

\item{k}{Number of most recent pivots to consider.}

\item{tol_mult}{Tolerance multiplier applied to ATR to merge close pivots.}

\item{atr_col}{Name of ATR column in df. Default is "atr_14".}
}
\value{
A tibble with two support and two resistance zones and scores:
\describe{
  \item{support_1, support_2}{Primary and secondary support levels.}
  \item{resistance_1, resistance_2}{Primary and secondary resistance levels.}
  \item{support_score_1, support_score_2}{Count of pivot hits near the support.}
  \item{resistance_score_1, resistance_score_2}{Count of pivot hits near resistance.}
}
}
\description{
Collapses pivot highs/lows into tolerant zones by scoring nearby clusters.
}
