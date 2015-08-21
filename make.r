library(dplyr)
library(rvest)
library(stringr)

library(ggplot2)
library(grid)

library(network)
library(sna)

dir.create("raw", showWarnings = FALSE)

colors = c(
  "UKIP" = "#984EA3", # purple
  "CON" = "#377EB8",  # blue
  "LD" = "#FF7F00",   # orange
  "LAB" = "#E41A1C",  # red
  "R" = "#B2182B",    # dark red
  "GP" = "#4DAF4A",   # green
  # Wales
  "W-PLAID" = "#B3DE69", # light green
  # N. Ireland
  "NI-ALL" = "#FFFFB3",  # light yellow
  "NI-DUP" = "#C51B7D",  # red -- magenta
  "NI-UUP" = "#053061",  # dark blue
  "NI-SDLP" = "#1B9E77", # dark green
  "NI-UKUP" = "#80B1D3", # light blue
  # Scotland
  "S-LAB" = "#FB8072", # light red
  "S-SNP" = "#FFFF33", # yellow
  "IND" = "#AAAAAA"    # light grey
)

source("01-data.r")
source("02-build.r")
