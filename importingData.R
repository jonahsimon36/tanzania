## Jonah Simon
## Import Arc Outputs and DHS data

# Load libraries
library(tidyverse)
library(haven)
library(readxl)

# Import DHS data files

children2010 <- read_dta("~/Google Drive/Middlebury/Econ Research/symposium/DHSData/DHS2010/TZKR63DT/TZKR63FL.DTA")
individual2010 <- read_dta("~/Google Drive/Middlebury/Econ Research/symposium/DHSData/DHS2010/TZIR63DT/TZIR63FL.DTA")
household2010 <- read_dta("~/Google Drive/Middlebury/Econ Research/symposium/DHSData/DHS2010/TZHR63DT/TZHR63FL.DTA")

children2015 <- read_dta("~/Google Drive/Middlebury/Econ Research/symposium/DHSData/DHS2015/TZKR7HDT/TZKR7HFL.DTA")
individual2015 <- read_dta("~/Google Drive/Middlebury/Econ Research/symposium/DHSData/DHS2015/TZIR7HDT/TZIR7HFL.DTA")
household2015 <- read_dta("~/Google Drive/Middlebury/Econ Research/symposium/DHSData/DHS2015/TZHR7HDT/TZHR7HFL.DTA")

# Import ArcGIS LossYear output tables from Excel
for (year in c(7,8,9,10,12,13,14,15,16)){
  assign(sprintf("DHS2010_LossYear%s", year),read_excel(sprintf("~/Google Drive/Middlebury/Econ Research/symposium/arcToExcelOutput/LossYear/DHS2010_LossYear%s.xls",year)))
  assign(sprintf("DHS2015_LossYear%s", year),read_excel(sprintf("~/Google Drive/Middlebury/Econ Research/symposium/arcToExcelOutput/LossYear/DHS2015_LossYear%s.xls",year)))
  assign(sprintf("DHS2010_LossYear%s", year), select(get(sprintf("DHS2010_LossYear%s", year)),DHSID,COUNT))
  assign(sprintf("DHS2015_LossYear%s", year), select(get(sprintf("DHS2015_LossYear%s", year)),DHSID,COUNT))
}

# Import ArcGIS Tree Cover output tables from Excel
treeCover2010 <- read_excel("~/Google Drive/Middlebury/Econ Research/symposium/arcToExcelOutput/TreeCover/DHS2010_TreeCover.xls")
treeCover2015 <- read_excel("~/Google Drive/Middlebury/Econ Research/symposium/arcToExcelOutput/TreeCover/DHS2015_TreeCover.xls")
