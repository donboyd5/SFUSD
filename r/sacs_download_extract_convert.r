

# CAUTION -----------------------------------------------------------------
# Reading Microsoft Access files is tricky.

# you will need the package RODBC

# First, you have to load this program using 32-bit R because it reads MS ACCESS files.
# (Tools\Global options)
# Oddly, Microsoft has not provided the ability to read these files with 64-bit R.
# 64-bit R is fine for other programs after the Access files are converted.

# Second, the older .mdb Access files can be read using
#   odbcConnectAccess(fpath)
# where fpath is the full path (including file name) to the .mdb file.

# Third, oddly, the newer .accdb Access fles must be read using
#   odbcConnectAccess2007(fpath)
# where fpath is the full path (including file name) to the .accdb file.


# Notes -------------------------------------------------------------------

# download, extract, and convert the CDE (California Department of Education) SACS data for multiple years

# There is a little bit of documentation on the CDE site for the data, and I have downloaded various documents
# that give more. I have sent an email to CDE asking for the data collection form (J-90) and instructions
# but have not heard back yet.

# Annual Financial Data
# Unaudited year-end data in downloadable, self-extracting files (i.e., SACS, J-200).
# Each year, California's kindergarten through grade twelve (K–12) school
# districts, county offices of education, charter schools, and joint powers
# agencies, all commonly known as local educational agencies (LEAs), submit
# annual financial reports to the California Department of Education (CDE). The
# Financial Accountability and Information Services (FAIS) Office, within the
# CDE, is responsible for annually collecting, reviewing, and preparing these
# financial data for dissemination.
 
# In 1997, a Standardized Account Code Structure (SACS) was developed and
# implemented by LEAs over the next seven years. At the same time, the SACS
# Financial Reporting Software was developed to accommodate SACS accounting. The
# use of SACS and the SACS Financial Reporting Software helps facilitate
# consistency among LEAs in recording and reporting financial information.

# Beginning with fiscal year 2003–04, charter school financial reporting was
# required by Education Code sections 1628External link opens in new window or
# tab. and 42100External link opens in new window or tab. (as amended by
# Assembly Bill 1994, Chapter 1058, Statutes of 2002). The Charter School
# Alternative Form is an option available for charter schools to prepare their
# unaudited actual financial reports without using SACS and the SACS Financial
# Reporting Software.

# LEA J-200 Financial Data
# During transition to SACS reporting, the J-200 Unaudited Actual Financial
# Reports continued to be used by some LEAs to report their balance sheet
# accounts, revenues by sources, and expenditures by types. Gradually, more and
# more LEAs reported using SACS. In 2003–04, for the first time all LEAs
# reported in SACS. For fiscal year 2003–04, the CDE converted the SACS data
# into the old J-200 format for comparison purposes. However, beginning with
# fiscal year 2004–05 data, SACS data are no longer converted to the J-200
# format and only the SACS format is available.


# sacs urls ----------------------------------------------------------------
# https://www.cde.ca.gov/ds/fd/fd/   landing page
# http://www.ed-data.org/  explorer for selected data
# http://www.ed-data.org/district/San-Francisco/San-Francisco-Unified

# same file naming convention 2004-2020
# https://www3.cde.ca.gov/fiscal-downloads/sacs_data/2019-20/sacs1920.exe
# https://www3.cde.ca.gov/fiscal-downloads/sacs_data/2003-04/sacs0304.exe

# earlier years are in a different format

# for now just work with 2004-2020


# sacs opeb information --------------------------------------------------------

# https://www.cde.ca.gov/fg/ac/sa/  landing page for accounting information
# https://www.cde.ca.gov/fg/ac/ac/validcodes.asp valid codes and combinations

# California School Accounting Manual (CSAM)
# https://www.cde.ca.gov/fg/ac/sa/documents/csam2019complete.pdf
# Procedure 785 pp.479-501 details OPEB-related reporting


# 3701 OPEB, Allocated, certificated positions
# Expenditures (1) for retirees and other former employees for current-year
# postemployment benefits other than pensions (OPEB) financed on a pay-as-you-go
# basis; or (2) for the amounts paid to an OPEB plan (administered through a
# qualifying trust) in excess of the current-year actuarially determined service
# cost. A qualifying trust is a trust or an equivalent arrangement that meets
# the criteria in paragraph 4 of GASB Statement 75. Do not include expenditures
# for service costs for active employees; these must be direct-charged using
# objects 3751–3752. Expenditures in objects 3701–3702 must be allocated to all
# activities in proportion to total salaries or total full-time equivalents
# (FTEs) in those activities. Object 3701 relates to certificated positions;
# Object 3702 relates to classified positions.

# 3702 OPEB, Allocated, classified positions 
# same text

# 3751 OPEB, Active Employees, certificated positions 
# Expenditures for the amounts paid to a OPEB plan (administered through a
# qualifying trust) up to the current-year actuarially determined service costs
# for OPEB-eligible active employees. A qualifying trust is a trust or an
# equivalent arrangement that meets the criteria in paragraph 4 of GASB
# Statement 75. Do not include expenditures for retirees and other former
# employees; these must be allocated using objects 3701–3702. Expenditures in
# objects 3751–3752 must be direct-charged on a per-eligible-FTE basis to the
# same resource, goal, and function as the OPEB-eligible active employee’s
# salary. Object 3751 relates to certificated positions; Object 3752 relates to
# classified positions.

# 3752 OPEB, Active Employees, classified positions
# same text

# 9664 Total/Net OPEB Liability
# The total OPEB liability is the portion of the actuarial present value of
# projected benefit payments that is attributed to past periods of employee
# service, measured in conformity with the requirements of GASB Statement 75.
# For a defined benefit OPEB plan that is not administered through a trust that
# meets the criteria in paragraph 4 of GASB 75 (specified criteria), the total
# OPEB liability is reported. For a defined benefit OPEB plan that is
# administered through a trust that meets the specified criteria, a net OPEB
# liability (that is, the total OPEB liability minus the OPEB plan’s fiduciary
# net position) is reported. The total or net OPEB liability is reported only in
# the LEA’s accrual-basis financial statements.


# ms access notes ---------------------------------------------------------
# https://www.roelpeters.be/solved-importing-microsoft-access-files-accdb-mdb-in-r/
# If your tables aren’t huge (+4GB), you can simply change R to the 32-bit version.

# https://newbedev.com/how-to-read-data-from-microsoft-access-accdb-database-files-into-r
# You may need to run the 32-bit c:\windows\sysWOW64\odbcad32.exe if running 64-bit Windows

# I (djb) examined alternative approaches but in the end concluded that the best thing to do
# is run this file with 32-bit R to extract and convert the files from MS Access, and do all
# subsequent analysis with 64-bit R.


# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(RODBC)


# local locations ---------------------------------------------------------------

sacsdir <- r"(E:\data\CA_sacs/)"


# ONETIME: sacs download ------------------------------------------------------------
urlbase <- "https://www3.cde.ca.gov/fiscal-downloads/sacs_data/"
# https://www3.cde.ca.gov/fiscal-downloads/sacs_data/2019-20/sacs1920.exe

for(year in 2004:2020){
  print(year)
  web_folder <- paste0(year - 1, "-", str_sub(year, 3, 4), "/")
  fname_years <- paste0(str_sub(year - 1, 3, 4), str_sub(year, 3, 4))
  fname <- paste0("sacs", fname_years, ".exe")
  url <- paste0(urlbase, web_folder, fname)
  # print(url)
  fpath <- paste0(sacsdir, fname)
  # print(fpath)
  download.file(url, fpath, mode="wb")
}


# ONETIME: sacs extract -------------------------------------------------------------
for(year in 2004:2020){
  fname_years <- paste0(str_sub(year - 1, 3, 4), str_sub(year, 3, 4))
  fname <- paste0("sacs", fname_years, ".exe")
  fpath <- paste0(sacsdir, fname)
  args <- c("/auto", paste0(sacsdir, "data/", year, "/"))
  print(fpath)
  system2(command=fpath, args=args, invisible = TRUE)  # will create year folder if does not exist
}


# sacs convert -------------------------------------------------------------
# data are in Access databases from 2004-2020
# to get the Access data, this R program needs to be using 32-bit R, NOT 64-bit R

# all are mdb files
# 2003-04 has the filename format shown below:
#   SACS2003_04.mdb
# 2004-05 through 2019-20 have the following filename format
#   sacs0405.mdb

for(year in 2004:2020){
  if(year==2004) {
    fname <- "SACS2003_04.mdb"
  } else {
    fname_years <- paste0(str_sub(year - 1, 3, 4), str_sub(year, 3, 4))
    fname <- paste0("sacs", fname_years, ".mdb")
  }
  fpath <- paste0(sacsdir, "data/", year, "/", fname)
  print(fpath)
  
  con <- odbcConnectAccess(fpath)
  tables <- sqlTables(con)
  tabnames <- tables %>%
    filter(TABLE_TYPE=="TABLE") %>%
    .$TABLE_NAME

  for(tab in tabnames){
    print(tab)
    df <- sqlFetch(con, tab) %>%
      setNames(str_trim(str_to_lower(names(.))))

    rdspath <- paste0(sacsdir, "data/", year, "/", str_to_lower(tab), ".rds")
    saveRDS(df, file=rdspath)
  }

  odbcClose(con)
}


# sacs check ---------------------------------------------------------------
# make sure every rds file is readable
for(year in 2004:2020){
  print(year)
  
  folder <- paste0(sacsdir, "data/", year, "/")
  flist <- list.files(folder, pattern=".rds", full.names = TRUE)
  
  for(file in flist){
    print(file)
    df <- readRDS(file)
    print(names(df))
    print(nrow(df))
  }
}


# If all good, close R and reopen as 64-bit R to work with the data

