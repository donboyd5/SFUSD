# filter(ccode==38, dcode==68478)


# notes -------------------------------------------------------------------

# Annual Financial Data
# Unaudited year-end data in downloadable, self-extracting files (i.e., SACS, J-200).
# Each year, California's kindergarten through grade twelve (Kâ12) school
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

# Beginning with fiscal year 2003â04, charter school financial reporting was
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
# more LEAs reported using SACS. In 2003â04, for the first time all LEAs
# reported in SACS. For fiscal year 2003â04, the CDE converted the SACS data
# into the old J-200 format for comparison purposes. However, beginning with
# fiscal year 2004â05 data, SACS data are no longer converted to the J-200
# format and only the SACS format is available.


# notes based on looking at 2019-20 ---------------------------------------

# For fiscal year 2019â20, 1,747 local educational agencies (LEAs) (989
# districts, 700 charter schools, and 58 county offices of education) in
# California submitted their year end unaudited actual (UA) financial data using
# the standardized account code structure (SACS).

# 1.	The Charters table includes charter schoolsâ Kâ12 average daily attendance (ADA). 
# 2.	The Kâ12 ADA in the LEAs table located within the SACS database do
# not include ADA for charter schools that filed UA data in their authorizing
# agencyâs General Fund (Fund 01). 
# 3.	The county and state totals are located in a separate table: UserGL_Totals.

# https://www.cde.ca.gov/ds/fd/fd/fielddescriptions.asp


# Key tables --------------------------------------------------------------
#.. UserGL  ----
# General ledger information submitted by LEAs that have a SACS based accounting system.
# Field	Data Type	Width	Description
# Ccode	Text	2	County Code
# Dcode	Text	5	District Code
# SchoolCode	Text	7	School Code
# Fiscalyear	Text	4	Fiscal Year
# Period	Text	4	Report Period Identifier ('A') = Unaudited Actual
# Colcode	Text	4	Column Code Identifier ('BA') = Unaudited Actual
# Account	Text	19	SACS Account Code
# Fund	Text	2	SACS Fund Code
# Resource	Text	4	SACS Resource Code
# Projectyear	Text	1	Project Year (for federally funded projects)
# Goal	Text	4	SACS Goal
# Function	Text	4	SACS Function
# Object	Text	4	SACS Object Code 
# Value	Number	Decimal (18,2)	Amount

#.. LEAs  ----
# Information on county offices of education, common administrative districts, joint powers agencies, and 
# elementary, high, and unified school districts that filed in SACS for fiscal year 2019â20.
# Field Name	Data Type	Width	Description
# Ccode	Text	2	County Code
# Dcode	Text	5	District Code
# Dname	Text	75	District Name
# Dtype	Text	15	District Type 
# K12ADA	Number	Double	Average Daily Attendance

# 2013 and prior have additional fields



#.. Fund ----
# SACS Fund codes included in the UserGL data.
# Field	Data Type	Width	Description
# Code	Text	4	Fund Code ID
# Title	Text	250	Fund Description


#.. Object ----
# SACS Object codes included in the UserGL data.
# Field	Data Type	Width	Description
# Code	Text	4	Object Code ID
# Title	Text	250	Object Description

#.. Charters ----
# The Charters table contains information about the Charter Schools that filed using either SACS or the Charter School Alternative Form (sometimes called the Charter Alt Form), as well as those that did not file year-end unaudited actual financial data for fiscal year 2019â20. 
# Field Name	Data Type	Width	Description
# Ccode	Text	2	County Code
# Dcode	Text	5	District Code
# SchoolID	Text	7	School Code
# CharterNumber	Text	10	Charter Number
# CharterName	Text	100	Charter Name
# ReportType	Text	100	Report Type
# ReportLevel	Text	30	Report Level 
# FundUsed	Text	30	Fund Used 
# K12ADA	Number	Double	Average Daily Attendance



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


#.. 3701 OPEB, Allocated, certificated positions ----
# Expenditures (1) for retirees and other former employees for current-year
# postemployment benefits other than pensions (OPEB) financed on a pay-as-you-go
# basis; or (2) for the amounts paid to an OPEB plan (administered through a
# qualifying trust) in excess of the current-year actuarially determined service
# cost. A qualifying trust is a trust or an equivalent arrangement that meets
# the criteria in paragraph 4 of GASB Statement 75. Do not include expenditures
# for service costs for active employees; these must be direct-charged using
# objects 3751â3752. Expenditures in objects 3701â3702 must be allocated to all
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
# employees; these must be allocated using objects 3701â3702. Expenditures in
# objects 3751â3752 must be direct-charged on a per-eligible-FTE basis to the
# same resource, goal, and function as the OPEB-eligible active employeeâs
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
# liability (that is, the total OPEB liability minus the OPEB planâs fiduciary
# net position) is reported. The total or net OPEB liability is reported only in
# the LEAâs accrual-basis financial statements.


# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(knitr)
library(kableExtra)
library(btools)
# library(gt)


# library(gridExtra)
# library(RcppRoll)
# library(ggrepel)
# library(ggbreak)
# library(patchwork)
# library(RColorBrewer)

library(readxl)


# local locations ---------------------------------------------------------------

dsacs <- r"(E:\data\CA_sacs/)"
dsacs_all <- paste0(dsacs, "data/allyears/")


# logical expressions -----------------------------------------------------

sfusd <- expression(ccode==38 & dcode==68478)


# file-stacking functions ------------------------------------------

codefix <- function(code){
  code <- as.character(code)  # to be safe -- at least one object code has a letter in it
  code <- str_pad(code, width=4, side="left", pad="0")
  code
}


fstack <- function(year, fname){
  print(year)
  indir <- paste0(dsacs, "data/", year, "/")
  fpath <- paste0(indir, fname)
  df <- readRDS(fpath) %>% 
    as_tibble() %>%
    mutate(across(where(is_character), str_trim),
           code=codefix(code),
           year=!!year)
  df
}

# LEAs ----
fleas <- function(year, fname){
  print(year)
  indir <- paste0(dsacs, "data/", year, "/")
  fpath <- paste0(indir, fname)
  df <- readRDS(fpath) %>% 
    as_tibble() %>%
    mutate(across(where(is_character), str_trim),
           dtypef=factor(
             dtype,
             levels=c("CO OFFICE", "Comm Admin Dist", "ELEMENTARY", "HIGH", "JPA", "UNIFIED"),
             labels=c("county", "admin", "elem", "hs", "jpa", "unified")) %>% as.character(),
           year=!!year)
  df
}

leas1 <- purrr::map_dfr(2004:2020, fleas, "leas.rds")
leas2 <- leas1 %>%
  mutate(k12ada_calc=ifelse(is.na(k12ada),
                            naz(regularada) + naz(specialedada),
                            k12ada))
leas2 %>%
  filter(eval(sfusd))
leas2 %>%
  filter(!is.na(k12ada)) %>%
  mutate(check=k12ada_calc - k12ada) %>%
  filter(check != 0)

leas <- leas2 %>%
  select(-k12ada) %>%
  rename(k12ada=k12ada_calc)

outdir <- paste0(dsacs, "data/allyears/")
outpath <- paste0(outdir, "leas.rds")
saveRDS(leas, file=outpath)

# read to be sure
leas <- readRDS(paste0(dsacs_all, "leas.rds"))  # 1 rec per district per year
glimpse(leas)

count(leas, year)
count(leas, dtype, dtypef)
count(leas, year, dtypef) %>%
  pivot_wider(names_from = dtypef,
              values_from = n)


# usergl ----
year <- 2020
indir <- paste0(dsacs, "data/", year, "/")
fname <- "usergl.rds"
fpath <- paste0(indir, fname)
df <- readRDS(fpath) %>% as_tibble()
glimpse(df)
ht(df)
df

fgl <- function(year, fname){
  print(year)
  indir <- paste0(dsacs, "data/", year, "/")
  fpath <- paste0(indir, fname)
  df <- readRDS(fpath) %>% 
    as_tibble() %>%
    rename(func=`function`) %>% # apparently func is a reserved word
    mutate(across(where(is_character), str_trim),
           across(c(fund, resource, goal, func, object), codefix),
           year=!!year,
           projectyear=as.integer(projectyear))  # it is text in 2020; we're going to drop anyway
  df
}

usergl <- purrr::map_dfr(2004:2020, fgl, "usergl.rds")
outdir <- paste0(dsacs, "data/allyears/")
outpath <- paste0(outdir, "usergl.rds")
saveRDS(usergl, file=outpath)  # takes a while

# read to be sure
usergl <- readRDS(paste0(dsacs_all, "usergl.rds"))
glimpse(usergl)
summary(usergl) # no NAs except in projectyear, which we won't use


# object ----
year <- 2020
indir <- paste0(dsacs, "data/", year, "/")
fname <- "object.rds"
fpath <- paste0(indir, fname)
df <- readRDS(fpath) %>% as_tibble()
glimpse(df)
ht(df)
df

object <- purrr::map_dfr(2004:2020, fstack, "object.rds")
outdir <- paste0(dsacs, "data/allyears/")
outpath <- paste0(outdir, "object.rds")
saveRDS(object, file=outpath)


# read to be sure
object <- readRDS(paste0(dsacs_all, "object.rds"))
glimpse(object)
count(object, code) %>% ht


# stack function fund goal resource ---------------------------------------
# function -- call it fund to avoid confusion with reserved word
func <- purrr::map_dfr(2004:2020, fstack, "function.rds")
saveRDS(func, file=paste0(dsacs_all, "func.rds"))
readRDS(paste0(dsacs_all, "func.rds"))

# fund 
fund <- purrr::map_dfr(2004:2020, fstack, "fund.rds")
saveRDS(fund, file=paste0(dsacs_all, "fund.rds"))
readRDS(paste0(dsacs_all, "fund.rds"))

# goal 
goal <- purrr::map_dfr(2004:2020, fstack, "goal.rds")
saveRDS(goal, file=paste0(dsacs_all, "goal.rds"))
readRDS(paste0(dsacs_all, "goal.rds"))

# resource
resource <- purrr::map_dfr(2004:2020, fstack, "resource.rds")
saveRDS(resource, file=paste0(dsacs_all, "resource.rds"))
readRDS(paste0(dsacs_all, "resource.rds"))


# make merged opeb-focused analytic file -----------------------------------------------
#.. get all the data ----

leas <- readRDS(paste0(dsacs_all, "leas.rds"))  # 1 rec per district per year
usergl <- readRDS(paste0(dsacs_all, "usergl.rds"))
object <- readRDS(paste0(dsacs_all, "object.rds"))

#   setNames(str_replace(names(.), "ts1_", "")) # drop prefixes?

opeb_codes <- c("3701", "3702", "3751", "3752", "9664")
object %>%
  filter(code %in% opeb_codes)

object %>% filter(code=="9664")  # year groups with same name
# 9664  Other Postemployment Benefits  2004:2005
# 9664  Net OPEB Obligation            2006:2016
# 9664  Total/Net OPEB Liability       2017:2020
# doesn't really matter - few districts report this

opeb_sacs1 <- usergl %>%
  filter(object %in% opeb_codes) %>%
  left_join(leas %>%
              select(year, ccode, dcode, dname, dtypef, k12ada), 
            by = c("ccode", "dcode", "year"))
summary(opeb_sacs1)
# good no NA values

# determine what we can drop
count(opeb_sacs1, schoolcode) # almost all 0, but keep until we know more
check <- opeb_sacs1 %>% filter(schoolcode!=0)

count(opeb_sacs1, year, fiscalyear)
opeb_sacs1 %>%
  filter(year != (fiscalyear + 1)) %>%
  select(ccode, dcode, dname, dtypef, year, fiscalyear, k12ada)
# no recs, so we can drop fiscal year - for some reason cde defines fiscal year as the cy in which the fy starts

count(opeb_sacs1, object)  # only 227 records (of 722k) with 9664
count(opeb_sacs1, period)  # drop - all A
count(opeb_sacs1, colcode) # drop - all BA
count(opeb_sacs1, account) # drop - looks like a combination of other codes
count(opeb_sacs1, fund)  # keep - many values
count(opeb_sacs1, resource)  # keep - many values
count(opeb_sacs1, projectyear) # 0-9 and NA???
count(opeb_sacs1, goal) # keep - many values
count(opeb_sacs1, func) # keep - many values

opeb_sacs <- opeb_sacs1 %>%
  select(-c(fiscalyear, period, colcode, account))

saveRDS(opeb_sacs, paste0(dsacs_all, "opeb_sacs.rds"))


# opeb checks ------------------------------------------------------------------
opeb_sacs <- readRDS( paste0(dsacs_all, "opeb_sacs.rds"))

opeb_sacs %>%
  filter(eval(sfusd)) %>% ht

sfusd_opeb <- opeb_sacs %>%
  filter(eval(sfusd)) %>%
  filter(object %in% c("3701", "3702")) %>%
  mutate(object=factor(object, 
                       levels=c("3701", "3702"),
                       labels=c("certificated", "classified")))

sfusd_opeb %>%
  filter(year==2020)

count(sfusd_opeb %>%
        filter(year==2020),
      fund)

tmp <- sfusd_opeb %>%
  group_by(year, object) %>%
  summarise(value=sum(value) / 1e6) %>%
  pivot_wider(names_from = object) %>%
  mutate(total=certificated + classified)

tmp %>%
  ggplot(aes(year, total)) +
  geom_line()

# where/how did SFUSD opeb drop between 2019 and 2020?


# get all sacs data -------------------------------------------------------
all_sacs <- usergl %>%
  left_join(leas %>%
              select(year, ccode, dcode, dname, dtypef, k12ada), 
            by = c("ccode", "dcode", "year"))
saveRDS(all_sacs, paste0(dsacs_all, "all_sacs.rds"))
all_sacs <- readRDS(paste0(dsacs_all, "all_sacs.rds"))
glimpse(all_sacs)
summary(all_sacs)

# find non-numeric codes
count(all_sacs, val=resource) %>%
  mutate(check=as.numeric(val)) %>%
  filter(is.na(check))
# non-numeric codes as comment
tmp <- count(all_sacs, object) # 979Z, PCRA
tmp <- count(all_sacs, fund) # none
tmp <- count(all_sacs, goal)  # ADLT, CAFE, CHILD
tmp <- count(all_sacs, func)  # none
tmp <- count(all_sacs, resource) # none


# check sacs data ---------------------------------------------------------
all_sacs <- readRDS(paste0(dsacs_all, "all_sacs.rds"))
sf <- all_sacs %>%
  filter(eval(sfusd))

tmp <- sf %>%
  filter(year==2020)
