

# documentation on selected sacs codes ------------------------------------
#.. 3101–3102 State Teachers’ Retirement System ----
# State Teachers’ Retirement System. Record expenditures to provide personnel
# with retirement benefits under the State Teachers’ Retirement System (STRS).
# This excludes employee contributions. Object 3101 is certificated personnel in
# STRS; Object 3102 includes those individuals who hold classified positions but
# are enrolled in STRS.

#.. 3201–3202 Public Employees’ Retirement System ----
# Public Employees’ Retirement System. Record expenditures to provide personnel
# with retirement benefits under the Public Employees’ Retirement System (PERS).
# This excludes employee contributions, although it does include the employer’s
# payment of an employee’s contribution. Object 3201 indicates those employees
# in certificated positions and enrolled in PERS; Object 3202 indicates
# employees in classified positions and enrolled in PERS.

#.. 3401–3402 Health and Welfare Benefits ----
# Health and Welfare Benefits. Record expenditures made to provide personnel
# with health and welfare insurance benefits. This excludes employee
# contributions but includes health and welfare benefit premiums paid to a
# self-insurance fund. Object 3401 indicates that the benefits cover
# certificated positions; Object 3402 indicates that the benefits cover
# classified positions.


#.. 3701 OPEB, Allocated, certificated positions ----
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

#.. 3702 OPEB, Allocated, classified positions ----
# same text

#.. 3751 OPEB, Active Employees, certificated positions ----
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

#.. 3752 OPEB, Active Employees, classified positions----
# same text

#.. 9664 Total/Net OPEB Liability ----
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

# I don't bother to include 9664 because almost no districts report it.

# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(scales)
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


# create a district-by year general fund file with major categories of spending, plus OPEB ----

# sacs_summary ----
# summary(all_sacs)

#.. define codes needed for summaries ----

# include current expense, based on
# https://www.cde.ca.gov/ds/fd/ec/currentexpense.asp
# plus Nov 15, 2021 email to my dboyd@albany.edu account from
# Kevin Turner for SACSINFO
# California Department of Education
# Financial Accountability & Information Services
# 1430 N Street, Suite 3800
# Sacramento, CA 95814
# 916-322-1770

# Current expense of education. The current general fund operating expenditures
# of an LEA for kindergarten and grades one through twelve, excluding
# expenditures for food services, community services, nonagency activities,
# fringe benefits for retired persons, acquisition and construction of
# facilities, and objects 6000 and 7000.

# see csam procedure 330

# items included in gross expenditures
# 1000 Certificated Salaries
# 2000 Classified Salaries
# 3000 Employee Benefits
# 4000 Books and Supplies
# 6500 Equipment Replacement
# 5000 & 7300 Services and Indirect Costs

# From the total expenditures reported in the above accounts, costs for the
# following categories are deducted: (1) Non-agency activities; (2) Community
# Services; (3) Food Services; (4) Fringe Benefits for Retired Persons; and (5)
# Facilities Acquisition and Construction.

certsals <- 1000:1999
classsals <- 2000:2999
empben <- 3000:3999
books <- 4000:4999
equip <- 6500:6599
service <- 5000:5999
icost <- 7300:7399

# Screenshot of Form CEA from Kevin Turner suggests deductions are:
# Nonagency -- goals 7100-7199
# Community Services -- goal 8100
# Food Services -- function 3700
# Fringe Benefits for retired persons object -- 3701-3702
# Facilities Acquisition and Construction  function 8500
deduct_nonagency_goals <- c(7100:7199) 
deduct_commservice_goals <- 8100
deduct_food_funcs <- 3700
deduct_fringeret_objects <- c(3701, 3702)
deduct_facilities_funcs <- 8500


#.. Dealing with state payments to TRS on behalf of districts ----

# Some of what I (djb) write here is a hypothesis that still needs to be verified, but I am highly confident it is correct.

# In 2014, the California legislature adopted AB1469, intended to put CalSTRS on stronger financial footing. As part of that, state required payments to CalSTRS were defined, and district required payments to CalSTRS were defined (previously they had been vague), and in addition the state began to make payments on behalf of school districts.

# These state payments on behalf of school districts were included in the CDE SACS data as if they were payments by the school districts. Thus, in addition to the fact that district payments to CalSTRS made with their own funds increased due to AB1469, REPORTED district payments also increased because of the state payments on behalf of districts that are recorded as district spending. BOTH kinds of payments are included in codes 3101 and 3102 (pension contributions to CalSTRS for certificated and classified employees respectively).

# To make apples-to-apples comparisons over time, we should subtract these "on behalf" payments from the reported payments, AFTER we calculate current educational expense. (As defined by CDE, current educational expense includes these payments so we want to keep them in the offical definition, but for our own analtic purposes we will want to subtract them.) These payments can be identified by the combination of object 8590 and resource 7690 (CSAM Jan 2019 p.310-15). We therefore include these on-behalf payments in the summary file so that they can be subtracted from current education expense.


#.. Create the summary file ----
all_sacs <- readRDS(paste0(dsacs_all, "all_sacs.rds"))
summary(all_sacs)

# this may take 10-20 secs
sacs_gfcurrent <- all_sacs %>%
  filter(fund=="0001") %>%  # general fund
  mutate(certsals=object %in% certsals,
         classsals=object %in% classsals,
         empben=object %in% empben,
         books=object %in% books,
         equip=object %in% equip,
         service=object %in% service,
         icost=object %in% icost,
         gross=certsals | classsals | empben | books | equip | service | icost,
         
         dnonagency=(goal %in% deduct_nonagency_goals) & gross,
         dcommservice=(goal %in% deduct_commservice_goals) & gross,
         dfood=(func %in% deduct_food_funcs) & gross,
         dfringeret=(object %in% deduct_fringeret_objects) & gross,
         dfacilities=(func %in% deduct_facilities_funcs) & gross,
         
         # avoid double-deductions
         deduct=dnonagency | dcommservice | dfood | dfringeret | dfacilities,
         
         # other variables we want
         o3701=object=="3701",
         o3702=object=="3702",
         o3751=object=="3751",
         o3752=object=="3752",
         
         erctrs=object %in% c(3101, 3102),
         onbehalf=object=="8590" & resource=="7690", # state payments to CalSTRS on behalf of districts
         ercpers=object %in% c(3201, 3202),
         health=object %in% c(3401, 3402)
         
         ) %>%
  group_by(year, ccode, dcode, dname) %>%
  summarise(across(
    c(gross, deduct,
      certsals, classsals, empben, books, equip, service, icost,
      dnonagency, dcommservice, dfood, dfringeret, dfacilities,
      
      o3701, o3702, o3751, o3752,
      erctrs, onbehalf, ercpers, health),
    ~ sum(.x * value, na.rm=TRUE)),
    .groups="drop") %>%
  mutate(gfopebretired=o3701 + o3702,
         gfopebactives=o3751 + o3752,
         gfopeb=gfopebretired + gfopebactives,
         ercpen=erctrs + ercpers,
         erctrsadj=erctrs - onbehalf,
         ercpenadj=erctrsadj + ercpers,
         
         current=gross - deduct,
         currentadj=current + gfopebretired - onbehalf
         )

saveRDS(sacs_gfcurrent, file=paste0(dsacs_all, "sacs_gfcurrent.rds"))


summary(sacs_gfcurrent)


# Check the general fund summary file -------------------------------------
sacs_gfcurrent <- readRDS(file=paste0(dsacs_all, "sacs_gfcurrent.rds"))
sacs_gfcurrent %>%
  filter(year==2018, str_detect(dname, "Alameda"), dcode==61119) %>%
  select(dname, gross, deduct, current)


# SFUSD current expense targets from CDE web page:
# 2017  616,535,632
# 2018  729,813,887
# 2019  817,101,561.65
# 2020  840,100,892.92
sacs_gfcurrent %>%
  filter(year >= 2017, eval(sfusd)) %>%
  select(dname, gross, deduct, current)
# all good


sacs_gfcurrent %>%
  filter(eval(sfusd)) %>%
  select(year, starts_with("o37"), gfopeb) %>%
  pivot_longer(cols = -year) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line()


# all_sacs %>%
#   filter(eval(sfusd), object %in% c(3701, 3702)) %>%
#   group_by(year, ccode, dcode, dname, object) %>%
#   summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
#   mutate(group=ifelse(object==3701, "teacher", "other")) %>%
#   ggplot(aes(year, value / value[year==2010], colour=group)) +
#   geom_line() +
#   geom_point() +
#   geom_hline(yintercept = 1)




# OLD - data for comparisons to reported current expense ----

path <- r"(C:\Users\donbo\Downloads\SFUSD\currentexpense1718.xlsx)"
currx1 <- read_excel(path, skip=10)
currx1
currx <- currx1 %>%
  select(1:4) %>%
  setNames(c("ccode", "dcode", "dname", "edp365")) %>%
  mutate(ccode=as.integer(ccode),
         dcode=as.integer(dcode))

