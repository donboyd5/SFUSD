

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

# include current expense, based
# https://www.cde.ca.gov/ds/fd/ec/currentexpense.asp
# plus Nov 15, 2021 email to my dboyd@albany.edu account from
# Kevin Turner for SACSINFO
# California Department of Education
# Financial Accountability & Information Services
# 1430 N Street, Suite 3800
# Sacramento, CA 95814
# 916-322-1770

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

# Current expense of education. The current general fund operating expenditures
# of an LEA for kindergarten and grades one through twelve, excluding
# expenditures for food services, community services, nonagency activities,
# fringe benefits for retired persons, acquisition and construction of
# facilities, and objects 6000 and 7000.

# Screenshot of Form CEA from Kevin Turner suggests deductions are:
# Nonagency -- goals 7100-7199
# Community Services -- goal 8100
# Food Services -- function 3700
# Fringe Benefits for retired persons object -- 3701-3702
# Facilities Acquisition and Construction  function 8500


all_sacs <- readRDS(paste0(dsacs_all, "all_sacs.rds"))
summary(all_sacs)


# object codes of items included in gross expenditures
certsals <- 1000:1999
classsals <- 2000:2999
empben <- 3000:3999
books <- 4000:4999
equip <- 6500:6599  # this is what the website says
# equip <- 6400:6599  # this includes equip purchase
service <- 5000:5999
icost <- 7300:7399
# see csam procedure 330
# 1000 Certificated Salaries
# 2000 Classified Salaries
# 3000 Employee Benefits
# 4000 Books and Supplies
# 6500 Equipment Replacement
# 5000 & 7300 Services and Indirect Costs
gross_objects <- c(certsals, classsals, empben, service, icost)


# now define deductions
# Nonagency -- goals 7100-7199
# Community Services -- goal 8100
# Food Services -- function 3700
# Fringe Benefits for retired persons object -- 3701-3702
# Facilities Acquisition and Construction  function 8500
deduct_nonagency_goals <- c(7100:7199) %>% as.character
deduct_commservice_goals <- 8100 %>% as.character
deduct_food_funcs <- 3700
deduct_fringeret_objects <- c(3701, 3702)
deduct_facilities_funcs <- 8500


# sacs_summary ----
# summary(all_sacs)

# object codes of items included in gross expenditures
certsals <- 1000:1999
classsals <- 2000:2999
empben <- 3000:3999
books <- 4000:4999
equip <- 6500:6599
service <- 5000:5999
icost <- 7300:7399
# see csam procedure 330
# 1000 Certificated Salaries
# 2000 Classified Salaries
# 3000 Employee Benefits
# 4000 Books and Supplies
# 6500 Equipment Replacement
# 5000 & 7300 Services and Indirect Costs

# now define deductions
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

# add some additional variables to summarize

# 3101–3102
# State Teachers’ Retirement System. Record expenditures to provide personnel
# with retirement benefits under the State Teachers’ Retirement System (STRS).
# This excludes employee contributions. Object 3101 is certificated personnel in
# STRS; Object 3102 includes those individuals who hold classified positions but
# are enrolled in STRS.

# 3201–3202
# Public Employees’ Retirement System. Record expenditures to provide personnel
# with retirement benefits under the Public Employees’ Retirement System (PERS).
# This excludes employee contributions, although it does include the employer’s
# payment of an employee’s contribution. Object 3201 indicates those employees
# in certificated positions and enrolled in PERS; Object 3202 indicates
# employees in classified positions and enrolled in PERS.

# 3401–3402
# Health and Welfare Benefits. Record expenditures made to provide personnel
# with health and welfare insurance benefits. This excludes employee
# contributions but includes health and welfare benefit premiums paid to a
# self-insurance fund. Object 3401 indicates that the benefits cover
# certificated positions; Object 3402 indicates that the benefits cover
# classified positions.


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
         
         erctrs=object %in% c(3101, 3102),
         ercpers=object %in% c(3201, 3202),
         health=object %in% c(3401, 3402)
         
         ) %>%
  group_by(year, ccode, dcode, dname) %>%
  summarise(across(
    c(gross, deduct,
      certsals, classsals, empben, books, equip, service, icost,
      dnonagency, dcommservice, dfood, dfringeret, dfacilities,
      
      o3701, o3702, erctrs, ercpers, health),
    ~ sum(.x * value, na.rm=TRUE)),
    .groups="drop") %>%
  mutate(current=gross - deduct,
         gfopeb=o3701 + o3702,
         ercpen=erctrs + ercpers)

saveRDS(sacs_gfcurrent, file=paste0(dsacs_all, "sacs_gfcurrent.rds"))


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

