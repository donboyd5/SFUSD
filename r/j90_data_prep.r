
# Notes -------------------------------------------------------------------

# trim and stack the CDE (California Department of Education) J-90 data for multiple years
# Report of Teacher Salary and Benefit Data (Form J-90)

# There is a little bit of documentation on the CDE site for the data, and I have downloaded various documents
# that give more. I have sent an email to CDE asking for the data collection form (J-90) and instructions
# but have not heard back yet.


# j90 urls ----------------------------------------------------------------
# https://www.cde.ca.gov/ds/fd/cs/   landing page

# same file naming convention 2004-2020
# https://www3.cde.ca.gov/ds-downloads/fd/cs/j901920.exe  2019-20 full package
# https://www3.cde.ca.gov/ds-downloads/fd/cs/j900304.exe 2003-04 full package

# earlier years
# https://www3.cde.ca.gov/ds-downloads/fd/cs/j90_0203.exe
# https://www3.cde.ca.gov/ds-downloads/fd/cs/j90_9900.exe

# for now just work with 2004-2020


# j90 information ---------------------------------------------------------

# TSAL117 File Contains information from sections I, III, IV, V, and VI of the J-90
# TSAL217 File Contains the text headings entered for each of the columns in the salary schedule, Section II
# TSAL317 File Contains the salary and FTEs for each column and salary in Section II
# Tsal417 File Contains the benefit information in Section VII
# TSAL517 File Contains the benefit information in Section VIII for retirees over 65
# TSAL617 File Contains the benefit information in Section VIII for retirees 65 and under

# TSAL517 and TSAL617 have essentially the same information. Here is TSAL617:
# County Two digit county code
# District Five digit district code
# CDS Seven digit combined code
# TS6_BenA one character code indicating type of benefit.  H for heath, D for dental, V for vision, L for life, O for other
# TS6_Desc The name of the plan
# TS6_Step The health benefit plans ID.  The first is always 1, the second and subsequent like plans are number consequently. This is calculated by the input program
# TS6_Column A column 1 is always Single, 2 is two party plan, 3 is family plan, 4 is composite rate
# TS6_Annual The annual cost of plan
# TS6_Contr The amount the district contributes to the cost of the plan
# TS6_FTE The number of retired teachers (age 65 or under) participating in the plan
# TS6_ID number that is generated when a J-90 is submitted. Each record has an ID number

# TS1_Maxcaf The maximum annual employer contributions to cafeteria plan.
# TS1_Maxsin The maximum annual employer contributions to single plan per employee.
# TS1_Maxtwo The maximum annual employer contributions to a two party plan per employee.
# TS1_Maxfam The maximum annual employer contributions to a family plan per employee.
# TS1_CAPTYPE Is the cap hard or soft or NA

# Ben_life	Does district cover retirees for life? (Y/N)
# Ben_stop	If not for life, when do benefits stop? Age or years
# H_plan	Number of Health plans
# D_plan	Number of dental plans
# V_plan	Number of vision plans
# L_plan	Number of life plans
# O_plan	Number of other plans


# County Two digit county code
# District Five digit district code
# CDS Seven digit combined code
# TS6_BenA one character code indicating type of benefit.  H for heath, D for dental, V for vision, L for life, O for other
# TS6_Desc The name of the plan
# TS6_Step The health benefit plans ID.  The first is always 1, the second and subsequent like plans are number consequently. This is calculated by the input program
# TS6_Column A column 1 is always Single, 2 is two party plan, 3 is family plan, 4 is composite rate
# TS6_Annual The annual cost of plan
# TS6_Contr The amount the district contributes to the cost of the plan
# TS6_FTE The number of retired teachers (age 65 or under) participating in the plan
# TS6_ID number that is generated when a J-90 is submitted. Each record has an ID number


# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
# library(kableExtra)
library(btools)
# library(gt)
# library(knitr)

# library(gridExtra)
# library(RcppRoll)
# library(ggrepel)
# library(ggbreak)
# library(patchwork)
# library(RColorBrewer)

# library(readxl)


# local locations ---------------------------------------------------------------

j90dir <- r"(E:\data\CA_j90/)"


# for each section, combine all years ----------------------

get_yearsec <- function(year, section) {
  print(year)
  print(section)
  indir <- paste0(j90dir, "data/", year, "/")
  fname <- paste0("tsal", section, str_sub(year, 3, 4), ".rds")
  fpath <- paste0(indir, fname)
  df <- readRDS(fpath) %>%
    mutate(year=!!year)
  df
}


savesec <- function(years, section){
  outdir <- paste0(j90dir, "data/allyears/")
  outfname <- paste0("tsal_sec", section, ".rds")
  outpath <- paste0(outdir, outfname)
  print(outpath)
  df <- purrr::map_dfr(years, get_yearsec, section)
  saveRDS(df, file=outpath)
}


for(section in 1:6) savesec(2016:2020, section)


# check to make sure files appear ok ----
for(section in 1:6) {
  ycount <- count(readRDS(paste0(j90dir, "data/allyears/", "tsal_sec", section, ".rds")), year)
  print(section)
  print(ycount)
}
count(readRDS(paste0(j90dir, "data/allyears/", "tsal_sec1.rds")), year)


# make merged opeb-focused analytic file -----------------------------------------------
#.. get all the data ----
ddir <- paste0(j90dir, "data/allyears/")
sec1 <- readRDS(paste0(ddir, "tsal_sec1.rds"))
sec2 <- readRDS(paste0(ddir, "tsal_sec2.rds"))
sec3 <- readRDS(paste0(ddir, "tsal_sec3.rds"))
sec4 <- readRDS(paste0(ddir, "tsal_sec4.rds"))
sec5 <- readRDS(paste0(ddir, "tsal_sec5.rds"))
sec6 <- readRDS(paste0(ddir, "tsal_sec6.rds"))

#.. create an id file that we can merge against others ----
glimpse(sec1)
# County	C 2	J-90	Two digit county code
# District	C 5	J-90	Five digit district Code
# CDS	C 7	J-90	Seven digit combined code - Note this is the code used as a key for all records in system
# TS1_Dname	C 34	Master	District Name (Long name)
# TS1_County	C 18	Master	County Name
# TS1_Type	C 1	Master	0=COE, 1=Elementary, 2=High School, 4=Unified, 3=Common Admin District
# TS1_ADA	N 7	Master	The current P-2 ADA
ids <- sec1 %>%
  select(year, ccode=county, dcode=district, dname=ts1_dname, county=ts1_county,
         type=ts1_type, ada=ts1_ada) %>%
  mutate(typef=factor(type, levels=0:4,
                      labels=c("COE", "Elementary", "High School", "Common Admin District", "Unified"))) %>%
  select(-ada, everything(), ada)
glimpse(ids)
count(ids, year, type, typef) %>%
  arrange(type, typef, year)
saveRDS(ids, paste0(ddir, "ids.rds"))

#.. get the health information from sec1 ----
# TS1_Note	C 80	J-90	The name of the agency or trust through which the district purchases the health plans
# TS1_Conf	C 1	Entered	After the district returns the confirmation for to SSC, this field contains a “Y”
# TS1_Maxcaf	N 8.2	J-90	The maximum annual employer contributions to cafeteria plan
# TS1_Maxsin	N 8.2	J-90	The maximum annual employer contributions to single plan per employee.
# TS1_Maxtwo	N 8.2	J-90	The maximum annual employer contributions to a two party plan per employee.
# TS1_Maxthree	N 8.2	J-90	The maximum annual employer contributions to a three party plan per employee
# TS1_Maxfam	N 8.2	J-90	The maximum annual employer contributions to a family plan per employee.
# TS1_CAPTYPE	C 2	J-90	Is the cap hard or soft or NA



glimpse(df1)
df2 <- df1 %>%
  rename(ccode=county, dcode=district) %>%
  setNames(str_replace(names(.), "ts1_", "")) %>%
  select(ccode, dcode, county, dname, type, ada, totfte, note,
         maxcaf, maxsin, maxtwo, maxfam, captype,
         ben_life, ben_stop, h_plan, d_plan, v_plan, o_plan) %>%
  mutate(dname=str_to_title(dname))
glimpse(df2)
ht(df2)

# type 0=COE, 1=Elementary, 2=High School, 4=Unified, 3=Common Admin District
ids <- df2 %>%
  select(ccode:totfte, ben_life, ben_stop) %>%
  mutate(year=2017,
         typef=factor(type, levels=0:4,
                      labels=c("COE", "Elementary", "High School", "Common Admin District", "Unified")))

count(ids, type, typef)

ids %>%
  count(type, typef, ben_life) %>%
  pivot_wider(names_from = ben_life, values_from = n, values_fill = 0) %>%
  mutate(tot=N + Y,
         lifeshare=Y / tot)

ids %>%
  filter(ben_life=="Y") %>%
  select(ccode, dcode, county, dname, typef, ada, totfte, ben_life, ben_stop) %>%
  arrange(desc(ada))

ids %>%
  mutate(adacut=cut(ada, c(-Inf, 0, 5e3, 10e3, 25e3, 50e3, 100e3, Inf))) %>%
  group_by(adacut, ben_life) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = ben_life, values_from = n) %>%
  mutate(tot=N + Y,
         lifeshare=Y / tot)



df2 %>%
  filter(ccode==38, dcode==68478)


df2 %>%
  arrange(desc(maxfam))
ns(df2)

# County Two digit county code
# District Five digit district code
# CDS Seven digit combined code
# TS6_BenA one character code indicating type of benefit.  H for heath, D for dental, V for vision, L for life, O for other
# TS6_Desc The name of the plan
# TS6_Step The health benefit plans ID.  The first is always 1, the second and subsequent like plans are number consequently. This is calculated by the input program
# TS6_Column A column 1 is always Single, 2 is two party plan, 3 is family plan, 4 is composite rate
# TS6_Annual The annual cost of plan
# TS6_Contr The amount the district contributes to the cost of the plan
# TS6_FTE The number of retired teachers (age 65 or under) participating in the plan
# TS6_ID number that is generated when a J-90 is submitted. Each record has an ID number

age65p1 <- sqlFetch(con, "tsal517") %>%
  setNames(str_trim(str_to_lower(str_trim(names(.))))) %>%
  as_tibble

agelt651 <- sqlFetch(con, "tsal617") %>%
  setNames(str_trim(str_to_lower(str_trim(names(.))))) %>%
  as_tibble

stack1 <- bind_rows(age65p1 %>% 
                      setNames(str_remove(names(.), "ts5_")) %>%
                      mutate(group="age65p"),
                    agelt651 %>% 
                      setNames(str_remove(names(.), "ts6_")) %>%
                      mutate(group="ltage65")) %>%
  select(-id, -cds) %>%
  rename(ccode=county, dcode=district, anncost=annual, ercamount=contr) %>%
  mutate(column=factor(column, 
                       levels=1:4, 
                       labels=c("single", "twoparty", "family", "composite")),
         ben=factor(ben,
                    levels=c("h", "d", "v", "l", "o"),
                    labels=c("health", "dental", "vision", "life", "other")),
         ercshare=ercamount / anncost) %>%
  right_join(ids, by = c("ccode", "dcode")) %>%
  select(all_of(names(ids)), everything())
glimpse(stack1)


cost1 <- stack1 %>%
  group_by(year, ccode, dcode, county, dname, type, typef, group, ben) %>%
  mutate(totcost=anncost * fte,
         erc=ercamount * fte) %>%
  summarise(ada=first(ada),
            totfte=first(totfte),
            ben_life=first(ben_life),
            ben_stop=first(ben_stop),
            totcost=sum(totcost, na.rm=TRUE),
            erc=sum(erc, na.rm=TRUE), 
            .groups="drop") %>%
  mutate(ercshare=erc / totcost)

cost2 <- cost1 %>%
  group_by(year, ccode, dcode, county, dname, type, typef, group) %>%
  summarise(ada=first(ada),
            totcost=sum(totcost, na.rm=TRUE), 
            erc=sum(erc, na.rm=TRUE), .groups="drop") %>%
  mutate(ercshare=erc / totcost)

# get age65p as share of total cost, get cost per ada  
cost3 <- cost2 %>%
  select(-ercshare, -totcost, -type, -year) %>%
  filter(!is.na(group)) %>%
  pivot_wider(names_from = group,
              values_from = erc,
              values_fill = 0) %>%
  mutate(ercost=ltage65 + age65p,
         share65p=age65p / ercost,
         costada=ercost / ada)

cost3 %>%
  arrange(desc(ercost))

cost3 %>%
  arrange(desc(share65p))

cost3 %>%
  filter(ada > 0) %>%
  filter(ada >= 5e3) %>%
  arrange(desc(costada))


sfusd <- df6b %>%
  filter(ccode==38, dcode==68478)


