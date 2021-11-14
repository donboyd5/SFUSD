
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


# logical expressions -----------------------------------------------------

sfusd <- expression(ccode==38 & dcode==68478)


# for each file number, combine all years ----------------------

get_yearfile <- function(year, file) {
  print(year)
  print(file)
  indir <- paste0(j90dir, "data/", year, "/")
  fname <- paste0("tsal", file, str_sub(year, 3, 4), ".rds")
  fpath <- paste0(indir, fname)
  df <- readRDS(fpath) %>%
    mutate(year=!!year) %>%
    as_tibble()
  df
}


savefile <- function(years, file){
  outdir <- paste0(j90dir, "data/allyears/")
  outfname <- paste0("tsal_file", file, ".rds")
  outpath <- paste0(outdir, outfname)
  print(outpath)
  df <- purrr::map_dfr(years, get_yearfile, file)
  saveRDS(df, file=outpath)
}


for(file in 1:6) savefile(2016:2020, file)


# check to make sure files appear ok ----
for(file in 1:6) {
  ycount <- count(readRDS(paste0(j90dir, "data/allyears/", "tsal_file", file, ".rds")), year)
  print(file)
  print(ycount)
}
count(readRDS(paste0(j90dir, "data/allyears/", "tsal_file1.rds")), year)


# make merged opeb-focused analytic file -----------------------------------------------
#.. get all the data ----
ddir <- paste0(j90dir, "data/allyears/")
file1 <- readRDS(paste0(ddir, "tsal_file1.rds"))  # 1 rec per district per year
file2 <- readRDS(paste0(ddir, "tsal_file2.rds"))
file3 <- readRDS(paste0(ddir, "tsal_file3.rds"))
file4 <- readRDS(paste0(ddir, "tsal_file4.rds"))
file5 <- readRDS(paste0(ddir, "tsal_file5.rds"))
file6 <- readRDS(paste0(ddir, "tsal_file6.rds"))

#   setNames(str_replace(names(.), "ts1_", "")) # drop prefixes?

#.. create an id file that we can merge against others ----
glimpse(file1)
# County	C 2	J-90	Two digit county code
# District	C 5	J-90	Five digit district Code
# CDS	C 7	J-90	Seven digit combined code - Note this is the code used as a key for all records in system
# TS1_Dname	C 34	Master	District Name (Long name)
# TS1_County	C 18	Master	County Name
# TS1_Type	C 1	Master	0=COE, 1=Elementary, 2=High School, 4=Unified, 3=Common Admin District
# TS1_ADA	N 7	Master	The current P-2 ADA

# TS1_totfte	N 10.2	J-90	The number of full time teachers on salary schedule.
ids <- file1 %>%
  select(year, ccode=county, dcode=district, dname=ts1_dname, county=ts1_county,
         dtype=ts1_type, ada=ts1_ada, teachfte=ts1_totfte,
         # opeb-related items -- documentation given a bit further below
         ben_life, ben_stop,
         retire, uretire, liability, studydate) %>%
  mutate(sfusd=ccode==38 & dcode==68478,  # create an indicator so we don't have to remember codes
         dtypef=factor(dtype, levels=0:4,
                      labels=c("COE", "Elementary", "High School", "Common Admin District", "Unified")),
         dname=str_to_title(dname)) %>%
  select(year, ccode, dcode, dname, county, dtype, dtypef, everything())

glimpse(ids)
count(ids, year, dtype, dtypef) %>%
  arrange(dtype, dtypef, year)
ids %>% filter(sfusd)

saveRDS(ids, paste0(ddir, "ids.rds"))


#.. get the health information from file1 MAY NOT USE THIS ----
# TS1_Note	C 80	J-90	The name of the agency or trust through which the district purchases the health plans
# TS1_Conf	C 1	Entered	After the district returns the confirmation for to SSC, this field contains a “Y”
# TS1_Maxcaf	N 8.2	J-90	The maximum annual employer contributions to cafeteria plan
# TS1_Maxsin	N 8.2	J-90	The maximum annual employer contributions to single plan per employee.
# TS1_Maxtwo	N 8.2	J-90	The maximum annual employer contributions to a two party plan per employee.
# TS1_Maxthree	N 8.2	J-90	The maximum annual employer contributions to a three party plan per employee
# TS1_Maxfam	N 8.2	J-90	The maximum annual employer contributions to a family plan per employee.
# TS1_CAPTYPE	C 2	J-90	Is the cap hard or soft or NA

# The following appear to be opeb related items although they are in the section for current actives
# Ben_life	C 1	J-90	Does district cover retirees for life? (Y/N)
# Ben_stop	N 3	J-90	If not for life, when do benefits stop? Age or years

# RETIRE	N 1	J-90	Is benefit plan provided for retired employees over 65
# URETIRE	N 1	J-90	Is benefit plan provided for retired employees under 65

# LIABILITY	N 13	J-90	Unfunded liability amount reported in actuarial study
# STUDYDATE	C 10	J-90	Latest actuarial study date

health <-  ids %>%
  left_join(file1 %>%
              select(year, ccode=county, dcode=district,
                     ts1_note:ts1_captype), 
            by = c("year", "ccode", "dcode"))
glimpse(health)

health %>% filter(sfusd)
health %>% filter(str_detect(dname, coll("oakland", ignore_case = TRUE)))

#.. get the opeb data from files 5 and 6 ----
# file 5 and 6 have the same basic format, but
#   file 5 is for retirees age 66+ and 
#   file 6 is for retirees age 65 and under

# each district can have multiple records in a year if they have multiple kinds of plans (health, dental, ...)
# and/or multiple plans for a given kind of plan

# here are the fields in file 6; file 5 is the same, except that fte is for age 65+
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

# get each section, create uniform variable names, and stack them
age66plus <- file5 %>%
  setNames(str_replace(names(.), "ts5_", "")) %>%
  mutate(source="file5", agecat="age66p")

under66 <- file6 %>%
  setNames(str_replace(names(.), "ts6_", "")) %>%
  mutate(source="file6", agecat="under66")

all.equal(names(age66plus), names(under66))


opeb1 <- bind_rows(age66plus, under66) %>%
  mutate(agecat=factor(agecat, 
                       levels=c("under66", "age66p")), # so they will sort as I want
         column=factor(column, 
                       levels=1:4, 
                       labels=c("single", "twoparty", "family", "composite")),
         ben=factor(ben,
                    levels=c("h", "d", "v", "l", "o"),
                    labels=c("health", "dental", "vision", "life", "other"))) %>%
  rename(ccode=county, dcode=district, opebfte=fte, plancost=annual, planercost=contr,
         plantype=ben, provider=desc, planseq=step, coverage=column) %>%
  mutate(planeecost=plancost - planercost,
         totcost=opebfte * plancost,
         totercost=opebfte * planercost,
         toteecost=opebfte * planeecost) %>%
  select(year, ccode, dcode, agecat, plantype, provider, planseq, coverage, opebfte, 
         plancost, planercost, planeecost,
         totcost, totercost, toteecost,
         source) %>%
  arrange(agecat, year, ccode, dcode, plantype, planseq, coverage)
glimpse(opeb1)

opeb <- ids %>%
  left_join(opeb1, by = c("year", "ccode", "dcode"))

saveRDS(opeb, paste0(ddir, "opeb.rds"))


# checks ------------------------------------------------------------------
ids <- readRDS(paste0(ddir, "ids.rds"))
glimpse(ids)

opeb <- readRDS(paste0(ddir, "opeb.rds"))
glimpse(opeb)
opeb 
count(opeb, provider) %>% arrange(desc(n))

tmp <- opeb %>% filter(sfusd)

tmp %>%
  filter(plantype=="health") %>%
  filter(year==2020) %>%
  mutate(erpct=totercost / sum(totercost),
         lbl=ifelse(erpct > .05, paste0(round(erpct, 2), "-", coverage), "")) %>%
  ggplot(aes(x=planeecost, y=opebfte, size=erpct, label=lbl)) +
  geom_point() +
  geom_text(nudge_x = 500, size=3)

tmp2 <- tmp %>%
  filter(plantype=="health") %>%
  filter(year==2020) %>%
  mutate(erpct=totercost / sum(totercost)) %>%
  arrange(-erpct)

tmp2 %>%
  select(dname, plantype, agecat, provider, coverage, opebfte, 
         plancost, eeplancost, erplancost, totcost, totercost, toteecost, erpct) %>%
  mutate(cumerpct=cumsum(erpct))
  


# total under66 and 66plus opeb costs by district by year
costs <- opeb %>%
  mutate(opebcost=naz(opebfte) * naz(totcost),
         eropebcost=naz(opebfte) * naz(ercost)) %>%
  group_by(year, ccode, dcode, county, dname, agecat) %>%
  summarize(across(c(opebfte, opebcost, eropebcost), ~ sum(.x, na.rm=TRUE)),
            ada=first(ada),
            sfusd=first(sfusd), .groups="drop")
summary(costs)

df <- costs %>%
  select(year, ccode, dcode, county, dname, agecat, ada, opebfte, eropebcost) %>%
  pivot_wider(names_from = agecat, values_from = c(opebfte, eropebcost), values_fill = 0) %>%
  select(-contains("NA")) %>%
  mutate(opebfte=opebfte_under66 + opebfte_age66p,
         totopeb=eropebcost_under66 + eropebcost_age66p,
         costada=totopeb / ada,
         age66pshare=eropebcost_age66p / totopeb,
         costfte=totopeb / opebfte,
         fteada=opebfte / ada
         )

df2 <- df %>%
  select(-contains("_"))

df2 %>% filter(year==2020) %>% arrange(-costada)
df2 %>% filter(eval(sfusd))

df %>% filter(eval(sfusd))



costs %>%
  pivot_wider(names_from = agecat, values_from = c(opebfte, opebcost, eropebcost, values_fill=0))



# OLD ----
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


