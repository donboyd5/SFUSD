# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(arrow)
library(kableExtra)
library(btools)
library(gt)
library(knitr)

library(maps)
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
library(usmap)
library(gridExtra)
library(RcppRoll)
library(ggrepel)
library(ggbreak)
library(patchwork)
library(RColorBrewer)

library(readxl)

library(RODBC)

# remotes::install_github("kiernann/mdbr")
# library(mdbr)
# ex <- mdb_example()
# mdb_tables(ex)
# mdb_tables(ex <- mdb_example())


# locations ---------------------------------------------------------------

dir <- r"(C:\Users\donbo\Downloads\SFUSD\1920SACS\Data/)"
fn <- "sacs1920.mdb"



# ms access notes ---------------------------------------------------------
# https://www.roelpeters.be/solved-importing-microsoft-access-files-accdb-mdb-in-r/
# If your tables aren’t huge (+4GB), you can simply change R to the 32-bit version.

# https://newbedev.com/how-to-read-data-from-microsoft-access-accdb-database-files-into-r
# You may need to run the 32-bit c:\windows\sysWOW64\odbcad32.exe if running 64-bit Windows



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



# Report of Teacher Salary and Benefit Data (Form J-90) -------------------
j90dir <- r"(C:\Users\donbo\Downloads\SFUSD\j901617/)"
j90fn <- "j901617.mdb"


con <- odbcConnectAccess(paste0(j90dir, j90fn))
tables <- sqlTables(con)
tables
keeptables <- tables %>%
  filter(TABLE_TYPE=="TABLE")
keeptables
keeptables$TABLE_NAME

df1 <- sqlFetch(con, "tsal117") %>%
  setNames(str_trim(str_to_lower(str_trim(names(.))))) %>%
  as_tibble
str_subset(names(df1), "county")

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

# how can we have more ftes enrolled in a plan than ftes in the district?  


for(tab in gettabs){
  print(tab)
  df <- sqlFetch(con, tab) %>%
    setNames(str_trim(str_to_lower(names(.))))
  assign(str_to_lower(tab), df)
}
odbcClose(con)




# get data ----------------------------------------------------------------
# https://www.cde.ca.gov/ds/fd/cs/
tables <- c("Object")

con <- odbcConnectAccess(paste0(dir, fn))
tables <- sqlTables(con)
tables
keeptables <- tables %>%
  filter(TABLE_TYPE=="TABLE")
keeptables
keeptables$TABLE_NAME

keeptables$TABLE_NAME[1]
gettabs <- setdiff(keeptables$TABLE_NAME, c("Goal"))

for(tab in gettabs){
  print(tab)
  df <- sqlFetch(con, tab) %>%
    setNames(str_trim(str_to_lower(names(.))))
  assign(str_to_lower(tab), df)
}
odbcClose(con)


# examine data ------------------------------------------------------------
glimpse(leas)
leas %>%
#   filter(str_detect(dname, coll("San Fran", ignore_case = TRUE)))
# ccode dcode                                                                       dname           dtype   k12ada
# 1    38 10389 San Francisco County Office of Education                                    CO OFFICE           0.00
# 2    38 68478 San Francisco Unified                                                       UNIFIED         50096.27
# 3    41 69070 South San Francisco Unified                                                 UNIFIED          7975.79

glimpse(object)
object %>%
  filter(str_detect(title, "OPEB")) %>%
  mutate(title=str_trim(title))
# code                                          title
# 1 3701        OPEB, Allocated, certificated positions
# 2 3702          OPEB, Allocated, classified positions
# 3 3751 OPEB, Active Employees, certificated positions
# 4 3752   OPEB, Active Employees, classified positions
# 5 9664                       Total/Net OPEB Liability
object <- object %>%
  mutate(title=str_trim(title))

opeb_codes <- object %>%
  filter(str_detect(title, "OPEB")) %>%
  .$code
opeb_codes

glimpse(usergl)
usergl <- usergl %>%
  rename(func=`function`) # function apparently is a reserved word

glcodes <- count(usergl, object)  # only 30 for 9664
usergl %>%
  filter(object=="9664") %>%
  arrange(desc(value))
# 168,255,392.1 for 30 66522 
leas %>%
  filter(ccode==30, dcode==66522)
  
usergl %>%
  filter(ccode==38, dcode==68478) %>%
  filter(object %in% opeb_codes) %>%
  arrange(desc(value))

usergl %>%
  filter(ccode==38, dcode==68478) %>%
  filter(object == "9664")

usergl %>%
  filter(ccode==38, dcode==68478) %>%
  filter(object == "3751") %>%
  arrange(desc(value))

count(usergl_totals %>% filter(object %in% opeb_codes), object)


tmp <- usergl_totals %>%
  filter(ccode==38) %>%
  arrange(object)
