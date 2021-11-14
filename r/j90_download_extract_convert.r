

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

# download, extract, and convert the CDE (California Department of Education) J-90 data for multiple years
# Report of Teacher Salary and Benefit Data (Form J-90)

# NOTE: You must use 

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

j90dir <- r"(E:\data\CA_j90/)"





# ONETIME: j90 download ------------------------------------------------------------
urlbase <- "https://www3.cde.ca.gov/ds-downloads/fd/cs/"
# j901920.exe

urls <- paste0(urlbase, "j90", )
for(year in 2004:2020){
  fname_years <- paste0(str_sub(year - 1, 3, 4), str_sub(year, 3, 4))
  fname <- paste0("j90", fname_years, ".exe")
  url <- paste0(urlbase, fname)
  fpath <- paste0(j90dir, fname)
  print(fpath)
  download.file(url, fpath, mode="wb")
}


# ONETIME: j90 extract -------------------------------------------------------------
for(year in 2004:2020){
  fname_years <- paste0(str_sub(year - 1, 3, 4), str_sub(year, 3, 4))
  fname <- paste0("j90", fname_years, ".exe")
  fpath <- paste0(j90dir, fname)
  args <- c("/auto", paste0(j90dir, "data/", year, "/"))
  print(fpath)
  system2(command=fpath, args=args, invisible = TRUE)  # will create year folder if does not exist
}


# j90 convert -------------------------------------------------------------
# data is in text files from 2004 through 2015
# then in Access databases from 2016-2020
# to get the Access data, this R program needs to be using 32-bit R, NOT 64-bit R

# get the access data -- sometimes accdb, sometimes mdb
# j901516.accdb
# j901617.mdb
# j901718.accdb
# j901819.accdb
# j901920.accdb


for(year in 2016:2020){
  fname_years <- paste0(str_sub(year - 1, 3, 4), str_sub(year, 3, 4))
  fname_base <- paste0("j90", fname_years)
  fname_ext <- ifelse(year==2017, ".mdb", ".accdb")
  fname <- paste0(fname_base, fname_ext)
  fpath <- paste0(j90dir, "data/", year, "/", fname)
  print(fpath)
  
  if(fname_ext == ".mdb") con <- odbcConnectAccess(fpath) else con <- odbcConnectAccess2007(fpath)
  tables <- sqlTables(con)
  tabnames <- tables %>%
    filter(TABLE_TYPE=="TABLE") %>%
    .$TABLE_NAME
  
  for(tab in tabnames){
    print(tab)
    df <- sqlFetch(con, tab) %>%
      setNames(str_trim(str_to_lower(names(.))))
    
    rdspath <- paste0(j90dir, "data/", year, "/", str_to_lower(tab), ".rds")
    saveRDS(df, file=rdspath)  # 
  }
  
  odbcClose(con)
}


# j90 check ---------------------------------------------------------------
# make sure every rds file is readable
for(year in 2016:2020){
  print(year)
  
  folder <- paste0(j90dir, "data/", year, "/")
  flist <- list.files(folder, pattern=".rds", full.names = TRUE)
  
  for(file in flist){
    print(file)
    df <- readRDS(file)
    print(names(df))
    print(nrow(df))
  }
}


# If all good, close R and reopen as 64-bit R to work with the data



# old ---------------------------------------------------------------------


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









# Report of Teacher Salary and Benefit Data (Form J-90) -------------------
j90dir <- r"(C:\Users\donbo\Downloads\SFUSD\j901617/)"
j90fn <- "j901617.mdb"
