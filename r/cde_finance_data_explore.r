# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
# library(arrow)
# library(kableExtra)
# library(btools)
# library(gt)
# library(knitr)

# library(maps)
# # https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
# library(usmap)
# library(gridExtra)
# library(RcppRoll)
# library(ggrepel)
# library(ggbreak)
# library(patchwork)
# library(RColorBrewer)

# library(readxl)

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
