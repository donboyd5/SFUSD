

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


# locations ---------------------------------------------------------------
dir <- r"(C:\Users\donbo\Downloads\SFUSD/)"
fn <- "OPEB Study Data 2021-02-18.xlsx"



# constants ---------------------------------------------------------------



# get data ----------------------------------------------------------------
df1 <- read_excel(paste0(dir, fn), sheet="OPEB Study Data")
glimpse(df1)
summary(df1)

df2 <- df1 %>%
  select(-...10) %>%
  rename(stabbr=state,
         penrev=`pension/revenue`,
         opebrev=`opeb/revenue`)
summary(df2)

df3 <- df2 %>%
  filter(str_detect(name, coll("school", ignore_case = TRUE)))

check <- df3 %>%
  filter(!str_detect(name, coll("school district", ignore_case = TRUE)))

count(df3, stabbr)

df3 %>%
  filter(stabbr=="CA") %>%
  arrange(-total_revenues)

df3 %>%
  filter(stabbr=="CA", total_revenues >= 100e6) %>%
  arrange(-opebrev)

df3 %>%
  filter(total_revenues >= 250e6)

