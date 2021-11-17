

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


# get data ----------------------------------------------------------------
dir <- r"(C:\Users\donbo\Downloads\SFUSD\demographics/)"
fn <- "calstrs-2020.csv"

df1 <- read_csv(paste0(dir, fn))

df2 <- df1 %>%
  filter(str_detect(employer, coll("SAN FRANCISCO UNIFIED SCHOOL DISTRICT", ignore_case = TRUE)))
count(df2, employer)

df2 <- df1 %>%
  filter(employer == "SAN FRANCISCO UNIFIED SCHOOL DISTRICT")
summary(df2)

fn2 <- "calpers-2020.csv"
df3 <- read_csv(paste0(dir, fn2))

df4 <- df3 %>%
  filter(str_detect(employer, coll("SAN FRAN", ignore_case = TRUE)))
count(df4, employer)

df4 <- df3 %>%
  filter(str_detect(employer, coll("SAN FRAN", ignore_case = TRUE)))
count(df4, employer)

fn3 <- "sfers-san-francisco-employees-retirement-system-2019.csv"
df5 <- read_csv(paste0(dir, fn3))
count(df5, job_title) %>% arrange(-n)


df2 %>%
  select(employer, pension_amount, years_of_service, year_of_retirement, year, pension_system) %>%
  head(5)

