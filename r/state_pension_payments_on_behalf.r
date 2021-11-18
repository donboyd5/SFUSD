

# STRS On-Behalf Pension Contributions 8590
# object 8590 All Other State Revenue. Record all other state funds received.
# resource code 7690 STRS On-Behalf Pension Contributions

sacs_gfcurrent <- readRDS(file=paste0(dsacs_all, "sacs_gfcurrent.rds"))
all_sacs <- readRDS(paste0(dsacs_all, "all_sacs.rds"))

sf <- all_sacs %>%
  filter(eval(sfusd))

tmp <- sf %>%
  filter(resource=="7690", object=="8590", year==2019)
# fund 0001, 0012 Child Development Fund

sf %>%
  filter(resource=="7690", object=="8590", fund=="0001") %>%
  group_by(year) %>%
  summarise(statepay=sum(value))


all_sacs %>%
  filter(fund=="0001") %>%
  filter((object %in% c(3101, 3102)) | (resource=="7690" & object=="8590")) %>%
  mutate(erctrs=object %in% c(3101, 3102),
         behalf=(resource=="7690" & object=="8590")) %>%
  group_by(year) %>%
  summarise(erctrs=sum(value * erctrs),
            behalf=sum(value * behalf), 
            .groups="drop") %>%
  mutate(erctrs_xbehalf=erctrs - behalf) %>%
  pivot_longer(contains("erctrs")) %>%
  # mutate(name=factor(name, levels=c("erctrs", "erctrs_xbehalf"),
  #                    labels="Including "))
  ggplot(aes(year, value / 1e6, colour=name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name="Expenditures ($ millions)", breaks=seq(0, 10e3, 2e3), limits=c(0, NA), labels = comma) +
  scale_x_continuous(name=NULL) +
  ggtitle("California school district general fund payments to CalSTRS",
          subtitle="Including and excluding payments by the state on behalf of districts") +
  theme_bw() +
  legend_notitle 




