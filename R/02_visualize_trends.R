###### 0) Libraries and the like
library(dplyr)
library(purrr)
library(tidyr)
library(raster)
library(lubridate)
library(meowR)
library(readr)
library(colorspace)


###### 1) Get the regions
data("regions")
data("provinces")
data("realms")


###### 2) Load the unique regions, and get annual values
unique_region_temp <- read_csv("../derived_data/hadsst_regions.csv")
region_temp_annual <- unique_region_temp %>%
  group_by(Year, ECOREGION) %>%
  dplyr::summarize(mean_tempC = mean(tempC, na.rm=T), 
            max_tempC = max(tempC, na.rm=T), 
            min_tempC = min(tempC, na.rm=T)) %>%
  ungroup()

###### 3) Plot the trends in temperature
p <- region_temp_annual %>%
  gather(type, tempC, -Year, -ECOREGION) %>%
  group_by(ECOREGION, type) %>%
  do(plots = ggplot(data = .) + 
    aes(x=Year, y=tempC) + 
    geom_line() + stat_smooth(method="lm") +
      ggtitle(.$ECOREGION[1])
  ) %>%
  spread(type, plots)

pdf("../figures/ecoregions_mean_temp_trends.pdf")
p$mean_tempC
dev.off()


pdf("../figures/ecoregions_max_temp_trends.pdf")
p$max_tempC
dev.off()


pdf("../figures/ecoregions_min_temp_trends.pdf")
p$min_tempC
dev.off()

###### 4) get linear trends for each type of temp summary
region_temp_annual_trends <- region_temp_annual %>%
  gather(type, value, -Year, -ECOREGION) %>%
  group_by(ECOREGION, type) %>%
  nest() %>%
  mutate(mod = purrr::map(data, ~broom::tidy(lm(value ~ Year, data=.)))) %>%
  unnest(mod) %>%
  ungroup() %>%
  filter(term != "(Intercept)") %>%
  dplyr::select(-p.value, -statistic, -term) %>%
  gather(output, value, estimate, std.error) %>%
  unite(info, type, output, sep="_") %>%
  mutate(value = 10*value) %>% #for decadal
  spread(info, value)
  
###### 5) Plot trends on a map
plot_trend_map <- function(avar){
  makeMEOWmap(region_temp_annual_trends,avar,
              fillPal=diverge_hsv(20, power=0.4),
              #fillPal = rev(brewer.pal(6, "RdYlBu")),
              guide=guide_colorbar(title="Estimated\nDecadal Slope"),
              add.worldmap=T, limits=c(-0.5, 0.5))  +
    theme_void(base_size=17)
}  

pdf("../figures/ecoregion_trends_map.pdf")
plot_trend_map("mean_tempC_estimate") + 
  ggtitle("Slope of Decadal Change in Mean Temperature\n1950-2014\n")
plot_trend_map("max_tempC_estimate") + 
  ggtitle("Slope of Decadal Change in Max Temperature\n1950-2014\n")
plot_trend_map("min_tempC_estimate") + 
  ggtitle("Slope of Decadal Change in Min Temperature\n1950-2014\n")
dev.off()

write.csv(unique_province_temp, "../derived_data/hadsst_provinces.csv", row.names=F)
write.csv(unique_realm_temp, "../derived_data/hadsst_realms.csv", row.names=F)

