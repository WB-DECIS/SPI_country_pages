library(tidyverse)
library(here)
library(highcharter)
library(wbstats)

yr <- 2019

#load data
SPI <- read_csv(here("data","SPI_index.csv")) 
country_info <- wbstats::wb_countries()
metadata_raw <- read_csv(here("data","SPI_dimensions_sources.csv"))


metadata <- metadata_raw %>%
  mutate(descript=paste(SPI_indicator_id,": ", spi_indicator_name," - ",source_name , sep="")) %>%
  select(source_id,  descript, spi_indicator_description,spi_indicator_scoring)

metadata <- read_csv(here("data","SPI_index_sources.csv")) %>%
  bind_rows(metadata)

metadataind <- metadata %>%
  filter(str_sub(source_id, 1, 9) %in% "SPI.INDEX") %>%
  select(source_id, descript) %>%
  rename("shortname2" = "descript")

metadata_raw <- metadata_raw %>%
  mutate(shortname = ifelse(str_sub(source_id, 1, 6) %in% "SPI.D3",
                            gsub(":.*","", source_name), source_name)) %>%
  mutate(shortname = ifelse(str_sub(source_id, 1, 6) %in% "SPI.D3",
                            gsub("GOAL","SDG", shortname), shortname),
         shortname = case_when(
           source_id == "SPI.D1.5.POV" ~"Availability of Comparable Poverty Data", 
           source_id == "SPI.D1.5.CHLD.MORT" ~"Availability of Mortality rate, under-5", 
           source_id == "SPI.D1.5.DT.TDS.DPPF.XP.ZS	" ~"Quality of Debt Reporting", 
           source_id == "SPI.D1.5.SAFE.MAN.WATER" ~"Availability of Safely Managed Water Data", 
           source_id == "SPI.D1.5.LFP	" ~"Availability of Labor Force Participation Data", 
           
           source_id == "SPI.D5.1.DILG"~ "Legislation Indicator based on SDG 17.18.2", 
           source_id == "SPI.D5.1.DILG"~ "Legislation Indicator based on SDG 17.18.2", 
           source_id == "SPI.D5.5.DIFI"~ "Statistical Plan fully funded", 
           source_id == "SPI.D5.2.5.HOUS"~ "Household consumption classification", 
           source_id == "SPI.D5.2.9.MONY"~ "Monetary & financial statistics compilation", 
           source_id == "SPI.D5.2.3.CNIN"~ "National industry classification", 
           source_id == "SPI.D5.2.6.EMPL"~ "Status of employment classification", 
           source_id == "SPI.D5.2.8.FINA"~ "Govt. finance statistics compilation", 
           TRUE ~ shortname),
         shortname= if_else(is.na(shortname), "N/A", shortname))

#######################
# Country Reports
#######################


lolli_df <- function(country_choice, year_choice) {
  df_lolli <- SPI %>%
    select(iso3c, country, region,  income, date, starts_with('SPI'), population) %>%
    filter(date==year_choice) %>%
    mutate(ISO_A3_EH=iso3c) 
  
  #need to get the aggregates for the income group and region
  country_select_info <- country_info %>%
    filter(country==country_choice)
  
  region_name<-country_select_info$region
  income_name<-country_select_info$income_level
  
  #produce aggregation
  region_agg <- df_lolli %>%
    filter(region==region_name) %>%
    summarise(across(starts_with('SPI'),~round(mean(as.numeric(.), na.rm=T),2))) %>%
    mutate(country=region_name)
  
  income_agg <- df_lolli %>%
    filter(income==income_name) %>%
    summarise(across(starts_with('SPI'),~round(mean(as.numeric(.), na.rm=T),2))) %>%
    mutate(country=income_name)
  
  #lollipop chart
      df_lolli %>%
      filter(country==country_choice) %>%
      bind_rows(region_agg) %>%
      bind_rows(income_agg) 
} 

#################
# Functions for Country Report
#################

country_report_lolli_fn <- function(variables, title, scale, country, year) {
  lollip_df_temp <- lolli_df(country, year) %>%
    select(country, starts_with('SPI')) %>%
    relocate(SPI.D3.13.CLMT, .after = SPI.D3.12.CNSP) %>%
    pivot_longer(
      cols = starts_with('SPI'),
      names_to = 'source_id',
      values_to = 'values'
    ) %>% 
    left_join(metadata_raw) %>%
    left_join(metadataind) %>%
    mutate(shortname = ifelse(!is.na(shortname2), shortname2, shortname),
           shortname2 = NULL) %>%
    filter(!is.na(shortname)) %>%
    mutate(source_name = ifelse(is.na(source_name), shortname, source_name),
           shortname=factor(shortname, ordered=TRUE),
           country=factor(country, levels=unique(country))) %>%
    mutate(indi2 = trimws(str_remove(SPI_indicator_id, "Dimension"))) %>%
    mutate(dimension = substr(indi2, 1, 1)) %>%
    mutate(dimname = paste("Pillar ", dimension),
           dimname = case_when(
             dimname=="Pillar  1" ~ "Pillar 1: Data Use",
             dimname=="Pillar  2" ~ "Pillar 2: Data Services",
             dimname=="Pillar  3" ~ "Pillar 3: Data Products",
             dimname=="Pillar  4" ~ "Pillar 4: Data Sources",
             dimname=="Pillar  5" ~ "Pillar 5: Data Infrastructure",
             TRUE ~ "Overall Index"
           ),
           shortname=ifelse(dimname == "Pillar  1: Data Use", str_wrap(shortname, 25), str_wrap(shortname, 12))
    ) %>%
    mutate(labtext = paste(paste(country),
                           paste(source_name),
                           paste("Score: ", values),
                           sep = "<br />"))
  
  #order the categories
  lollip_df_temp <- lollip_df_temp %>%
    mutate(shortname=factor(shortname, levels = unique(lollip_df_temp$shortname))) %>%
    filter(grepl(variables,source_id)) %>%
    select(shortname, values, dimname, country, source_name ) %>%
    mutate(values=round(values,2))
  
  
  highchart() %>%
    hc_add_series(data = lollip_df_temp,
                  hcaes(x=shortname, y = values, group = country),
                  type = "column") %>%
    hc_xAxis(type='category') %>%
    hc_yAxis(min=0,max=scale) %>%
    hc_title(text=title)
}

country_plots <-  function(cntry) {
  

  
  p0 <- country_report_lolli_fn('SPI.INDEX','Overall Index', 100, cntry, yr) %>% 
    htmlwidgets::saveWidget(file = here("country_reports","reports","charts/",paste0(cntry, "_chart.html")), selfcontained = TRUE)
  #   
  # p1 <- country_report_lolli_fn('SPI.D1', "Pillar 1: Data Use",1, cntry, yr) %>% 
  # htmlwidgets::saveWidget(file = here("country_reports","reports","charts/",paste0(cntry, "_chart1.html")), selfcontained = TRUE)
  # p2 <- country_report_lolli_fn('SPI.D2', "Pillar 2: Data Services",1, cntry, yr) %>% 
  #   htmlwidgets::saveWidget(file = here("country_reports","reports","charts/",paste0(cntry, "_chart2.html")), selfcontained = TRUE)
  # p3 <- country_report_lolli_fn('SPI.D3', "Pillar 3: Data Products",1, cntry, yr) %>% 
  #   htmlwidgets::saveWidget(file = here("country_reports","reports","charts/",paste0(cntry, "_chart3.html")), selfcontained = TRUE)
  # p4 <- country_report_lolli_fn('SPI.D4', "Pillar 4: Data Sources",1, cntry, yr) %>% 
  #   htmlwidgets::saveWidget(file = here("country_reports","reports","charts/",paste0(cntry, "_chart4.html")), selfcontained = TRUE)
  # p5 <- country_report_lolli_fn('SPI.D5', "Pillar 5: Data Infrastructure",1, cntry, yr) %>% 
  #   htmlwidgets::saveWidget(file = here("country_reports","reports","charts/",paste0(cntry, "_chart5.html")), selfcontained = TRUE)
  # 
  
}

countries_list <- country_info %>%
  filter(region!="Aggregates") %>%
  select(country) 

lapply(countries_list$country, country_plots)
