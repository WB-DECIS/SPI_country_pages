library(tidyverse)
library(here)
library(highcharter)
library(wbstats)

yr <- 2022

#load data
SPI <- read_csv(here("data","SPI_index.csv")) %>%
  select(iso3c, country, region,  income, date, SPI.INDEX, everything())

#country_info <- wbstats::wb_countries()
#country_info %>%
#  write_excel_csv(here("data","country_info.csv"))

country_info <- read_csv(here("data","country_info.csv")) 


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
      mutate(group=country_choice) %>%
      bind_rows(region_agg %>% mutate(group="Region")) %>%
      bind_rows(income_agg %>% mutate(group="Income Group")) 
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
    hc_title(text=title,
      style = list(fontSize = "20px",
      fontWeight = "bold",
      fontFamily = "Andes"
    )
    ) %>%
  #make the colors hex code #023047, #ffb703, and #fb8500
  hc_colors(c("#023047", "#ffb703", "#fb8500")) %>%  
  hc_add_theme(hc_theme_smpl()) 


    
}




#turn previous code starting with agg_df into a function
country_report_beeswarm <- function(cntry, year) {

  agg_df <- lolli_df(cntry, year)

  #create a highcharter plot that is a beeswarm plot with the SPI data plotted for column SPI.INDEX.  The data is filtered so the year is 2022.  Each point is the value for SPI.INDEX
  df_beeswarm <- SPI %>% 
    filter(date==year) %>%
    filter(!is.na(SPI.INDEX)) 

  #get all data for 2022  
  df_beeswarm <- df_beeswarm %>%   
    mutate(
      density = density(SPI.INDEX, n=nrow(df_beeswarm))$y,
      y = jitter(rep(1, nrow(.)), factor = 200*density)
      ) %>%    
    mutate(group="Other Countries") %>%
    bind_rows(
        agg_df %>% mutate(y=1)
    ) %>%
    mutate(SPI.INDEX=round(SPI.INDEX,1))

    df_beeswarm <- df_beeswarm %>%
      mutate(country=factor(country, levels=unique(df_beeswarm$country)))


  #create a dataframe that is the data for the country selected
  df_country <- df_beeswarm %>%
    filter(country==cntry, group!="Other Countries")

  #create a dataframe that is the data for the region selected
  df_region <- df_beeswarm %>%
    filter(group=="Region")

  #create a dataframe that is the data for the income group selected
  df_income <- df_beeswarm %>%
    filter(group=="Income Group")

  # create a dataframe for the other countries
  df_other <- df_beeswarm %>%
    filter(group=="Other Countries", country!=cntry)

  hchart(df_other, "scatter", hcaes(x = SPI.INDEX, y=y, 
    name=group, group=group, showInLegend=TRUE),
    minSize=7, maxSize=20,
    marker=list(symbol='circle', radius=5, fillColor='#C8E2EE')
    ) %>%
    hc_add_series(data = df_country,
                  hcaes(x=SPI.INDEX, y = y, name=country, group=group, showInLegend=TRUE),
                  type = "scatter",
                  marker=list(symbol='diamond', radius=8, fillColor='#023047')
    ) %>%
    hc_add_series(data = df_region,
                  hcaes(x=SPI.INDEX, y = y,  name=country, group=country, showInLegend=TRUE),
                  type = "scatter",
                  marker=list(symbol='triangle', radius=8, fillColor='#ffb703')
    ) %>%
    hc_add_series(data = df_income,
                  hcaes(x=SPI.INDEX, y = y, name=country, group=country, showInLegend=TRUE),
                  type = "scatter",
                  marker=list(symbol='square', radius=8, fillColor='#fb8500')
    ) %>%
    hc_plotOptions(series = list(stacking = "normal")) %>%
    #remove yaxis title and labels
    hc_yAxis(min = 1.1, max = .9, title = list(text = ""),
      labels=list(enabled=FALSE),
      ticklength=0) %>%
    # change the marker symbol by group
    hc_xAxis(title = list(text = "SPI Overall Score")
      ) %>%
    hc_title(
      text = "The SPI overall score combines the 50+ indicators into single measure",
      style = list(fontSize = "20px",
        fontWeight = "bold",
        fontFamily = "Andes"
      )
    ) %>%
    # add tooltip info of country, SPI scores and so on
    hc_tooltip(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = "<b>{point.country}</b><br>Overall Score: {point.x}<br>",
      footerFormat = "",
      followPointer = TRUE
    ) %>%
    hc_add_theme(hc_theme_smpl()) 
}


country_report_time_trends <- function(cntry) {
## time trends
  df_trends <- SPI %>%
        select(iso3c, country, region,  income, date, SPI.INDEX, population) %>%
        rename(Score=SPI.INDEX) %>%
        mutate(ISO_A3_EH=iso3c) 
      
      #need to get the aggregates for the income group and region
      country_select_info <- country_info %>%
        filter(country==cntry)
      
      region_name<-country_select_info$region
      income_name<-country_select_info$income_level
      
      #produce aggregation
      region_agg <- df_trends %>%
        filter(region==region_name) %>%
        group_by(date) %>%
        summarise(across(starts_with('Score'),~round(mean(as.numeric(.), na.rm=T),2))) %>%
        mutate(country=region_name)
      
      income_agg <- df_trends %>%
        filter(income==income_name) %>%
        group_by(date) %>%
        summarise(across(starts_with('Score'),~round(mean(as.numeric(.), na.rm=T),2))) %>%
        mutate(country=income_name)
      
        df_trends <- df_trends %>%
          filter(country==cntry) %>%
          bind_rows(region_agg) %>%
          bind_rows(income_agg) %>%
          filter(!is.na(Score))%>%
          mutate(Score=round(Score, 1))

        df_trends <- df_trends %>%
          mutate(country=factor(country, levels=unique(df_trends$country)))

        
      hchart(
        df_trends,
        type='line',
        hcaes(
          x=date,
          y=Score,
          group=country
        ),
      ) %>%
        hc_xAxis(
          allowDecimals=FALSE
        ) %>%
    hc_title(
      text = "Progress on the SPI can be tracked over time",
      style = list(fontSize = "20px",
        fontWeight = "bold",
        fontFamily = "Andes"
      )
    ) %>%
    # add tooltip info of country, SPI scores and so on
    hc_tooltip(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = "<b>{point.country}</b><br>Overall Score: {point.y}<br>",
      footerFormat = "",
      followPointer = TRUE
    ) %>%
    hc_colors(c("#023047", "#ffb703", "#fb8500")) %>%  
    hc_add_theme(hc_theme_smpl()) 
}