options(repos="http://cran.rstudio.com/")
Sys.setenv("RENV_CONFIG_REPOS_OVERRIDE" = "http://cran.rstudio.com")

library(here)
library(wbstats)
library(tidyverse)

countries <- wbstats::wb_countries() %>%
  filter(region!="Aggregates") %>%
  select(country, iso2c, region, income_level) 

# Convert the dataframe to a list of lists
countries <- split(countries, 1:nrow(countries))


# Loop through the list of countries and generate .qmd files
for (country_info in countries) {
  # Extract country-specific information
  country <- country_info$country
  iso2c <- country_info$iso2c
  region <- country_info$region
  income_level <- country_info$income_level
  
  # Create the content for the .qmd file
  content <- paste0(
    '---\n',
    sprintf('title: "%s"\n', country),
    'subtitle: "SPI Country Brief"\n',
    'description: |\n',
    sprintf('  SPI country reports are designed to give an overview of the performance of %s.\n', country),
    'categories:\n',
    '    - Statistical Performance\n',
    sprintf('    - %s\n', region),
    sprintf('    - %s\n', income_level),
    sprintf('image: images/%s.png\n', iso2c),
    '---\n\n',
    sprintf('This is the %s Country Report.\n', country),
    
    # Add the highcharter HTML code to embed the chart
    sprintf('<iframe src="charts/%s_chart.html" width="800" height="600"></iframe>\n', country)
    # sprintf('<iframe src="charts/%s_chart1.html" width="800" height="600"></iframe>\n', country),
    # sprintf('<iframe src="charts/%s_chart2.html" width="800" height="600"></iframe>\n', country),
    # sprintf('<iframe src="charts/%s_chart3.html" width="800" height="600"></iframe>\n', country),
    # sprintf('<iframe src="charts/%s_chart4.html" width="800" height="600"></iframe>\n', country),
    # sprintf('<iframe src="charts/%s_chart5.html" width="800" height="600"></iframe>\n', country)
    
  )
  
  # Define the filename for the .qmd file
  filename <- here("country_reports","reports",paste0(country, ".qmd"))
  
  # Write the content to the .qmd file
  writeLines(content, filename)
  
  # Print a message indicating the file has been created
  cat(sprintf('Created file: %s\n', filename))
}
