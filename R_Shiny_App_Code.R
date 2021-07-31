library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(maps)
library(dplyr)
library(forcats)
library(gganimate)


### Importing and preparing Data ###

data <- read_csv("owid-energy-data.csv")
data <- tibble(data) 

data <- replace_na(data, list(solar_consumption=0, hydro_consumption=0, wind_consumption=0, year=0, hydro_cons_change_pct=0, hydro_share_energy=0, wind_share_elec=0)) # Replacing NA values with zero
data$year <- as.character(data$year) # Changing type of year to character
data$country <- recode(data$country, "United States"="USA", "Democratic Republic of Congo"="Democratic Republic of the Congo") # Recoding names of countries to make join with map data possible 


### Data for figure 1 ### 

data_1 <- data %>% 
  select(country, year, renewables_share_energy, renewables_consumption) %>%  # Selecting needed columns
  rename(region=country, year=year, renewables_share_energy=renewables_share_energy, renewables_consumption=renewables_consumption) # Renaming column country to make join with map data possible
data_1

my_world_map <- map_data("world") # Calling map data from maps package
my_world_map_cleaned <- select(my_world_map, long, lat, region, group) # Selecting needed columns

fig1_data <- left_join(my_world_map_cleaned, data_1, by="region") # Joining Our World in Data dataset with map data available in maps package


### Data for figure 5 ###

fig5_data <- data %>%
  select(country, year, wind_share_elec) %>%   # Selecting needed columns
  rename(country_fig5=country, year=year, wind_share_elec=wind_share_elec)%>% # Renaming country column to make navigation panel work
  filter(country_fig5 %in% c("Europe", "Australia", "North America", "Africa", "World")) %>% # Choosing regions to be shown on a graph
  replace_na(list(wind_share_elec=0)) # Replacing NA values


### Data for figure 7 ###

fig7_data <- data %>%
  select(country, year, solar_electricity, solar_share_elec, solar_cons_change_pct, solar_share_energy, solar_cons_change_twh) %>%   # Selecting needed columns
  filter(year>1990) %>% # Filtering years 
  rename(country_fig7=country) # Renaming country column




### Making Shiny App ###

### User Interface ###

ui <- fluidPage(
  navbarPage(      # Creating top level navigation bar with multiple pages
    title="Renewable Energy",
    
    
    ### Page 1 ###
    
    tabPanel("General Information",
             
             # Creating sidebar navigation panel - user can choose year and the and the first plot on a page will be automatically adjusted
             
             sidebarLayout(
               sidebarPanel(
                 
                 sliderInput(
                   inputId = "year",
                   label = "Select year",
                   min= 1965,
                   max = 2019,
                   value = 2019) # Default value
               ),
               
               # Creating main panel with output elements
               
               mainPanel("", plotlyOutput("plot1"), plotlyOutput("plot2"))
              )
    ),
    
    
    ### Page 2 ###
    
    tabPanel("Hydropower",
             
             # Creating sidebar navigation panel - user can choose multiple countries and the first plot on a page will be automatically adjusted
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(
                   inputId = "country",
                   label = "Select countries",
                   choices = unique(data$country), # All countries available in a dataset may be selected
                   selected = c("Germany", "India", "USA","Brazil", "Sweden", "China", "United Kingdom", "World"), # Countries selected by default
                   multiple = TRUE) # Multiple selection is possible 
               ),
               
               # Creating main panel with output elements
               
               mainPanel("", plotlyOutput("plot3"), plotlyOutput("plot4"))
             )
    ),
    
    
    ### Page 3 ###
    
    tabPanel("Wind energy",
             
             # Creating sidebar navigation panel - user can choose multiple regions and the first plot on a page will be automatically adjusted
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(
                   inputId = "country_fig5",
                   label = "Select region",
                   choices = unique(fig5_data$country_fig5), # Only regions specified in fig5_data may be selected 
                   selected = "World", # Default option
                   multiple = TRUE) # Multiple selection is possible
               ),
               
               # Creating main panel with output elements
               
               mainPanel("", plotlyOutput("plot5"), plotlyOutput("plot6"))
             )
    ),
    
    
    ### Page 4 ### 
    
    tabPanel("Solar power",
             
             # Creating sidebar navigation panel - user can choose a country and the data table will be automatically adjusted
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(
                   inputId = "country_fig7",
                   label = "Select country",
                   choices = unique(fig7_data$country_fig7), # Only regions specified in fig7_data may be selected
                   selected = "World") # Default option 
               ),
               
               # Creating main panel with output elements
               
               mainPanel(dataTableOutput(outputId = "table1"), plotlyOutput("plot7"))
             )
    ),
    
    
    ### Page 5 ###
    
    tabPanel("Copyright",
             
             # Adding and formatting Copyright notice 
             
             mainPanel(
               h1("Copyright notice"),
               p("This project was created as part of Coursera specialization", a("Data Visualization & Dashboarding with R.", href="https://www.coursera.org/specializations/jhu-data-visualization-dashboarding-with-r")),
               p("I used a dataset published under Creative Commons BY license by ", a(em("Our World in Data"), href="https://ourworldindata.org"), " and created by Hannah Ritchie, Max Roser and Edouard Mathieu. The dataset is available ", a("here.", href="https://github.com/owid/energy-data"), "An inspiration for the creation of the graphs was ", a("this", href="https://ourworldindata.org/renewable-energy"), "page."),
               p(("Kinga Siwiec (kinga.siwiec@gmail.com), 2021"))
             )
    )
  )
)




### Server function ### 


server <- function(input, output){
  
  
    ### Page 1 ###
  
    # Plot 1 #
  
  output$plot1 <- renderPlotly({ # Using plotly for interactivity 
    ggplot(
      filter(fig1_data, year == input$year), # Filtering data - year is chosen by user - connected with user interface function for page 1
      mapping = aes(x=long, y=lat, group=group, text=region, fill=renewables_share_energy))+ # Adding a map and fill
      geom_polygon()+ # Choosing geom - map
      labs(x="", y="", title="Percentage share of renewable energy consumption", fill="Percentage share of renewable energy")+ # Labels
      scale_fill_distiller(palette=1, direction=1) # Color scale
  })
  
  # Plot 2 #
  
  output$plot2 <- renderPlotly({
    ggplot(
      filter(data, country %in% c("Europe", "Africa", "North America", "Australia"), year >= 1965), # Filtering data - countries and year
      aes(x=year, y=renewables_share_energy, fill=country))+ # Setting x and y axis and fill
      geom_area(aes(group=country, text=NULL))+ # Grouping countries and creating area graph
      labs(x="Year", y="Share of renewable energy consumption", title="Share of energy consumption that comes from renewables", fill="Region")+ # Labels
      scale_fill_brewer()+ # Color scale 
      theme(axis.text.x=element_text(angle=45))+ # Rotating x labels by 45 degrees
      scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) # Only set years are listed on x axis labels
  })
  
  
    ### Page 2 ###
  
    # Plot 3 #
  
  output$plot3 <- renderPlotly({
    ggplot(
      filter(data, (year == 1980 | year == 2019), country %in% input$country), # Filtering data - years are set, countries are chosen by user 
      aes(y=hydro_share_energy, x=reorder(country, -hydro_share_energy), fill=hydro_share_energy))+ # Graph is always ordered by value - countries with highest share are shown on a bottom
      geom_bar(stat="identity")+ # Bar chart - Y values are provided in dataset, so stat="identity" is needed
      coord_flip()+ # Flipping chart - creating horizontal bar chart 
      labs(x="", y="", title="Share of energy consumption from hydropower in 1980 and 2019")+
      scale_fill_distiller(direction=1)+ # Colors and direction - the bigger share the more intensive the color 
      theme(legend.position="none")+ # No legend is shown 
      facet_wrap(~year) # Two graphs are created, each showing different year
  })
  
    # Plot 4 #
  
  output$plot4 <- renderPlotly({
    ggplot(
      filter(data, year > 1965, country %in% c("Europe", "North America")), 
      aes(x=year, y=hydro_consumption, color=country))+ 
      geom_line(aes(group=country))+ # Creating line graph 
      geom_point()+ # Adding points to a graph 
      labs(x="Year", y="Energy consumption from hydropower", title="Energy consumption from hydropower in TWh per region", fill="Region")+ # Labels including legend 
      scale_color_manual(values=c("#063970", "#76b5c5"), name="Region")+ # Setting colors manually
      scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020))+ # Setting years to be listed on x axis labels
      transition_reveal(year) # Creating an animation 
  })
  
  
    ### Page 3 ###
  
    # Plot 5 # 
  
  output$plot5 <- renderPlotly({
    ggplot(
      filter(fig5_data, year > 1985, country_fig5 %in% input$country_fig5), # Filtering data - regions can be chosen by a user via interactive sidebar 
      aes(x=year, y=wind_share_elec, color=country_fig5))+
      geom_point()+ # Creating scatter plot
      geom_jitter()+
      labs(x="Year", y="Electricity consumption from wind", title="Share of electricity consumption that comes from wind per year")+
      scale_fill_distiller(palette=1)+ # Setting colors
      scale_x_discrete(breaks=c(1990, 2000, 2010, 2020))+ # Setting years to be listed on x axis labels
      scale_color_manual(values=c("#6baed6", "#4292c6", "#2171b5", "#084594"), name="Region") # Setting colors and legend name
  })
  
    # Plot 6 # 
  
  output$plot6 <- renderPlotly({
    ggplot(
      filter(data, (year == 1990 | year == 2000 | year == 2010 | year == 2019), country %in% c("Europe", "Africa", "North America", "Australia")),
      aes(x=year, y=wind_elec_per_capita, fill=country))+ # Setting fill as country creates a stacked bar plot
      geom_bar(stat="identity")+ # Y values are provided, so stat="identity" is needed
      labs(x="Year", y="Electricity consumption in TWh", title="Per capita electricity consumption in TWh")+
      scale_fill_brewer(name="Region") # Color scale
  })
  
  
   ### Page 4 ###
  
    # Plot 7 #
  
  output$table1 <- renderDataTable({ # Creating data table
    filter(fig7_data, country_fig7 == input$country_fig7) # Countries can be chosen by a user via interactive sidebar panel
  })
  
  # Plot 8 #
  
  output$plot7 <- renderPlotly({
    ggplot(
      filter(data, year > 2000, country %in% c("World", "Europe", "North America", "Australia", "Africa")), 
      aes(x=year, y=solar_cons_change_twh, color=country))+
      geom_line(aes(group=country))+ # Creating line graph
      labs(x="", y="", title="Annual change in solar consumption in TWh")+
      scale_color_manual(values=c("#6baed6", "#4292c6", "#2171b5", "#084594", "#08208e"), name="Region")+ # Colors and legend name
      scale_x_discrete(breaks=c(2000, 2010, 2019))+ # Setting years to be listed on x axis labels
      facet_wrap(~country) # Creating multiple graphs, each showing plot for a different country
  })
}



### Constructing and starting Shiny app from UI and server function ### 

shinyApp(ui, server)
