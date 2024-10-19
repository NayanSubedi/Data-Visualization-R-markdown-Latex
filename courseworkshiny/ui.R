library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
library(DT)
library(plotly)
library(rworldmap)
library(reshape2)
library(viridis)
library(RColorBrewer)
library(shinydashboard)


wb_data <- read.csv("WorldBank.csv")



names(wb_data) <- c(
  "Country", "Country_Code", "Region", "Income_Group", "Year", 
  "Birth_Rate", "Death_Rate", 
  "Electric_Power_Consumption_per_Capita", "GDP", "GDP_per_Capita", 
  "Internet_Users_Percentage", "Infant_Mortality_Rate", 
  "Life_Expectancy", "Population_Density", 
  "Unemployment_Rate")


world_bank_data <- wb_data %>% 
  filter(Year >= 2000 & Year <= 2018)

ui <- dashboardPage(
  dashboardHeader(title = "World Bank Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Country", tabName = "country", icon = icon("flag")),
      menuItem("Region", tabName = "region", icon = icon("globe")),
      menuItem("Income Group", tabName = "income_group", icon = icon("money-bill")),
      menuItem("Maps", tabName = "maps", icon = icon("globe")),
      menuItem("Report Plots", tabName = "report_plots", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "World Economic Indicators", width = 12, status = "primary", solidHeader = TRUE,
                  "This web app provides insights into various World Economic Indicators as provided by the World Bank.
                  The data includes metrics such as GDP, unemployment rates, life expectancy, electric power consumption,
                  internet usage. You can explore data trends across different countries
                  and regions, compare indicators, and analyze economic progress over time.
                  
                  Use the sidebar to select a country and an indicator to visualize. You can also choose the type of plot
                  and adjust the year range to focus on specific periods. The Data Table tab allows you to view and interact
                  with the underlying data in a tabular format."
                )
              )
      ),
      tabItem(tabName = "country",
              fluidRow(
                infoBoxOutput("country_info"),
                infoBoxOutput("region_info"),
                infoBoxOutput("income_group_info")
              ),
              fluidRow(
                box(
                  width = 12,
                  selectInput("country", "Select Country", choices = unique(world_bank_data$Country)),
                  selectInput("indicator", "Select Indicator", choices = names(world_bank_data)[6:15]),
                  radioButtons("plot_type", "Select Plot Type", 
                               choices = list("Line Plot" = "line", "Scatter Plot" = "scatter", "Bar Plot" = "bar")),
                  sliderInput("yearRange", "Select Year Range", min = 2000, max = 2018, value = c(2000, 2018)),
                  plotlyOutput("plot", height = "600px")
                )
              )
      ),
      tabItem(tabName = "region",
              
              fluidRow(
                box(
                  width = 12,
                  selectInput("region_select", "Select Region", choices = unique(world_bank_data$Region)),
                  selectInput("indicator", "Select Indicator", choices = names(world_bank_data)[6:15]),
                  radioButtons("plot_type", "Select Plot Type",
                               choices = list( "Scatter Plot" = "scatter", "Bar Plot" = "bar", "Box Plot" = "box")),
                  sliderInput("yearRange", "Select Year Range", min = 2000, max = 2018, value = c(2000, 2018)),
                  plotlyOutput("region_plot", height = "600px")
                )
              )
      ),
      tabItem(tabName = "income_group",
              fluidRow(
                box(
                  width = 12,
                  selectInput("income_group_select", "Select Income Group", choices = unique(world_bank_data$Income_Group)),
                  selectInput("indicator", "Select Indicator", choices = names(world_bank_data)[6:15]),
                  radioButtons("plot_type", "Select Plot Type",
                               choices = list( "Scatter Plot" = "scatter", "Bar Plot" = "bar", "Box Plot" = "box")),
                  sliderInput("yearRange", "Select Year Range", min = 2000, max = 2018, value = c(2000, 2018)),
                  plotlyOutput("income_group_plot", height = "600px")
                )
              )
      ),
      
      tabItem(tabName = "data_table",
              fluidRow(
                box(width = 12, DT::dataTableOutput("data_table"))
              )
      ),
      tabItem(tabName = "maps",
              fluidRow(
                box(
                  width = 12,
                  selectInput("map_type", "Select Map Type", choices = c(
                    "Average Birth Rate" = "birth_rate",
                    "Average Death Rate" = "death_rate",
                    "Average Electric Power Consumption" = "electric_power",
                    "Average GDP" = "gdp",
                    "Average GDP Per Capita" = "gdp_per_capita",
                    "Average Internet Users Percentage" = "internet_users",
                    "Average Infant Mortality Rate" = "infant_mortality",
                    "Average Life Expectancy" = "life_expectancy",
                    "Average Population Density" = "population_density",
                    "Average Unemployment Rate" = "unemployment_rate"
                  )),
                  plotlyOutput("map", height = "600px")
                )
              )
      ),
      tabItem(tabName = "report_plots",
              fluidRow(
                box(
                  width = 12,
                  selectInput("report_select", "Select Report Type", choices = c(
                    "Unemployment Rates Trend" = "unemployment",
                    "Life Expectancy vs GDP Per Capita" = "life_gdp",
                    "Population Density and Life Expectancy" = "life_density",
                    "Trend in Infant Mortality Rate" = "infant_mortality",
                    "Birth Rates by Region and Income Group" = "birth_rates",
                    "Death Rates by Region and Income Group" = "death_rates",
                    "Electric Power Consumption vs Income Group" = "electric_power",
                    "Birth Rate vs Infant Mortality Rate" = "birth_infant",
                    "Internet Usage and GDP per Capita" = "internet_gdp",
                    "Correlation Heatmap" = "correlation"
                  )),
                  
                  plotlyOutput("report_plot", height = "800px")
                )
              )
      )
    )
  )
)