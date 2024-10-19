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

server <- function(input, output, session) {
  filtered_data <- reactive({
    world_bank_data %>%
      filter(Country == input$country,
             Year >= input$yearRange[1],
             Year <= input$yearRange[2])
  })
  
  filtered_region_data <- reactive({
    world_bank_data %>%
      filter(Region == input$region_select,
             Year >= input$yearRange[1],
             Year <= input$yearRange[2])
  })
  
  filtered_income_group_data <- reactive({
    world_bank_data %>%
      filter(Income_Group == input$income_group_select,
             Year >= input$yearRange[1],
             Year <= input$yearRange[2])
  })
  
  output$plot <- renderPlotly({
    data <- filtered_data()
    
    if (input$plot_type == "line") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_line(color = "steelblue", size = 1) +
        labs(y = input$indicator,
             title = paste(input$indicator, "over Time in", input$country)) +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      
    
      ggplotly(p)
      
    } else if (input$plot_type == "scatter") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_point(color = "purple", size = 1) +
        labs(y = input$indicator,
             title = paste(input$indicator, "over Time in", input$country)) +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      
      ggplotly(p)
      
    } else if (input$plot_type == "bar") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_bar(stat = "identity", color="black", fill ="orange") +
        labs(y = input$indicator,
             title = paste(input$indicator, "over Time in", input$country)) +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      ggplotly(p)
      
    } 
  })
  
  output$region_plot <- renderPlotly({
    data <- filtered_region_data()
    
if (input$plot_type == "scatter") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_point(color = "steelblue", size = 1) +
        labs(y = input$indicator, 
             title = paste(input$indicator, "over Time in", input$region_select))+
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      ggplotly(p)
      
    
    } else if (input$plot_type == "bar") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_bar(stat = "identity", fill ="purple") +
        labs(y = input$indicator, title = paste(input$indicator, "over Time in", input$region_select)) +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      ggplotly(p)
      
    } else if (input$plot_type == "box") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_boxplot(fill="#FFCC99") +

        labs(y = input$indicator, title = paste(input$indicator, "over Time in", input$region_select)) +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      ggplotly(p)
    }
  })
  
  output$income_group_plot <- renderPlotly({
    data <- filtered_income_group_data()
    
  if (input$plot_type == "scatter") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_point(color = "steelblue", size = 1) +
        labs(y = input$indicator, title = paste(input$indicator, "over Time in", input$income_group_select))+
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      ggplotly(p)
      ggplotly(p)
      
    } else if (input$plot_type == "bar") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_bar(stat = "identity", fill="purple") +
        labs(y = input$indicator, title = paste(input$indicator, "over Time in", input$income_group_select))+
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      ggplotly(p)

      
    } else if (input$plot_type == "box") {
      options(scipen = 20)
      p <- ggplot(data, aes_string(x = "Year", y = input$indicator)) +
        geom_boxplot(fill="#FFCC99") +
        labs(y = input$indicator, title = paste(input$indicator, "over Time in", input$income_group_select))+
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1),
              plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
      ggplotly(p)

    }
  })
  
  output$report_plot <- renderPlotly({
    
    report_type <- input$report_select
    
    
    if (report_type == "unemployment") {
      unemployment_data <- world_bank_data %>%
        select(Year, Region, Income_Group, Unemployment_Rate) %>%
        na.omit() %>%
        group_by(Year, Region, Income_Group) %>%
        summarise(Average_Unemployment = mean(Unemployment_Rate))
      
      p <- ggplot(unemployment_data, aes(x = Year, y = Average_Unemployment, color = Income_Group)) +
        geom_line(size = 1.2) + 
        facet_wrap(~Region, scales = "free_y", ncol = 3) + 
        labs(title = "Trend in Unemployment Rates by Income Group and Region",
             x = "Year",
             y = "Unemployment (% of total labor force)",
             color = "Income Group") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
          axis.title = element_text(size = 14), 
          axis.text = element_text(size = 12), 
          legend.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          strip.text = element_text(size = 14, face = "bold"), 
          panel.grid.major = element_line(color = "grey80", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank() 
        ) 
      
      ggplotly(p)
    } else if (report_type == "life_gdp") {
      life_gdp_data <- world_bank_data %>%
        select(Life_Expectancy, GDP_per_Capita, Income_Group, Region) %>%
        na.omit()
      
      options(scipen = 20)
      p <- ggplot(life_gdp_data, aes(x = GDP_per_Capita, y = Life_Expectancy, color = Income_Group)) +
        geom_point(alpha = 0.7, size = 3) + 
        facet_wrap(~Region, ncol = 3) +  
        labs(title = "Life Expectancy and GDP per Capita by Region and Income Group ",
             x = "GDP per Capita (Current US$)",
             y = "Life Expectancy (Years)",
             color = "Income Group") +
        theme_bw() +  
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "bottom", 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"),
          panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
          panel.grid.minor = element_line(color = "gray95", linetype = "dashed")
        ) +
        scale_x_log10()  
      
      
      ggplotly(p)
      
    } else if (report_type == "life_density") {
      p <- ggplot(world_bank_data, aes(x = Life_Expectancy, y = GDP_per_Capita, size = Population_Density, color = Region)) +
        geom_point(alpha = 0.7, shape = 20, stroke = 0.75) +  
        scale_y_log10(labels = scales::comma) + 
        scale_size_continuous(range = c(2, 10), name = "Population Density\n(people per sq. km)") +  
        scale_color_brewer(palette = "Set1") +  
        labs(title = "Population Density by Life Expentancy and Region",  
             x = "Life Expectancy (Years)",
             y = "GDP per Capita") + 
        theme_bw() +  
        theme(
          panel.grid.major = element_line(color = "grey80", size = 0.2),
          panel.grid.minor = element_line(color = "grey90", size = 0.1),
          axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 14),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 9)
        ) +
        guides(color = guide_legend(title = "Region", override.aes = list(size = 5)))  
      
      ggplotly(p)
      
    } else if (report_type == "infant_mortality") {
      infant_mortality_data <- world_bank_data %>%
        select(Year, Region, Infant_Mortality_Rate) %>%
        na.omit() %>%
        group_by(Year, Region) %>%
        summarise(Average_Infant_Mortality = mean(Infant_Mortality_Rate))
      
      p <- ggplot(infant_mortality_data, aes(x = Year, y = Average_Infant_Mortality, color = Region)) +
        geom_line(size = 1) +  
        labs(title = "Trend in Infant Mortality Rate by Region",
             x = "Year",
             y = "Infant Mortality Rate (per 1,000 live births)",
             color = "Region") +
        theme_minimal() +
        
        scale_color_brewer(palette = "Set1") +  
        theme(
          plot.title = element_text(size = 16, face = "bold"),  
          axis.title = element_text(size = 12),                
          legend.title = element_text(size = 12),              
          panel.grid.major = element_line(color = "grey90", linetype = "dashed"),   
          panel.grid.minor = element_blank()                   
        ) 
      
      ggplotly(p)
      
    } else if (report_type == "birth_rates") {
      p<- ggplot(world_bank_data, aes(x = Year, y = Birth_Rate, fill = Region)) +
        geom_violin(trim = FALSE, alpha = 0.8) +  
        facet_wrap(~ Income_Group, scales = "free_y") +  
        labs(title = "Birth Rates Year-wise by Region and Income Group",
             x = "Year",
             y = "Birth Rate") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "gray", linetype = "dashed"),
          panel.grid.minor = element_line(color = "gray", linetype = "dotted"),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        ) +
        scale_fill_viridis_d(option = "plasma", direction = -1) 
      ggplotly(p)
      
    } else if (report_type == "death_rates") {
      p<- ggplot(world_bank_data, aes(x = Year, y = Death_Rate, fill = Region)) +
        geom_violin(trim = FALSE, alpha = 0.8) +  
        facet_wrap(~ Income_Group, scales = "free_y") +  
        labs(title = "Death Rates Year-wise by Region and Income Group",
             x = "Year",
             y = "Death Rate") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "gray", linetype = "dashed"),
          panel.grid.minor = element_line(color = "gray", linetype = "dotted"),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        ) +
        scale_fill_viridis_d(option = "Paired", direction = -1) 
      ggplotly(p)
    } else if (report_type == "electric_power") {
      electric_gdp_data <- world_bank_data %>%
        select(Electric_Power_Consumption_per_Capita, GDP_per_Capita, Income_Group) %>%
        na.omit()
      
      p <- ggplot(electric_gdp_data, aes(x = Income_Group, y = Electric_Power_Consumption_per_Capita, fill = Income_Group)) +
        geom_boxplot() +
        scale_y_log10() + 
        labs(title = "Box Plot: Electric Power Consumption by Income Group",
             x = "Income Group",
             y = "Electric Power Consumption (kWh per capita)",
             fill = "Income Group") +
        theme_minimal()
      ggplotly(p)
    } else if (report_type == "birth_infant") {
      
      p<- ggplot(world_bank_data, aes(x = Birth_Rate, y = Infant_Mortality_Rate)) +
        geom_point(aes(color = Region), alpha = 0.7, size = 3) +
        scale_color_viridis(discrete = TRUE, option = "D", direction = -1) +  
        labs(
          title = "Birth Rate vs. Infant Mortality Rate",
          x = "Birth Rate (per 1000 people)",
          y = "Infant Mortality Rate (per 1000 live births)",
          color = "Region"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_line(color = "gray", linetype = "dashed", size = 0.25)
        )
      ggplotly(p)
    } else if (report_type == "internet_gdp") {
       correlation <- cor(world_bank_data$Internet_Users_Percentage, world_bank_data$GDP_per_Capita, use="complete.obs")
      correlation
      ggplot(world_bank_data, aes(x = Internet_Users_Percentage, y = GDP_per_Capita)) +
        geom_point(color = "#336699", size = 2, alpha = 0.7) +  
        geom_smooth(method = "lm", se = FALSE, color = "#FF6600", linetype = "dashed") +  
        labs(
          title = "Relationship between Internet Usage and GDP per Capita",
          subtitle = paste0("Correlation: ", round(correlation, 3)),  
          x = "Internet Users Percentage",
          y = "GDP per Capita (USD)"  
        ) +
        theme_bw() +  
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid.major = element_line(color = "#EEEEEE"),
          panel.grid.minor = element_line(color = "#EEEEEE")
        )
    
    } else if (report_type == "correlation") {
      cor_matrix <- cor(world_bank_data[, c("Birth_Rate", "Death_Rate", 
                                            "Electric_Power_Consumption_per_Capita", "GDP_per_Capita", 
                                            "Internet_Users_Percentage", "Infant_Mortality_Rate", 
                                            "Life_Expectancy", "Population_Density", 
                                            "Unemployment_Rate")
      ], use = "pairwise.complete.obs")
      
      
      cor_data <- melt(cor_matrix)
      
      p<- ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "white", size = 0.5) + 
        scale_fill_viridis(option = "magma", name = "Correlation") + 
        geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
        labs(x = "", y = "") +
        ggtitle("Correlation Heatmap of World Bank Data") +
        theme_minimal() +  
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10, vjust = 1, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title = element_blank(),  
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey80", size = 0.2),  
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 10, face = "bold"),
          legend.title = element_text(size = 12, face = "bold")
        ) +
        coord_fixed()
      ggplotly(p)
    }
    
    
  })
  
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })
  
  country_info <- eventReactive(input$country, {
    infoBox("Country", input$country, icon = icon("flag"))
  })
  
  region_info <- eventReactive(input$country, {
    region <- world_bank_data %>% filter(Country == input$country) %>% select(Region) %>% distinct()
    infoBox("Region", region$Region[1], icon = icon("globe"))
  })
  
  income_group_info <- eventReactive(input$country, {
    income_group <- world_bank_data %>% filter(Country == input$country) %>% select(Income_Group) %>% distinct()
    infoBox("Income Group", income_group$Income_Group[1], icon = icon("money-bill"))
  })
  
  output$country_info <- renderInfoBox({
    country_info()
  })
  
  output$region_info <- renderInfoBox({
    region_info()
  })
  
  output$income_group_info <- renderInfoBox({
    income_group_info()
  })
  
  output$map <- renderPlotly({
    map_data <- NULL
    title <- NULL
    color_palette <- NULL
    
    if (input$map_type == "birth_rate") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(Birth_Rate, na.rm = TRUE))
      title <- "Average Birth Rate (2000-2018)"
      color_palette <- "Viridis"
      
    } else if (input$map_type == "death_rate") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(Death_Rate, na.rm = TRUE))
      title <- "Average Death Rate (2000-2018)"
      color_palette <- "Inferno"
      
    } else if (input$map_type == "electric_power") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(Electric_Power_Consumption_per_Capita, na.rm = TRUE))
      title <- "Average Electric Power Consumption (kWh per capita 2000-2018)"
      color_palette <- "Plasma"
      
    } else if (input$map_type == "gdp") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(GDP, na.rm = TRUE))
      title <- "Average GDP (2000-2018)"
      color_palette <- "Magma"
      
    } else if (input$map_type == "gdp_per_capita") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(GDP_per_Capita, na.rm = TRUE))
      title <- "Average GDP Per Capita (2000-2018)"
      color_palette <- "Cividis"
      
    } else if (input$map_type == "internet_users") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(Internet_Users_Percentage, na.rm = TRUE))
      title <- "Average Internet Users Percentage (2000-2018)"
      color_palette <- "YlGnBu"
      
    } else if (input$map_type == "infant_mortality") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(Infant_Mortality_Rate, na.rm = TRUE))
      title <- "Average Infant Mortality Rate (2000-2018)"
      color_palette <- "RdPu"
      
    } else if (input$map_type == "life_expectancy") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(Life_Expectancy, na.rm = TRUE))
      title <- "Average Life Expectancy (2000-2018)"
      color_palette <- "BuGn"
      
    } else if (input$map_type == "population_density") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(Population_Density, na.rm = TRUE))
      title <- "Average Population Density (2000-2018)"
      
      
    } else if (input$map_type == "unemployment_rate") {
      map_data <- world_bank_data %>%
        group_by(Country) %>%
        summarise(Value = mean(Unemployment_Rate, na.rm = TRUE))
      title <- "Average Unemployment Rate (2000-2018)"
      color_palette <- "Oranges"
    }
    
    map_data <- map_data %>%
      mutate(Country = countrycode::countrycode(Country, "country.name", "iso3c"))
    
    map <- plot_ly(data = map_data, type = 'choropleth', locations = ~Country, z = ~Value, text = ~Country,
                   colorscale = color_palette, marker = list(line = list(color = 'rgb(255,255,255)', width = 1))) %>%
      layout(title = title, geo = list(showframe = FALSE, showcoastlines = FALSE, projection = list(type = 'Mercator')))
    
    map
  })
}