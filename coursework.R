#LL
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(rworldmap)
library(reshape2)
library(viridis)
library(plotly)
##Data Overview
wb_data <- read.csv("WorldBank.csv")

head(wb_data)
str(wb_data)
summary(wb_data)

#data Cleaning
names(wb_data) <- c(
  "Country", "Country_Code", "Region", "Income_Group", "Year", 
  "Birth_Rate", "Death_Rate", 
  "Electric_Power_Consumption_per_Capita", "GDP", "GDP_per_Capita", 
  "Internet_Users_Percentage", "Infant_Mortality_Rate", 
  "Life_Expectancy", "Population_Density", 
  "Unemployment_Rate")


sum(is.na(wb_data))

world_bank_data <- wb_data %>% 
  filter(Year >= 2000 & Year <= 2018)

sum(is.na(world_bank_data))

#Data Exploration

View(world_bank_data)


#1.How have unemployment rates evolved across different regions and income groups?
unemployment_data <- world_bank_data %>%
  select(Year, Region, Income_Group, Unemployment_Rate) %>%
  na.omit() %>%
  group_by(Year, Region, Income_Group) %>%
  summarise(Average_Unemployment = mean(Unemployment_Rate))

p1 <- ggplot(unemployment_data, aes(x = Year, y = Average_Unemployment, color = Income_Group)) +
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

p1

#2.Does the relationship between life expectancy and GDP per capita vary across regions and income groups?
life_gdp_data <- world_bank_data %>%
  select(Life_Expectancy, GDP_per_Capita, Income_Group, Region) %>%
  na.omit()

options(scipen = 20)
p2 <- ggplot(life_gdp_data, aes(x = GDP_per_Capita, y = Life_Expectancy, color = Income_Group)) +
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


p2

#3.What is the trend of Life Expectancy in region with population Density?
p3 <- ggplot(world_bank_data, aes(x = Life_Expectancy, y = GDP_per_Capita, size = Population_Density, color = Region)) +
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

p3


#4.How has the infant mortality rate changed over time in different regions?
infant_mortality_data <- world_bank_data %>%
  select(Year, Region, Infant_Mortality_Rate) %>%
  na.omit() %>%
  group_by(Year, Region) %>%
  summarise(Average_Infant_Mortality = mean(Infant_Mortality_Rate))

p4 <- ggplot(infant_mortality_data, aes(x = Year, y = Average_Infant_Mortality, color = Region)) +
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

p4

#5. Are there differences in the rate of change in birth rates between regions or income groups?
p5 <- ggplot(world_bank_data, aes(x = Year, y = Birth_Rate, fill = Region)) +
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
p5

#6. Is the relationship between income group and death rate consistent across regions?
p6 <-ggplot(world_bank_data, aes(x = Year, y = Death_Rate, fill = Region)) +
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

p6
#7.How does the average electric power consumption per capita compare across income groups?
electric_gdp_data <- world_bank_data %>%
  select(Electric_Power_Consumption_per_Capita, GDP_per_Capita, Income_Group) %>%
  na.omit()


p7 <- ggplot(electric_gdp_data, aes(x = Income_Group, y = Electric_Power_Consumption_per_Capita, fill = Income_Group)) +
  geom_boxplot() +
  scale_y_log10() + 
  labs(title = "Box Plot: Electric Power Consumption by Income Group",
       x = "Income Group",
       y = "Electric Power Consumption (kWh per capita)",
       fill = "Income Group") +
  theme_minimal()


p7

p8 <-ggplot(world_bank_data, aes(x = Birth_Rate, y = Infant_Mortality_Rate)) +
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
p8

#8. Is there a strong correlation between internet usage and GDP per capita?
correlation <- cor(world_bank_data$Internet_Users_Percentage, world_bank_data$GDP_per_Capita, use="complete.obs")
correlation
p9 <-ggplot(world_bank_data, aes(x = Internet_Users_Percentage, y = GDP_per_Capita)) +
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
p9
#9.What are the correlations among various socio-economic variables in the dataset?
cor_matrix <- cor(world_bank_data[, c("Birth_Rate", "Death_Rate", 
                                      "Electric_Power_Consumption_per_Capita", "GDP_per_Capita", 
                                      "Internet_Users_Percentage", "Infant_Mortality_Rate", 
                                      "Life_Expectancy", "Population_Density", 
                                      "Unemployment_Rate")
], use = "pairwise.complete.obs")


cor_data <- melt(cor_matrix)

p10 <- ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
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

p10


average_birth_rate <- world_bank_data %>%
  group_by(Country) %>%
  summarise(Birth_Rate = mean(Birth_Rate, na.rm = TRUE))


BR <- joinCountryData2Map(average_birth_rate, joinCode = "NAME", nameJoinColumn = "Country")


birth_rate_map <- mapCountryData(BR, nameColumnToPlot = "Birth_Rate", mapTitle = "Average Birth Rate (2000-2018)", 
                                 colourPalette = "heat", missingCountryCol = "black")



average_death_rate <- world_bank_data %>%
  group_by(Country) %>%
  summarise(Death_Rate = mean(Death_Rate, na.rm = TRUE))


DR <- joinCountryData2Map(average_death_rate, joinCode = "NAME", nameJoinColumn = "Country")


death_rate_map <- mapCountryData(DR, nameColumnToPlot = "Death_Rate", mapTitle = "Average Death Rate (2000-2018)", 
                                 colourPalette = "white2Black", missingCountryCol = "white")



average_electric_power<- world_bank_data %>%
  group_by(Country) %>%
  summarise(Electric_Power_Consumption_per_Capita = mean(Electric_Power_Consumption_per_Capita, na.rm = TRUE))


EPR <- joinCountryData2Map(average_electric_power, joinCode = "NAME", nameJoinColumn = "Country")


electric_power_map <- mapCountryData(EPR, nameColumnToPlot = "Electric_Power_Consumption_per_Capita", mapTitle = " Average Electric Power Consumption (kWh per capita 2000-2018)", 
                                     colourPalette = "diverging", missingCountryCol = "black")


average_GDP<- world_bank_data %>%
  group_by(Country) %>%
  summarise(GDP = mean(GDP, na.rm = TRUE))


GDPM <- joinCountryData2Map(average_GDP, joinCode = "NAME", nameJoinColumn = "Country")


GDP_map <- mapCountryData(GDPM, nameColumnToPlot = "GDP", mapTitle = " Average GDP", 
                                     colourPalette = "topo", missingCountryCol = "black")

average_gdp_per_capita<- world_bank_data %>%
  group_by(Country) %>%
  summarise(GDP_per_Capita = mean(GDP_per_Capita, na.rm = TRUE))


GDPPC <- joinCountryData2Map(average_gdp_per_capita, joinCode = "NAME", nameJoinColumn = "Country")


GDPC_map <- mapCountryData(GDPPC, nameColumnToPlot = "GDP_per_Capita", mapTitle = "Average GDP Per Capita", 
                                     colourPalette = "rainbow", missingCountryCol = "black")

average_internet_users<- world_bank_data %>%
  group_by(Country) %>%
  summarise(Internet_Users_Percentage = mean(Internet_Users_Percentage, na.rm = TRUE))


IUP <- joinCountryData2Map(average_internet_users, joinCode = "NAME", nameJoinColumn = "Country")


IUP_map <- mapCountryData(IUP, nameColumnToPlot = "Internet_Users_Percentage", mapTitle = "Average Internet Users Percentage", 
                           colourPalette = "terrain", missingCountryCol = "black")




average_Infant_Mortality_Rate<- world_bank_data %>%
  group_by(Country) %>%
  summarise(Infant_Mortality_Rate = mean(Infant_Mortality_Rate, na.rm = TRUE))


IMR <- joinCountryData2Map(average_Infant_Mortality_Rate, joinCode = "NAME", nameJoinColumn = "Country")


IMR_map <- mapCountryData(IMR, nameColumnToPlot = "Infant_Mortality_Rate", mapTitle = "Average Infant Mortality Rate", 
                          colourPalette = "white2Black", missingCountryCol = "white")


my_palette <- brewer.pal(5, "Blues")

average_Life_Expentancy <- world_bank_data %>%
  group_by(Country) %>%
  summarise(Life_Expectancy = mean(Life_Expectancy, na.rm = TRUE))

LE <- joinCountryData2Map(average_Life_Expentancy, joinCode = "NAME", nameJoinColumn = "Country")

LE_map <- mapCountryData(LE, nameColumnToPlot = "Life_Expectancy", mapTitle = "Average Life Expectancy", 
                         colourPalette = my_palette, missingCountryCol = "black")


my_palette2 <- brewer.pal(5, "Purples")

average_Population_Density <- world_bank_data %>%
  group_by(Country) %>%
  summarise(Population_Density = mean(Population_Density, na.rm = TRUE))

PD <- joinCountryData2Map(average_Population_Density, joinCode = "NAME", nameJoinColumn = "Country")

PD_map <- mapCountryData(PD, nameColumnToPlot = "Population_Density", mapTitle = "Average Population Density", 
                         colourPalette = my_palette2, missingCountryCol = "black")



my_palette3 <- brewer.pal(5, "PuOr")

average_Unemployment_Rate <- world_bank_data %>%
  group_by(Country) %>%
  summarise(Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE))

UR <- joinCountryData2Map(average_Unemployment_Rate, joinCode = "NAME", nameJoinColumn = "Country")

UR_map <- mapCountryData(UR, nameColumnToPlot = "Unemployment_Rate", mapTitle = "Average Unemployment Rate of Labour Force", 
                         colourPalette = my_palette3, missingCountryCol = "white")



