library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(naniar)
library(DataExplorer)
library(jtools)
library(plm)
library(lme4)
library(lmtest)
library(parameters)
library(lme4)
library(sjPlot)
library(panelr)
library(stargazer)
library(kableExtra)

## DATA UNDERSTANDING (Independent variable) ##
###############################################

## Opening the Dataframe

df <- read_excel("C:/Users/Magalhaes/Desktop/GV/TCC/Empirical research/Concertation index.xlsx")

df2 <- read_excel("C:/Users/Magalhaes/Desktop/GV/TCC/Empirical research/ICTWSS_6_1_Dataset.xlsx")


## First, lets pick the variables needed for the index and filter for the countries we want.

df <- df %>%
  select(country = Country, year, RA_m, RCB_m, RS_m, Central, Coord, EDpriv, UD_s_private) %>%
  filter(country %in% c("Denmark", "Italy", "Japan", "Sweden", "Austria", "Portugal", "Germany", "Ireland", "Netherlands", "Belgium",
                        "Spain", "United Kingdom", "Greece", "Finland", "Australia", "United States", "Canada", 
                        "Argentina", "Brazil", "Costa Rica", "India", "Indonesia", "South Africa", "Chile", "Colombia", "Mexico"))

df2 <- df2 %>%
  select(country, year, RI) %>%
  filter(country %in% c("Denmark", "Italy", "Japan", "Sweden", "Austria", "Portugal", "Germany", "Ireland", "Netherlands", "Belgium",
                        "Spain", "United Kingdom", "Greece", "Finland", "Australia", "United States of America", "Canada", 
                        "Argentina", "Brazil", "Costa Rica", "India", "Indonesia", "South Africa", "Chile", "Colombia", "Mexico"))

# Lets organize both datasets a little bit

df[2:9] = lapply(df[2:9], FUN = function(y){as.numeric(y)})

df <- df %>% replace_with_na_all(condition = ~.x == -88)

df2[df2 == "United States of America"] <- "United States"

## Joining both datasets.

joined <- merge(df, df2, by = c("country", "year"))

## Year: Picking from 1995 to 2018.

joined <- joined %>%
  filter(year %in% c(1995:2018))

# Checking how missing variables are distributed.

(colMeans(is.na(joined)))*100

# We can see that EDpriv and UD_s_private are heavily occupied by missing data, meaning it would be hard to use it as a part of the index.
# Therefore, I will drop both EDpriv and UD_s_private. 

joined <- joined %>%
  select(country, year, RA_m, RCB_m, RS_m, Central, Coord, RI)

## Creating a dummy to indicate whether it is a advanced democracy

joined <- joined %>%
  mutate(developing = if_else(country %in% c("Denmark", "Italy", "Japan", "Sweden", "Austria", "Portugal", "Germany", "Ireland", "Netherlands", "Belgium",
                                           "Spain", "United Kingdom", "Greece", "Finland", "Australia", "United States", "Canada"), 0, 1))

## Using the mean for each group of countries to fill the NA variables.

joined <- joined %>%
  group_by(developing) %>%
  mutate(RI_adjusted = if_else(is.na(RI), round(mean(RI, na.rm=T),0), RI),
         central_adjusted = if_else(is.na(Central), round(mean(Central, na.rm=T),2), Central),
         coord_adjusted = if_else(is.na(Coord), round(mean(Coord, na.rm=T),0), Coord))

(colMeans(is.na(joined)))*100

## Creating the index

joined <- joined %>%
  mutate(concertation_index = round((RA_m + RCB_m + RS_m + central_adjusted + coord_adjusted + 2*RI_adjusted)/7,0))

concertation = joined %>%
  select(country, year, developing, concertation_index)

concertation

## DATA UNDERSTANDING (Dependent variable) ##
#############################################

df3 <- read_excel("C:/Users/Magalhaes/Desktop/GV/TCC/Empirical research/qog_ei_sept21.xlsx")

df3 <- df3 %>%
  select(country = cname, year, oecd_cctr_tot, oecd_etr_tot) %>%
  filter(country %in% c("Denmark", "Italy", "Japan", "Sweden", "Austria", "Portugal", "Germany", "Ireland", "Netherlands", "Belgium",
                        "Spain", "United Kingdom of Great Britain and Northern Ireland", "Greece", "Finland", "Australia", "United States of America", "Canada", 
                        "Argentina", "Brazil", "Costa Rica", "India", "Indonesia", "South Africa", "Chile", "Colombia", "Mexico"),
         year %in% c(1995:2018))

df3[df3 == "United States of America"] <- "United States"
df3[df3 == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"

(colMeans(is.na(df3)))*100

## JOINING BOTH DEPENDENT AND INDEPENDENT ##
############################################

merged_df <- merge(concertation, df3, by=c("country", "year"))

plot_missing(merged_df)

merged_df <- merged_df %>%
  drop_na()

## EXPLANATORY ANALYSIS ##
##########################

plot_bar(merged_df)

qq_data <- merged_df[, c("concertation_index",  "oecd_etr_tot", "oecd_cctr_tot")]

plot_qq(qq_data)

plot_density(qq_data)

plot_correlation(merged_df)

plot_prcomp(merged_df)

###

merged_df %>%
  select(country, year, developing, concertation_index, oecd_etr_tot) %>%
  group_by(country) %>%
  summarise(environment = mean(oecd_etr_tot, na.rm=T),
            developing = developing) %>%
  arrange(desc(environment)) %>%
  unique() %>%
  mutate(developing = if_else(developing == 1, "Developing", "Developed"),
         country = fct_reorder(country, desc(environment))) %>%
  ggplot() + geom_bar(mapping = aes(x = reorder(country,environment), y = environment, fill = developing), stat = "identity") +
  coord_flip() +
  theme_bw(base_size = 10) +
  xlab("") +
  labs(x = element_blank(),
       y = "Environmentally-related tax revenue as a % of total tax revenue") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(hjust=c(1), size = 8)) +
  scale_fill_manual(values = c("#b76e79", "#a5bf9f")) +
  theme(panel.grid.major=element_blank(), 
        # Removing minor gridlines
        panel.grid.minor=element_blank(), 
        # Removing panel border
        panel.border=element_blank(),     
        # Adding black line along axes
        axis.line=element_line(),         
        # Changing font to Times
        text=element_text(family = "Times"), 
        # Removing legend title
        legend.title=element_blank(),
        # Increasing size of y-axis labels
        axis.text.y=element_text(size = 10),
        # Increasing size of x-axis labels
        axis.text.x=element_text(size = 10)) 


## India is a big outlier, will remove.

merged_df <- merged_df %>%
  filter(country != 'India')

## Charts for the draft ##

merged_df %>%
  select(country, year, developing, concertation_index, oecd_etr_tot) %>%
  group_by(country) %>%
  summarise(environment = mean(oecd_etr_tot, na.rm=T),
            developing = developing) %>%
  arrange(desc(environment)) %>%
  unique() %>%
  mutate(developing = if_else(developing == 1, "Developing", "Developed"),
         country = fct_reorder(country, desc(environment))) %>%
  ggplot() + geom_bar(mapping = aes(x = reorder(country,environment), y = environment, fill = developing), stat = "identity") +
  coord_flip() +
  theme_bw(base_size = 10) +
  xlab("") +
  labs(x = element_blank(),
       y = "Environmentally-related tax revenue as a % of total tax revenue") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(hjust=c(1), size = 8)) +
  scale_fill_manual(values = c("#b76e79", "#a5bf9f")) +
  theme(panel.grid.major=element_blank(), 
        # Removing minor gridlines
        panel.grid.minor=element_blank(), 
        # Removing panel border
        panel.border=element_blank(),     
        # Adding black line along axes
        axis.line=element_line(),         
        # Changing font to Times
        text=element_text(family = "Times"), 
        # Removing legend title
        legend.title=element_blank(),
        # Increasing size of y-axis labels
        axis.text.y=element_text(size = 10),
        # Increasing size of x-axis labels
        axis.text.x=element_text(size = 10)) 

###

merged_df %>%
  select(country, year, developing, concertation_index, oecd_etr_tot) %>%
  group_by(country) %>%
  summarise(environment = mean(oecd_etr_tot, na.rm=T),
            developing = developing,
            concertation_index = mean(concertation_index, na.rm=T)) %>%
  arrange(desc(environment)) %>%
  unique() %>%
  mutate(developing = if_else(developing == 1, "Developing", "Developed")) %>%
  ggplot(mapping = aes(x = environment, y = concertation_index, label=country, color = developing), size=) + 
  geom_point() +
  geom_text(size=4, nudge_x = 0.1, hjust=0.9, vjust=1.5, alpha=2) +
  stat_smooth(method = "lm", color = "red") +
  theme_bw(base_size = 10) +
  labs(x = "Environmentally-related tax revenue as a % of total tax revenue") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(hjust=c(1), size = 8)) +
  scale_color_manual(values = c("#b76e79", "#a5bf9f")) +
  theme(panel.grid.major=element_blank(), 
        # Removing minor gridlines
        panel.grid.minor=element_blank(), 
        # Removing panel border
        panel.border=element_blank(),     
        # Adding black line along axes
        axis.line=element_line(),         
        # Changing font to Times
        text=element_text(family = "Times"), 
        # Removing legend title
        legend.title=element_blank(),
        # Increasing size of y-axis labels
        axis.text.y=element_text(size = 10),
        # Increasing size of x-axis labels
        axis.text.x=element_text(size = 10)) 

## TAKING CONTROL VARIABLES ##
##############################

merged_df <- merged_df %>%
  mutate(eu_member = if_else(country %in% c("Denmark", "Italy", "Sweden", "Austria", "Portugal", "Germany", "Ireland", "Netherlands", 
                                             "Belgium",
                                             "Spain", "United Kingdom", "Greece", "Finland"), 1, 0))

fossil_fuel <- read.csv("C:/Users/Magalhaes/Desktop/GV/TCC/Data/fossil-fuels-per-capita.csv")

fossil_fuel <- fossil_fuel %>%
  rename(country=Entity, year=Year, consumption = Fossil.fuels.per.capita..kWh.) %>%
  filter(country %in% c("Denmark", "Italy", "Japan", "Sweden", "Austria", "Portugal", "Germany", "Ireland", "Netherlands", "Belgium",
                        "Spain", "United Kingdom", "Greece", "Finland", "Australia", "United States", "Canada", 
                        "Argentina", "Brazil", "Costa Rica", "Indonesia", "South Africa", "Chile", "Colombia", "Mexico"),
         year %in% c(1995:2018))

merged_df <- merge(merged_df, fossil_fuel, by=c("country", "year"))

gdp <- read_excel("C:/Users/Magalhaes/Desktop/GV/TCC/Data/imf-dm-export-20220906.xls")
gdp <- gdp %>%
  gather(year, gdp, "1980":"2020") %>%
  select(country=`Real GDP growth (Annual percent change)`, year, gdp) %>%
  filter(country %in% c("Denmark", "Italy", "Japan", "Sweden", "Austria", "Portugal", "Germany", "Ireland", "Netherlands", "Belgium",
                        "Spain", "United Kingdom", "Greece", "Finland", "Australia", "United States", "Canada", 
                        "Argentina", "Brazil", "Costa Rica", "Indonesia", "South Africa", "Chile", "Colombia", "Mexico"),
         year %in% c(1995:2018))

gdp$gdp <- as.numeric(gdp$gdp)

merged_df <- merge(merged_df, gdp, by=c("country", "year"))
  
merged_df$consumption <- log(merged_df$consumption)

## Summary statistics ##

stargazer(merged_df, out = ".html")

### Modelling - Environmental taxation ###
##########################################

test <- merged_df

#test$consumption <- log(test$consumption)

#test <- panel_data(test, id = country, wave = year)

test <- cbind(
  test,
  demean(test, select = c("concertation_index", "consumption", "gdp"), group = "country") # from package "parameters"
)

#model_simple_rewb_climate <- lmer(
  #oecd_cctr_tot ~ concertation_index_between + concertation_index_within + consumption_between + consumption_within +
    #gdp_between + gdp_within + developing + (year | country),
  #data = test
#)

model_simple_rewb_environment <- lmer(
  oecd_etr_tot ~ concertation_index_between + concertation_index_within + consumption_between + consumption_within +
    gdp_between + gdp_within + developing + (year | country),
  data = test
)

#model <- wbm(oecd_cctr_tot ~ concertation_index + consumption + gdp | developing, data = test)
#summary(model)

#export_summs(model)

stargazer(model_simple_rewb_environment,
          type = "text", 
          allign = TRUE,
          title = "Regression results", 
          header = FALSE,
          single.row = TRUE,
          out = "model.html",
          dep.var.labels = c("Environmental-related tax revenue"),
          covariate.labels = c("Concertation (between)", "Concertation (within)", "Consumption (between)", "Consumption (within)",
                              "GDP (between)", "GDP (within)", "Developing"))


#tab_model(model_simple_rewb_climate, model_simple_rewb_environment,
  #show.ci = FALSE, 
  #show.se = TRUE, 
  #auto.label = TRUE, 
  #string.se = "SE",
  #show.icc = FALSE
#)

### Difference between developed and developing


