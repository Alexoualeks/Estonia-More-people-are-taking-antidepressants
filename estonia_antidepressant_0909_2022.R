# Plotting the antidepressant usage in Estonia between 2017 and 2022

set.seed(1991)

# Importing packages
library(pacman)
p_load(tidyverse, readr, ggthemes, ragg)

# Import all N category (Nervous system) prescription
n_medication <- read_delim("atc_n_2017_2022.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 1)

############
# Cleaning #
############

n_medication <- n_medication %>%
  
  # Split the ATC classification code and the names of the drugs
  separate("ATC code and group, active substance", 
           c('atc_code', 'name'), 
           sep = ':') %>%
  
  # Change the '-' to 'NA'
  replace(. == '-', NA)


#########
# Plots #
#########

# Bar plot of yearly antidepressant DDD/1000inhabitants/day rate separated by medication type
antidepressant_consumption_rate_plot <- n_medication %>%
  
  # Keep only N06AA, N06AB and N06AX medications
  filter(atc_code == 'N06AA' | atc_code == 'N06AB' | atc_code == 'N06AX') %>%
  
  # Pivot the years into a single column
  # add the values to a DDD/1000inhabitants/day column
  pivot_longer('2017':'2021',
               names_to = 'year',
               values_to = 'DDD_1000_day') %>%
  
  # Change the years and DDD_1000_day as numbers
  mutate_at(c('year', 'DDD_1000_day'), as.numeric) %>% 
  
  # bar plot, 'x' is year, 'y' is DDD/1000inhabitants/day, filled the bar by medication type
  ggplot(aes(x = year, y = DDD_1000_day, fill = name)) +
    geom_bar(stat = 'identity',
             width = 0.6) +
  
  # THEME parameters
  # Use a theme template to that looks similar to what I want
  theme_fivethirtyeight() +
  
  # Allow x and y label text
  theme(axis.title = element_text(),
        
  # Make the legends listing down,
  legend.direction = 'vertical',
  
  # Reduce the size of the title      
  plot.title = element_text(size=15),

  # Change the background to white
  plot.background = element_blank(),
  rect = element_rect(fill = '#FFFFFF'),
  
  #Remove the vertical lines in the plot
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank()) +
  
  # NON-THEME paremetres
  # Add the title, remove x label and change y to DDD/1000/day
    labs(title = 'Consumption of antidepressant in Estonia',
         x = element_blank(),
         y = 'DDD/1000/day',
         fill = element_blank()) +
  
  # Change the colour to match the baltic depressant study
  scale_fill_manual(values = c("#C45926", "#F9CBAC", "#ACB6C8"))


# save the previous plot
ragg::agg_png("antidepressant consumption in Estonia.png", width = 10, height = 10, units = "in", res = 300, scaling = 2)
antidepressant_consumption_rate_plot
dev.off()



# Create a dataset with only the antidepressant data
antidepressant_data <- n_medication %>%
  
  # Keep only N06AA, N06AB and N06AX medications
  filter(atc_code == 'N06AA' | atc_code == 'N06AB' | atc_code == 'N06AX') %>%
  
  # Pivot the years into a single column
  # add the values to a DDD/1000inhabitants/day column
  pivot_longer('2017':'2021',
               names_to = 'year',
               values_to = 'DDD_1000_day') %>%
  
  # Change the years and DDD_1000_day as numbers
  mutate_at(c('year', 'DDD_1000_day'), as.numeric)
  

# Use a bar chart to show differences in consumption of antidepressant use between
# 2017 vs 2022
antidepressant_type_2017vs2021 <- antidepressant_data %>%
  
  # Keep only 2017 and 2021 data
  filter(year == 2017 | year == 2021) %>%
  
  # X is the name of the drug, y is the percentage of usage compared to all other antidepressants
  ggplot(aes(x = name, y = DDD_1000_day)) +
  
  # year needs to be changed to character so it's a categorical value instead of numeric
  geom_bar(aes(fill = as.character(year)), 
           position = "dodge", 
           stat="identity",
           width = 0.6) +
  
  # THEME parameters
  # Use a theme template to that looks similar to what I want
  theme_fivethirtyeight()  +
  
  # Allow x and y label text
  theme(axis.title = element_text(),
        
        # Reduce the size of the title      
        plot.title = element_text(size=15),
        
        # Change the background to white
        plot.background = element_blank(),
        rect = element_rect(fill = '#FFFFFF'),
        
        # Remove the vertical lines in the plot
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  
  # NON-THEME paremetres
  # Add the title, remove x label and change y to DDD/1000/day
  labs(title = 'Antidepressant consumption between 2017 and 2021',
       x = element_blank(),
       y = 'DDD/1000/day',
       fill = element_blank()) +
  
  # Rename the x axis labels which contains the medication type
  scale_x_discrete(labels = c('non-selective MAOIs', 'Other', 'SSRIs')) +
  
  # Change the colour of the bar to something prettier
  scale_fill_manual(values = c('#422577', '#F193AE'))
  
# save the previous plot
ragg::agg_png("antidepressant type between 2017 vs 2021.png", width = 10, height = 10, units = "in", res = 300, scaling = 1.5)
antidepressant_type_2017vs2021
dev.off()

#################################
# Plotting specific medications #
#################################

# Finding out which specific medication are consumed within each group
specific_antidepressant <- n_medication %>%

  # Keep all subcodes within the N06AA, N06AB and N06AX categories
  filter(str_detect(atc_code, 'N06AA.|N06AB.|N06AX.')) %>%
  
  # Pivot the years into a single column
  # add the values to a DDD/1000inhabitants/day column
  pivot_longer('2017':'2021',
               names_to = 'year',
               values_to = 'DDD_1000_day') %>%
  
  # Change the years and DDD_1000_day as numbers
  mutate_at(c('year', 'DDD_1000_day'), as.numeric)





# Plot a line plot of SSRI antidepressant to observe changes in consumption overtime
ssri_trend_plot <- specific_antidepressant %>%
  filter(str_detect(atc_code, 'N06AB')) %>%
  arrange(DDD_1000_day) %>%
  ggplot(aes(x = year, y = DDD_1000_day, colour = name)) +
  geom_line(size = 1, alpha = 0.8) +
  
  # THEME parameters
  # Use a theme template to that looks similar to what I want
  theme_fivethirtyeight() +

  # Allow x and y label text
  theme(axis.title = element_text(),
      
      # Reduce the size of the title      
      plot.title = element_text(size=15),
      
      # Make the legends listing down, move to the right and remove title
      legend.direction = 'vertical',
      legend.position = 'right',
      legend.title = element_blank(),
      
      # Change the background to white
      plot.background = element_blank(),
      rect = element_rect(fill = '#FFFFFF'),
      
      # Remove the vertical lines in the plot
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank()) +
  
  # NON-THEME paremetres
  # Add the title, remove x label and change y to DDD/1000/day
  labs(title = 'Consumption of SSRI classes',
       x = element_blank(),
       y = 'DDD/1000/day',
       fill = element_blank())

# save the previous plot
ragg::agg_png("ssri consumption in Estonia.png", width = 10, height = 7, units = "in", res = 300, scaling = 2)
ssri_trend_plot
dev.off()



# Plot a line plot of other antidepressant to observe changes in consumption overtime
other_trend_plot <- specific_antidepressant %>%
  filter(str_detect(atc_code, 'N06AX')) %>%
  arrange(DDD_1000_day) %>%
  ggplot(aes(x = year, y = DDD_1000_day, colour = name)) +
  geom_line(size = 1, alpha = 0.8) +
  
  # THEME parameters
  # Use a theme template to that looks similar to what I want
  theme_fivethirtyeight() +
  
  # Allow x and y label text
  theme(axis.title = element_text(),
        
        # Reduce the size of the title      
        plot.title = element_text(size=15),
        
        # Make the legends listing down, move to the right and remove title
        legend.direction = 'vertical',
        legend.position = 'right',
        legend.title = element_blank(),
        
        # Change the background to white
        plot.background = element_blank(),
        rect = element_rect(fill = '#FFFFFF'),
        
        # Remove the vertical lines in the plot
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  
  # NON-THEME paremetres
  # Add the title, remove x label and change y to DDD/1000/day
  labs(title = 'Consumption of other classes',
       x = element_blank(),
       y = 'DDD/1000/day',
       fill = element_blank())

# save the previous plot
ragg::agg_png("other antidepressant consumption in Estonia.png", width = 10, height = 7, units = "in", res = 300, scaling = 2)
other_trend_plot
dev.off()
  
  
  
  
  

# Plot a line plot of Non-selective monoamine reuptake inhibitors to observe changes in consumption overtime
maoi_trend_plot <- specific_antidepressant %>%
  filter(str_detect(atc_code, 'N06AA')) %>%
  arrange(DDD_1000_day) %>%
  ggplot(aes(x = year, y = DDD_1000_day, colour = name)) +
  geom_line(size = 1, alpha = 0.8) +
  
  # THEME parameters
  # Use a theme template to that looks similar to what I want
  theme_fivethirtyeight() +
  
  # Allow x and y label text
  theme(axis.title = element_text(),
        
        # Reduce the size of the title      
        plot.title = element_text(size=15),
        
        # Make the legends listing down, move to the right and remove title
        legend.direction = 'vertical',
        legend.position = 'right',
        legend.title = element_blank(),
        
        # Change the background to white
        plot.background = element_blank(),
        rect = element_rect(fill = '#FFFFFF'),
        
        # Remove the vertical lines in the plot
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  
  # NON-THEME paremetres
  # Add the title, remove x label and change y to DDD/1000/day
  labs(title = 'Consumption of MAOIs non-selective',
       x = element_blank(),
       y = 'DDD/1000/day',
       fill = element_blank())


# save the previous plot
ragg::agg_png("MAOIs non selective consumption in Estonia.png", width = 10, height = 7, units = "in", res = 300, scaling = 2)
maoi_trend_plot
dev.off()
