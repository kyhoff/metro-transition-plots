## ---------------------------------------------------------
## individual_top_20_metro_plots.R
##
## Title: 1990-2020 plots for Individual Top 20 Metros
## Purpose: Create transition plots to show decadal changes
##          in racial diversity and segregatino in the top 20
##          biggest metropoltian areas in the United States   
## Author: Kylee Hoffman
## Date: 6/14/2022
## ---------------------------------------------------------

library(plotly)
library(tidyverse)
library(dplyr)

dataDir <- "~/Desktop/Projects/MixedMetro_viz/largest_100_MSA_transitions copy"

order <- c("LDW", "MDW",
           "LDL", "MDL",
           "LDB", "MDB",
           "LDA", "MDA",
           "LDNAm", "MDNAm",
           "LDO", "MDO",
           "HD")


# Cleans up the csv files and adds a color column.
df_manip <-  function(matrix) {
  data <-  read.csv(file.path(dataDir, matrix)) %>%
    filter(`X` != "sums") %>%
    select(everything(), -c("sums")) %>%
    arrange(match(X, order))
  
  data$col <-  c("rgba(255,153,0,0.6)", #LDW
                 "rgba(255,204,153,0.6)", #MDW
                 "rgba(153,102,255,0.6)", #LDL
                 "rgba(204,153,255,0.6)", #MDL
                 "rgba(102,204,0,0.6)", #LDB
                 "rgba(153,255,153,0.6)", #MDB
                 "rgba(255,102,102,0.6)", #LDA
                 "rgba(255,153,153,0.6)", #MDA
                 "rgba(255,255,0,0.6)", #LDNAm
                 "rgba(255,255,153,0.6)", #MDNAm
                 "rgba(119,119,119,0.6)", #LDO
                 "rgba(204,204,204,0.6)", #MDO
                 "rgba(153,117,46,0.6)") #HD
  
  df <-  as.data.frame(pivot_longer(data, cols = 2:14)) %>%
    filter(`value` != 0)
  
  colnames(df) <- c('source','color', 'target','value')
  
  return(df)
}


# Sets indices for all dataframes. The parameters are easily changeable for every decade's necessary indices.
set_indices <- function(data,
                       LDW_s,MDW_s,LDL_s,MDL_s,LDB_s,MDB_s,LDA_s,MDA_s,LDNAm_s,MDNAm_s,LDO_s,MDO_s,HD_s,
                       LDW_t,MDW_t,LDL_t,MDL_t,LDB_t,MDB_t,LDA_t,MDA_t,LDNAm_t,MDNAm_t,LDO_t,MDO_t,HD_t) {
  df <-  df_manip(data)
  
  df$source_idx <- with(df, 
                       ifelse(source == "LDW", LDW_s, 
                       ifelse(source == "MDW", MDW_s,
                       ifelse(source == "LDL", LDL_s,
                       ifelse(source == "MDL", MDL_s,
                       ifelse(source == "LDB", LDB_s,
                       ifelse(source == "MDB", MDB_s,
                       ifelse(source == "LDA", LDA_s,
                       ifelse(source == "MDA", MDA_s,
                       ifelse(source == "LDO", LDO_s,
                       ifelse(source == "MDO", MDO_s,
                       ifelse(source == "LDNAm", LDNAm_s,
                       ifelse(source == "MDNAm", MDNAm_s,
                       ifelse(source == "HD", HD_s, NA
                    ))))))))))))))
                  
                  
df$target_idx <- with(df, 
                        ifelse(target == "LDW", LDW_t, 
                       ifelse(target == "MDW", MDW_t,
                       ifelse(target == "LDL", LDL_t,
                       ifelse(target == "MDL", MDL_t,
                       ifelse(target == "LDB", LDB_t,
                       ifelse(target == "MDB", MDB_t,
                       ifelse(target == "LDA", LDA_t,
                       ifelse(target == "MDA", MDA_t,
                       ifelse(target == "LDO", LDO_t,
                       ifelse(target == "MDO", MDO_t,
                       ifelse(target == "LDNAm", LDNAm_t,
                       ifelse(target == "MDNAm", MDNAm_t,
                       ifelse(target == "HD", HD_t, NA
                      ))))))))))))))
  
  return(df)
}

indices_90_00 <- function(file) {
  set_indices(file, 0,1,2,3,4,5,6,7,8,9,10,11,12, 13,14,15,16,17,18,19,20,21,22,23,24,25)
}
indices_00_10 <- function(file) {
  set_indices(file, 13,14,15,16,17,18,19,20,21,22,23,24,25, 26,27,28,29,30,31,32,33,34,35,36,37,38)
}
indices_10_20 <- function(file) {
  set_indices(file, 26,27,28,29,30,31,32,33,34,35,36,37,38, 39,40,41,42,43,44,45,46,47,48,49,50,51)
}


# Counts number of unique categories for a given decade to automate the process of creating the correct amount of rows
x_node_count <- function(count1, count2, count3, count4) {
  num1 <- length(unique(count1))
  num2 <- length(unique(count2))
  num3 <- length(unique(count3))
  num4 <- length(unique(count4))
  
  node_loc1 <- c(0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001)
  node_loc2 <- c(0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33,0.33)
  node_loc3 <- c(0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66)
  node_loc4 <- c(1,1,1,1,1,1,1,1,1)
  
  return(c(node_loc1[1:num1], node_loc2[1:num2], node_loc3[1:num3], node_loc4[1:num4]))
}


# This function is similar to the above, but is only used in a few plots that don't require manual positioning
y_node_count <- function(count1, count2, count3, count4) {
  num1 <- length(unique(count1))
  num2 <- length(unique(count2))
  num3 <- length(unique(count3))
  num4 <- length(unique(count4))
  
  node_loc <- c(0.001,0.11,0.22,0.33,0.44,0.5,0.55,0.66,0.77,0.88,0.99)
  
  return(c(node_loc[1:num1 - 1], 0.99, node_loc[1:num2 - 1], 0.99, node_loc[1:num3 - 1], 0.99, node_loc[1:num4 - 1], 0.99))
}


# General plotting parameters
node_label <- c(LDW = 'LDW', MDW = 'MDW', LDL = 'LDL', MDL = 'MDL',
                LDB = 'LDB', MDB = "MDB", LDA = 'LDA', MDA = 'MDA', 
                LDO = 'LDO', MDO = 'MDO', LDNAm = 'LDNAm', MDNAm = 'MDNAm', HD = 'HD')
mm_palette <-  c(LDW="rgba(255,153,0,0.6)", MDW="rgba(255,204,153,0.6)", LDL="rgba(153,102,255,0.6)", 
                 MDL="rgba(204,153,255, 0.6)", LDB="rgba(102,204,0,0.6)", MDB="rgba(153,255,153,0.6)", 
                 LDA="rgba(255,102,102,0.6)", MDA="rgba(255,153,153,0.6)", LDO="rgba(119,119,119,0.6)",
                 MDO="rgba(204,204,204,0.6)", LDNAm="rgba(255,255,0,0.6)",
                 MDNAm = "rgba(255,255,153,0.6)", HD="rgba(153,117,46, 0.6)")

m <- list(l = 10,
          r = 10,
          b = 15,
          t = 40,
          pad = 4)

t <- list(
  family = "Lato",
  size = 15,
  color = 'black')



plotter = function(metros_90_00, metros_00_10, metros_10_20, x_node, y_node, title) {
  source = c(metros_90_00$source_idx, metros_00_10$source_idx, metros_10_20$source_idx)
  
  target = c(metros_90_00$target_idx, metros_00_10$target_idx, metros_10_20$target_idx)
  
  value = c(metros_90_00$value, metros_00_10$value, metros_10_20$value)
  
  color = c(metros_90_00$color, metros_00_10$color, metros_10_20$color)
  
  p <- plotly::plot_ly(type = "sankey", 
                       arrangement = "snap",
                       textfont = list(size=10),
                       node = list(
                         label = c(node_label, node_label, node_label, node_label),
                         x = x_node,
                         y = y_node,
                         pad = 25,
                         thickness = 20,
                         color = c(mm_palette, mm_palette, mm_palette, mm_palette)),  
                       link = list(
                         source = source,
                         target = target,
                         value = value,
                         color = color)) %>% 
    layout(margin = m, title = title, font = t)
  
  return(p)
}


# Combines every function above to create all plots
create_plot <- function(file_90_00,file_00_10, file_10_20, title, y) {
  df_90_00 <- indices_90_00(file_90_00)
  df_00_10 <- indices_00_10(file_00_10)
  df_10_20 <- indices_10_20(file_10_20)
  
  x_df <- x_node_count(df_90_00$source_idx, df_00_10$source_idx, df_10_20$source_idx, df_10_20$target_idx)
  y_df <- y_node_count(df_90_00$source_idx, df_00_10$source_idx, df_10_20$source_idx, df_10_20$target_idx)
  
  if (is.na(y)) {
    return(plotter(df_90_00, df_00_10, df_10_20, x_df, y_df, title))
  } else {
    return(plotter(df_90_00, df_00_10, df_10_20, x_df, y, title))
  }
}


# New York, NY
create_plot("New York.transition_90_00.csv", "New York.transition_00_10.csv", "New York.transition_10_20.csv", "New York, NY Transition: 90-00-10-20", NA)


# Los Angeles-Long Beach, CA 
create_plot("Los Angeles-Long Beach.transition_90_00.csv", "Los Angeles-Long Beach.transition_00_10.csv", "Los Angeles-Long Beach.transition_10_20.csv", "Los Angeles-Long Beach, CA Transition: 90-00-10-20", NA)


# Chicago, IL
create_plot("Chicago.transition_90_00.csv", "Chicago.transition_00_10.csv", "Chicago.transition_10_20.csv", "Chicago, IL Transition: 90-00-10-20", NA)


# Dallas-Fort Worth-Arlington, TX
dallas_y <- c(0.001,0.22,0.33,0.44,0.55,0.66,0.77,0.88, 0.001,0.33,0.5,0.6,0.77,0.88,0.99, 0.001,0.22,0.33,0.44,0.55,0.66,0.77,0.88, 0.001,0.22,0.33,0.44,0.55,0.66,0.77,0.88)
create_plot("Dallas-Fort Worth-Arlington.transition_90_00.csv", "Dallas-Fort Worth-Arlington.transition_00_10.csv", "Dallas-Fort Worth-Arlington.transition_10_20.csv", "Dallas-Fort Worth-Arlington, TX Transition: 90-00-10-20", dallas_y)


# Philadeplia, PA
phil_y <- c(0.001,0.11,0.22,0.45,0.6,0.8,0.9,0.99, 0.001,0.11,0.22,0.33,0.55,0.66,0.74,0.87,0.99, 0.001,0.22,0.33,0.55,0.66,0.81,0.88,0.99, 0.001,0.18,0.33,0.44,0.66,0.77,0.88,0.99)
create_plot("Philadelphia.transition_90_00.csv", "Philadelphia.transition_00_10.csv", "Philadelphia.transition_10_20.csv", "Philadelphia, PA Transition: 90-00-10-20", phil_y)


# Houston, TX
y_houston <- c(0.001,0.11,0.33,0.44,0.66,0.77,0.88,0.97, 0.001,0.11,0.22,0.55,0.74,0.87,0.99, 0.001,0.22,0.33,0.55,0.66,0.77,0.88,0.99, 0.001,0.18,0.33,0.44,0.66,0.77,0.88,0.97)
create_plot("Houston.transition_90_00.csv", "Houston.transition_00_10.csv", "Houston.transition_10_20.csv", "Houston, CA Transition: 90-00-10-20", y_houston)


# Washington DC, MD/VA/WV
y_dc <- c(0.001,0.37,0.48,0.6,0.81,0.91,0.99, 0.001,0.22,0.52,0.65,0.8,0.9,0.99, 0.001,0.22,0.33,0.44,0.55,0.77,0.9,0.99, 0.001,0.22,0.33,0.44,0.55,0.66,0.84,0.96)
create_plot("Washington DC.transition_90_00.csv", "Washington DC.transition_00_10.csv", "Washington DC.transition_10_20.csv", "Washington DC, MD/VA/WV Transition: 90-00-10-20", y_dc)


# Miami-Fort Lauderdale-Pompano Beach, FL
y_miami <- c(0.001,0.33,0.55,0.74,0.85,0.99, 0.001,0.22,0.44,0.55,0.77,0.88,0.99, 0.001,0.22,0.44,0.55,0.77,0.88,0.99, 0.001,0.11,0.22,0.55,0.66,0.88,0.99)
create_plot("Miami-Fort Lauderdale-Pompano Beach.transition_90_00.csv", "Miami-Fort Lauderdale-Pompano Beach.transition_00_10.csv", "Miami-Fort Lauderdale-Pompano Beach.transition_10_20.csv", "Miami-Fort Lauderdale-Pompano Beach, FL Transition: 90-00-10-20", y_miami)


# Atlanta, GA
y_atl <- c(0.001,0.49,0.7,0.86,0.99, 0.001,0.37,0.54,0.69,0.86,0.99, 0.001,0.22,0.44,0.55,0.69,0.88,0.99, 0.001,0.18,0.33,0.44,0.55,0.66,0.77,0.99)
create_plot("Atlanta.transition_90_00.csv", "Atlanta.transition_00_10.csv", "Atlanta.transition_10_20.csv", "Atlanta, GA Transition: 90-00-10-20", y_atl)


# Boston-Cambridge-Newton, MA-NH
y_boston <- c(0.001,0.22,0.33,0.44,0.55,0.66,0.82,0.90,0.99, 0.001,0.44,0.55,0.68,0.77,0.84,0.91,0.99, 0.001,0.44,0.55,0.68,0.77,0.84,0.91,0.99, 0.001,0.37,0.58,0.68,0.77,0.84,0.91,0.99)
create_plot("Boston-Cambridge-Newton.transition_90_00.csv", "Boston-Cambridge-Newton.transition_00_10.csv", "Boston-Cambridge-Newton.transition_10_20.csv", "Boston-Cambridge-Newton, MA-NH Transition: 90-00-10-20", y_boston)


# San Francisco-Oakland-Fremont, CA
y_sf <- c(0.001,0.13,0.33,0.44,0.55,0.66,0.77,0.95, 0.001,0.13,0.33,0.44,0.55,0.66,0.77,0.95, 0.001,0.22,0.33, 0.44,0.55,0.66,0.77,0.95, 0.001,0.18,0.33,0.44,0.55,0.66,0.77,0.80)
create_plot("San Francisco-Oakland-Fremont.transition_90_00.csv", "San Francisco-Oakland-Fremont.transition_00_10.csv", "San Francisco-Oakland-Fremont.transition_10_20.csv", "San Francisco-Oakland-Fremont, CA Transition: 90-00-10-20", y_sf)


# Detroit, MI
y_detroit <- c(0.001,0.57,0.69,0.83,0.99, 0.001,0.5,0.6,0.72,0.89,0.99, 0.001,0.44,0.55,0.73,0.86,0.99, 0.001,0.33,0.55,0.64,0.72,0.84,0.99)
create_plot("Detroit.transition_90_00.csv", "Detroit.transition_00_10.csv", "Detroit.transition_10_20.csv", "Detroit, MI Transition: 90-00-10-20", y_detroit)


# Riverside-San Bernardino, CA
y_riverside <- c(0.001,0.11,0.33,0.66,0.82,0.99, 0.001,0.22,0.44,0.66,0.77,0.82,0.99, 0.001,0.22,0.44,0.66,0.82,0.99, 0.001,0.11,0.33,0.66,0.82,0.99)
create_plot("Riverside-San Bernardino.transition_90_00.csv", "Riverside-San Bernardino.transition_00_10.csv", "Riverside-San Bernardino.transition_10_20.csv", "Riverside-San Bernardino, CA Transition: 90-00-10-20", y_riverside)


# Phoenix-Mesa, AZ
y_phx <- c(0.001,0.44,0.55,0.73,0.81,0.91,0.99, 0.001,0.11,0.22,0.33,0.55,0.66,0.88,0.99, 0.001,0.22,0.35,0.63,0.84,0.91,0.99, 0.001,0.29,0.54,0.68,0.83,0.92,0.99)
create_plot("Phoenix-Mesa.transition_90_00.csv", "Phoenix-Mesa.transition_00_10.csv", "Phoenix-Mesa.transition_10_20.csv", "Phoenix-Mesa, AZ Transition: 90-00-10-20", y_phx)


# Seattle-Tacoma-Bellevue, WA
y_sea <- c(0.001,0.33,0.66,0.88,0.99, 0.001,0.33,0.66,0.88,0.99, 0.001,0.51,0.84,0.98, 0.001,0.33,0.55,0.66,0.96)
create_plot("Seattle-Tacoma-Bellevue.transition_90_00.csv", "Seattle-Tacoma-Bellevue.transition_00_10.csv", "Seattle-Tacoma-Bellevue.transition_10_20.csv", "Seattle-Tacoma-Bellevue, WA Transition: 90-00-10-20", y_sea)


# Minneapolis-St. Paul, MN
y_minni <- c(0.001,0.55,0.66,0.77,0.82,0.90,0.99, 0.001,0.55,0.82,0.90,0.99, 0.001,0.44,0.55,0.82,0.90,0.99, 0.001,0.22,0.33,0.55,0.66,0.88,0.99)
create_plot("Minneapolis-St. Paul.transition_90_00.csv", "Minneapolis-St. Paul.transition_00_10.csv", "Minneapolis-St. Paul.transition_10_20.csv", "Minneapolis-St. Paul, MN Transition: 90-00-10-20", y_minni)


# San Diego, CA
y_sd <- c(0.001,0.22,0.33,0.55,0.66,0.88,0.99, 0.001,0.22,0.33,0.55,0.66,0.88,0.99, 0.001,0.22,0.33,0.66,0.88,0.99, 0.001,0.22,0.33,0.66,0.88,0.990)
create_plot("San Diego.transition_90_00.csv", "San Diego.transition_00_10.csv", "San Diego.transition_10_20.csv", "San Diego, CA Transition: 90-00-10-20", y_sd)


# St. Louis, MO/IL
y_sl <- c(0.001,0.62,0.8,0.99, 0.001,0.44,0.66,0.81,0.99, 0.001,0.4,0.64,0.8,0.99, 0.001,0.22,0.33,0.66,0.88,0.99)
create_plot("St. Louis.transition_90_00.csv", "St. Louis.transition_00_10.csv", "St. Louis.transition_10_20.csv", "St. Louis, CA Transition: 90-00-10-20", y_sl)


# Tampa-St. Petersburg-Clearwater, FL
y_tampa <- c(0.001,0.58,0.72,0.85,0.99, 0.001,0.48,0.71,0.85,0.99, 0.001,0.44,0.68,0.83,0.93,0.99, 0.001,0.22,0.33,0.55,0.66,0.88,0.99)
create_plot("Tampa-St. Petersburg-Clearwater.transition_90_00.csv", "Tampa-St. Petersburg-Clearwater.transition_00_10.csv", "Tampa-St. Petersburg-Clearwater.transition_10_20.csv", "Tampa-St. Petersburg-Clearwater, FL Transition: 90-00-10-20", y_tampa)


# Baltimore, MD
y_balt <- c(0.001,0.57,0.73,0.99, 0.001,0.49,0.76,0.99, 0.001,0.22,0.45,0.69,0.88,0.99, 0.001,0.22,0.33,0.55,0.66,0.88,0.99)
create_plot("Baltimore.transition_90_00.csv", "Baltimore.transition_00_10.csv", "Baltimore.transition_10_20.csv", "Baltimore, MD Transition: 90-00-10-20", y_balt)

