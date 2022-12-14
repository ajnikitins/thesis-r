library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(zoo)
library(RcppRoll)
library(ggpubr)
library(ggnewscale)
library(ggsci)
library(ggrepel)

setwd("G:/My Drive/BSc Thesis/Datasets")

#Main dataset | includes first dif
data <- readRDS("data_complete (1).RDS")
#data <- readRDS("data/data_complete.RDS")
summary(data)

data <- data %>% 
  mutate(date = floor_date(as_date(date), unit = "day"))

#Deseasoned dataset | includes deseasoned first dif
data_des <- readRDS("data_deseasoned.RDS")
#data <- readRDS("data/data_complete.RDS")
summary(data_des)

data_des <- data_des %>% 
  mutate(date = floor_date(as_date(date), unit = "day"))

#creating important events variables
events <- read_xlsx("important_events.xlsx")
#events <- read_xlsx("data/important_events.xlsx")
events <- events %>%
  mutate(date = floor_date(as_date(date), unit = "day")) %>%
  mutate(coloring = as.factor(coloring)) %>%
  filter(event_name != "N/A")

#dataset with 7 day moving averages
data_k7 <- data %>%
  group_by(type) %>%
  mutate(don_count = log(rollmean(don_count, k = 7, fill = NA)),
         don_mean_usd = log(rollmean(don_mean_usd, k = 7, fill = NA)),
         tweet_count = log(rollmean(tweet_count, k = 7, fill = NA)),
         siren_count = log(rollmean(siren_count, k = 7, fill = NA)))

#dataset for plotting all-time donations
data_k7_sum <- data_k7 %>%
  group_by(date) %>%
  summarise(don_count = sum(don_count), don_mean_usd = sum(don_count * don_mean_usd) / sum(don_count), siren_count = unique(siren_count), tweet_count = unique(tweet_count))

data_des_sum <- data_des %>% 
  group_by(date) %>%
  summarise(d_don_count = sum(d_don_count), d_don_mean_usd = sum(d_don_count * d_don_mean_usd) / sum(d_don_count), d_siren_count = unique(d_siren_count), d_tweet_count = unique(d_tweet_count))

#Create annotations
annotate1 <- data_des %>% 
  select(d_don_count, d_don_mean_usd, don_count, don_mean_usd, type, date) %>% 
  left_join (events, by = "date")

annotate2 <- data_k7_sum %>% 
  select(don_count, don_mean_usd, date) %>% 
  left_join (events, by = "date")

annotate2_des <- data_des_sum %>% 
  select(d_don_count, d_don_mean_usd, date) %>% 
  left_join (events, by = "date")

#RQ1: relations between emotion intensity (geolocated tweet count for world,
#air sirens for UAH) and donation count/value
#RQ2: relationships between different emotion types (events) and donation count/value
#RQ3: does intensity explain heterogeneity between donor types? (UAH, foreign, crypto)

########################################################################
#simple plots of counts and values by type

create_distrib <- \(.data, var_dep, ylab = "") {
  .data %>%
    filter(date <= ymd("2022-10-31") & date >= ymd("2022-02-10")) %>%
    ggplot(aes(x = date, y =  log({ {var_dep} } ) - log(lag({ {var_dep} })))) +
    geom_line(aes(color = type), linewidth = 1) +
    scale_x_date(date_labels = "%d/%m", date_breaks = "1 month") +
    scale_color_uchicago() +
    theme_classic() +
    theme(legend.spacing.y = unit(0, "mm"), 
          legend.position = "bottom",
          panel.border = element_rect(color = "black", linewidth = 0.2, fill=NA),
          panel.grid.major.x = element_line(color="light grey", size =0.1),
          axis.text.x = element_text(angle = 45, hjust=1, colour = "black", size = 9),
          axis.text.y = element_text(hjust=1, colour = "black", size = 9),
          axis.title = element_text(face="bold"))  +
    xlab(NULL) +
    ylab(ylab) +
    #facet_wrap(~type, scale = "free") +
    NULL
}


#plot of counts by type \ mov avg
a <- data_k7 %>%
  create_distrib(don_count,"Donation count") 
#add events
a + new_scale_color() +
  geom_vline(data = events, aes(xintercept = date, color = coloring), linewidth = 2, alpha = 0.5, show.legend = FALSE) + 
  geom_label_repel(data=annotate2, 
                   aes(label=event_name), size = 2,
                   box.padding = unit(1, "lines"),
                   direction = "x",
                   segment.color = "grey",
                   nudge_x= 3,
                   nudge_y = 1.5) +
  scale_color_manual(values = c(`1` = "red", `0` = "dark green")) 

#plot of counts by type \ des (absolute values)
data_des %>%
  create_distrib(d_don_count,"Donation count, deseasonalised") + facet_wrap(~type, scale = "free")

#plot of values by type \ mov avg
b <- data_k7 %>%
  create_distrib(don_mean_usd,"Donation mean value")
#add events
b + new_scale_color() +
  geom_vline(data = events, aes(xintercept = date, color = coloring), linewidth = 2, alpha = 0.5, show.legend = FALSE) + 
  geom_label_repel(data=annotate2, 
                   aes(label=event_name), size = 2,
                   box.padding = unit(1, "lines"),
                   direction = "x",
                   segment.color = "grey",
                   nudge_x= 3,
                   nudge_y = 1.5) +
  scale_color_manual(values = c(`1` = "red", `0` = "dark green"))

#plot of values by type \ des (absolute values)
data_des %>%
  create_distrib(d_don_mean_usd,"Donation mean value, deseasonalised") + facet_wrap(~type, scale = "free")

#Merge a & b
count_value <- ggarrange(a, b, ncol = 2, nrow = 1, legend = "bottom", common.legend = TRUE)
count_value 

##########################################################################
#Scatterplots
create_scatterplot <- \(.data, var_dep, var_ind, xlab = "", ylab = "") {
  .data %>%
    filter(don_count>0) %>%
    filter(date > ymd("2022-02-10")) %>%
    ggplot(aes(x = { {var_ind} }, y = { {var_dep} })) +
    geom_point(aes(color = type), alpha = 0.5) +
    geom_smooth(aes(color = type), method="lm",se=FALSE) +
    scale_color_uchicago() +
    xlab(xlab) +
    ylab(ylab) +
    theme_classic() +
    theme(legend.spacing.y = unit(0, "mm"), 
          legend.position = "none",
          panel.border = element_rect(color = "black", linewidth = 0.2, fill=NA),
          panel.grid.major = element_blank(),
          axis.text = element_text(colour = "black", size = 9),
          axis.title.y = element_text(face="bold", size = 10.2),
          axis.title.x = element_text(face="bold", size = 13)) +
    facet_wrap(~type, scales="free") +
    NULL
}

#scatterplot count | tweets | type
f <- data %>%
  create_scatterplot(log(don_count), log(tweet_count),"", "log Donation count")
f

#scatterplot count | tweets | type
F <- data_des %>%
  create_scatterplot(d_don_count, d_tweet_count,"", "Δ Donation count, deseasonalised")
F 


#scatterplot count | sirens | type
#data %>%
#  filter(siren_count>0) %>%
#  create_scatterplot(log(don_count), log(siren_count),"Air raid sirens count", "Donation count")

#scatterplot count | sirens proportion | type
#data %>%
#  create_scatterplot(log(don_count), siren_prop,"Territory affected", "Donation mean value")

#scatterplot value | tweet | type
g <- data %>%
  create_scatterplot(log(don_mean_usd), log(tweet_count),"log Tweets count", "log Donation mean value")
g

G <- data_des %>%
  create_scatterplot(d_don_mean_usd, d_tweet_count,"Δ Tweets count", "Δ Donation mean value, deseasonalised")
G 

#scatterplot value | sirens | type
#data %>%
#  filter(siren_count>0) %>% 
#  create_scatterplot(log(don_mean_usd), log(siren_count),"Air raid sirens count", "Donation mean value, by type")

#scatterplot value | sirens proportion | type
#data %>%
#  create_scatterplot(log(don_mean_usd), siren_prop,"Territory affected", "Donation mean value, by type")

#Merge f & g
scatter_count_value <- ggarrange(F, G, ncol = 1, nrow = 2, legend = "none")
scatter_count_value

############################################################################################
#Close-up graphs

create_closeup <- \(.data, var_dep, ylab = "") {
  .data %>%
    filter(date <= ymd("2022-10-17") & date >= ymd("2022-10-01")) %>%
    ggplot(aes(x = date, y = { {var_dep} } )) +
    geom_line(aes(color=type), linewidth = 1) +
    scale_color_uchicago() +
    new_scale_color() +
    geom_vline(data = events, aes(xintercept = date, color = coloring), linewidth = 1, alpha = 0.5) +
    geom_label_repel(data=filter(annotate1, event_name == "Kerch bridge explosion" | event_name == "Nationwide missile strikes"), 
                     aes(label=event_name), size = 4,
                     box.padding = unit(1, "lines"),
                     direction = "x",
                     segment.color = "grey",
                     nudge_x = 12,
                     nudge_y = 12) +
    scale_color_manual(values = c(`1` = "red", `0` = "dark green"), guide = "none") +
    ylab(ylab) +
    xlab(NULL) +
    scale_x_date(date_labels = "%d/%m", date_breaks = "3 days") +
    theme_classic() +
    theme(legend.spacing.y = unit(0, "mm"), 
          legend.position = "none",
          panel.border = element_rect(color = "black", linewidth = 0.2, fill=NA),
          panel.grid.major.x = element_line(color="light grey", size =0.1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1, colour = "black", size = 9),
          axis.text.y = element_text(hjust=1.1, colour = "black", size = 9),
          axis.title = element_text(face="bold", size=16)) +
   facet_wrap(~type, scales="free") +
    NULL
}

#count | des absolute values
T <- data_des %>%
  #filter(type=="Ukrainian") %>% 
  create_closeup(don_count,"Donation count, deseasonalised") 
T 

#value | des absolute values
P <- data_des %>%
#filter(type=="Ukrainian") %>% 
  create_closeup(don_mean_usd,"Donation mean value, deseasonalised")
P

#count | des first difference
t <- data_des %>%
  #filter(type=="Ukrainian") %>% 
  create_closeup(d_don_count,"Δ Donation count, deseasonalised") 
t 


#value | des first difference
p <- data_des %>%
  #filter(type=="Ukrainian") %>% 
  create_closeup(d_don_mean_usd,"Δ Donation mean value, deseasonalised")
p

#merged
#oct_closeup <- ggarrange(T, P, ncol = 1, nrow = 2, legend = "bottom", common.legend = TRUE)
#oct_closeup


##########################################################################################################
#Plotting events against total donations and explanatory variables

create_overlay <- \(.data, var_dep, ylab = "") {
  .data %>%
    ggplot(aes(x = date, y = { { var_dep } })) +
    geom_line(linewidth=0.8) +
    new_scale_color() +
    geom_vline(data = events, aes(xintercept = date, color = coloring), size = 2, alpha = 0.5) +
    geom_label_repel(data=annotate2, 
                     aes(label=event_name), size = 5,
                     box.padding = unit(1, "lines"),
                     direction = "both",
                     segment.color = "grey",
                     nudge_x= 10,
                     nudge_y = 10) +
    scale_color_manual(values = c(`1` = "red", `0` = "dark green")) +
    ylab(ylab) +
    xlab(NULL) +
    xlim("2022-02-10", "2022-10-31") +
    scale_x_date(date_labels = "%d/%m", date_breaks = "1 month", minor_breaks = "2 weeks") +
    theme_classic() +
    theme(legend.spacing.y = unit(0, "mm"), 
          legend.position = "none",
          panel.border = element_rect(color = "black", linewidth = 0.2, fill=NA),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1, colour = "black", size = 9),
          axis.text.y = element_text(hjust=1, colour = "black", size = 7),
          axis.title = element_text(face="bold", size = 20)) +
    NULL
}


#total count | events | tweet count
data_k7_sum %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_overlay(don_count, "log of Total donation count") +
  #geom_line(aes(y = d_tweet_count), linewidth = 0.8, color = "brown") +
  NULL


# value across types | events | tweet count
data_k7_sum %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_overlay(don_mean_usd, " log of Mean donation value") +
  #geom_line(aes(y = d_tweet_count),linewidth = 0.8, color = "brown")+
  NULL


