library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(RcppRoll)
library(ggpubr)
library(ggnewscale)

# setwd("G:/My Drive/BSc Thesis")

# data <- readRDS("data_complete.RDS")
data <- readRDS("data/data_complete.RDS")
summary(data)


#RQ1: relations between emotion intensity (geolocated tweet count for world,
#air sirens for UAH) and donation count/value
#RQ2: relationships between different emotion types (events) and donation count/value
#RQ3: does sentiment explain heterogeneity between donor types? (UAH, foreign, crypto)

#creating important events variable
# events <- read_xlsx("important_events.xlsx")
events <- read_xlsx("data/important_events.xlsx")
events <- events %>%
  mutate(date = floor_date(as_date(date), unit = "day")) %>%
  mutate(coloring = as.factor(coloring)) %>%
  filter(event_name != "N/A")

#dataset with 7 days window (for full timeline)

data_k7 <- data %>%
  group_by(type) %>%
  mutate(don_count = log(rollmean(don_count, k = 7, fill = NA)),
         don_mean_usd = log(rollmean(don_mean_usd, k = 7, fill = NA)),
         tweet_count = log(rollmean(tweet_count, k = 7, fill = NA)),
         siren_count = log(rollmean(siren_count, k = 7, fill = NA)))

#dataset with 2 day window (for more zoomed-in graphs)
#### !!!!! I set it to 1; is there a point for a 2-day window?
data_k2 <- data %>%
  group_by(type) %>%
  mutate(don_count = log(rollmean(don_count, k = 1, fill = NA)),
         don_mean_usd = log(rollmean(don_mean_usd, k = 1, fill = NA)),
         tweet_count = log(rollmean(tweet_count, k = 1, fill = NA)),
         siren_count = log(rollmean(siren_count, k = 1, fill = NA)))

#back-up dataset for plotting w/o moving averages
data_plots <- data %>%
  group_by(type)

########################################################################
#plot of counts by type
data_k7 %>%
  filter(date <= ymd("2022-10-31") & date >= ymd("2022-02-10")) %>%
  ggplot(aes(x = date, y = don_count)) +
  geom_line(aes(color = type)) +
  xlab("Time") +
  ylab("log of Donation count, by type") +
  NULL

#plot of values by type
data_k7 %>%
  filter(date <= ymd("2022-10-31") & date >= ymd("2022-02-10")) %>%
  ggplot(aes(x = date, y = don_mean_usd)) +
  geom_line(aes(color = type)) +
  xlab("Time") +
  ylab("log of Average donation, by type") +
  NULL

##########################################################################
create_scatterplot <- \(.data, var_dep, var_ind, xlab = "", ylab = "") {
  .data %>%
    filter(don_count>0) %>%
    filter(date >= ymd("2022-02-10")) %>%
    ggplot(aes(x = { { var_ind } }, y = { {var_dep} } )) +
    geom_point(aes(color = type, alpha = 0.5)) +
    geom_smooth(aes(color=type), method="lm",se=FALSE) +
    xlab(xlab) +
    ylab(ylab) +
    facet_wrap(~type, scales="free") +
    theme(legend.position = "none") +
    NULL
}

#scatterplot count | tweets | by type

data %>%
  create_scatterplot(log(don_count), log(tweet_count),"Tweets count", "Donation count, by type")

#scatterplot count | sirens | by type
data %>%
  filter(siren_count>0) %>%
  create_scatterplot(log(don_count), log(siren_count),"Air raid sirens count", "Donation count, by type")

#scatterplot value | tweet | by type
data %>%
  create_scatterplot(log(don_mean_usd), log(tweet_count),"Tweets count", "Donation mean value, by type")

#scatterplot value | sirens | type
data %>%
  filter(siren_count>0) %>%
  create_scatterplot(log(don_count), log(siren_count),"Air raid sirens count", "Donation mean value, by type")

#!!!!! Change colours, add labels (?)

# donations count | important events (by type)
A <- data_k7 %>%
  filter(date >= ymd("2022-02-10")) %>%
  ggplot(aes(x = date, y = don_count)) +
  geom_line() +
  geom_vline(data = events, aes(xintercept = date, color = coloring), size = 1, alpha = 0.5) +
  scale_color_manual(values = c(`1` = "red", `0` = "turquoise")) +
  xlab("Time") +
  ylab("Donation count, by type") +
  facet_wrap(~type) +
  theme(legend.position = "none") +
  NULL

# donations count | important events | close-up (by type)
B <- data_res %>%
  filter(date <= ymd("2022-05-02") & date >= ymd("2022-04-18")) %>%
  ggplot(aes(x = date, y = don_count_des)) +
  geom_line(aes(color=type)) +
  new_scale_color() +
  geom_vline(data = events, aes(xintercept = date, color = coloring), size = 1, alpha = 0.5) +
  scale_color_manual(values = c(`1` = "red", `0` = "turquoise"), guide = "none") +
  xlab("Time") +
  ylab("Donation count, by type") +
  theme(legend.position = "none") +
  NULL

# donations value | important events (by type)
C <- data_k7 %>%
  filter(date >= ymd("2022-02-10")) %>%
  ggplot(aes(x = date, y = don_mean_usd)) +
  geom_line() +
  geom_vline(data = events, aes(xintercept = date, color = coloring), size = 1, alpha = 0.5) +
  scale_color_manual(values = c(`1` = "red", `0` = "turquoise")) +
  xlab("Time") +
  ylab("Average donation, by type") +
  facet_wrap(~type) +
  theme(legend.position = "none") +
  NULL

# donations value | important events | close-up (by type)
D <- data_res %>%
  filter(date <= ymd("2022-05-02") & date >= ymd("2022-04-18")) %>%
  ggplot(aes(x = date, y = don_mean_usd_des)) +
  geom_line(aes(color=type)) +
  new_scale_color() +
  geom_vline(data = events, aes(xintercept = date, color = coloring), size = 1, alpha = 0.5) +
  scale_color_manual(values = c(`1` = "red", `0` = "turquoise"), guide = "none") +
  xlab("Time") +
  ylab("Average donation, by type") +
  # theme(legend.position = "noen") +
  NULL

#Merge together

plot_events <- ggarrange(A, C, ncol = 1, nrow = 2)
plot_events

plot_closeup <- ggarrange(B, D, ncol = 2, nrow = 1, legend = "bottom", common.legend = TRUE)
plot_closeup


#using regression residuals to deseasonalise

#create weekday dummies
data_dummies <- data %>%
  filter(date >= ymd("2022-02-10")) %>%
  mutate(weekday = weekdays(date)) %>%
  mutate (monday = ifelse(weekday=="Monday",1,0)) %>%
  mutate (tuesday = ifelse(weekday=="Tuesday",1,0)) %>%
  mutate (wednesday = ifelse(weekday=="Wednesday",1,0)) %>%
  mutate (thursday = ifelse(weekday=="Thursday",1,0)) %>%
  mutate (friday = ifelse(weekday=="Friday",1,0)) %>%
  mutate (saturday = ifelse(weekday=="Saturday",1,0)) %>%
  mutate (sunday = ifelse(weekday=="Sunday",1,0)) %>%
  select(-starts_with("siren"), -tweet_count, -factiva_count, -weekday) %>%
  pivot_wider(names_from = type, values_from = c(don_count, don_mean_usd)) %>%
  filter()

#obtain residuals
mod_don_count_des <- data_dummies %>%
  lm(cbind(don_count_Ukrainian, don_count_Foreign, don_count_Crypto) ~ monday + tuesday + wednesday + thursday + friday + saturday, data=.)
summary(mod_don_count_des)

mod_don_mean_usd_des <- data_dummies %>%
  lm(cbind(don_mean_usd_Ukrainian, don_mean_usd_Foreign, don_mean_usd_Crypto) ~ monday + tuesday + wednesday + thursday + friday + saturday, data=.)
summary(mod_don_mean_usd_des)

## Deseasonalised counts
# Ukrainian
mod_don_count_des$fitted.values %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = don_count_Ukrainian)) + geom_line()

mod_don_count_des$residuals %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = don_count_Ukrainian)) + geom_line()

# Foreign
mod_don_count_des$fitted.values %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = don_count_Foreign)) + geom_line()

mod_don_count_des$residuals %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = don_count_Foreign)) + geom_line()

# Crypto donations
mod_don_count_des$fitted.values %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = don_count_Crypto)) + geom_line()

mod_don_count_des$residuals %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = don_count_Crypto)) + geom_line()

#add residuals to original dataframe
#data$don_count_des <- abs(don_count_des$residuals)
#data$don_mean_usd_des <- abs(don_mean_usd_des$residuals)

data_res <- mod_don_count_des$residuals %>%
  as.data.frame() %>%
  mutate(date = data_dummies$date) %>%
  pivot_longer(-date, names_to = "type", values_to = "don_count_des", names_prefix = "don_count_") %>%
  left_join(data, by = c("date", "type"))

data_res <- mod_don_mean_usd_des$residuals %>%
  as.data.frame() %>%
  mutate(date = data_dummies$date) %>%
  pivot_longer(-date, names_to = "type", values_to = "don_mean_usd_des", names_prefix = "don_mean_usd_") %>%
  left_join(data_res, by = c("date", "type"))

#plot to check
data_res %>%
  ggplot(aes(x=date, y=don_count_des)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y")

data %>%
  ggplot(aes(x=date, y=don_count)) +
  geom_line()

#save to original dataset
saveRDS(data, file="data_complete.RDS")


####################################################################
#RQ1################################################################
####################################################################

create_event_plot <- \(.data, var_dep, var_ind, ylab = "") {
  .data %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = {{var_dep}})) +
    geom_line(aes(y = { { var_ind } }), color = "green") +
    geom_vline(data = events, aes(xintercept = date, color = coloring), size = 2, alpha = 0.5) +
    scale_color_manual(values = c(`1` = "red", `0` = "green")) +
    xlab("Time") +
    ylab(ylab) +
    theme(legend.position = "none") +
    NULL
}

#donation *count for Ukrainian type against important events & air raid siren count
#full
data_k7 %>%
  filter(type == "Ukrainian") %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_event_plot(don_count, siren_count, "n Ukrainian donations, air raid sirens")

#April only
data_k2 %>%
  filter(type == "Ukrainian") %>%
  filter(date <= ymd("2022-04-30") & date >= ymd("2022-04-01")) %>%
  create_event_plot(don_count, siren_count, "n Ukrainian donations, air raid sirens")

#donation *count for Foreign type against important events & tweet count (total)
data_k7 %>%
  filter(type == "Foreign") %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_event_plot(don_count, tweet_count, "n Foreign donations, tweets")

#donation *count for Crypto type against important events & tweet count (total)
data_k7 %>%
  filter(type == "Crypto") %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_event_plot(don_count, tweet_count, "n Crypto donations, tweets")

####################################################################

#donation *value for Ukrainian type against important events & air raid siren count
#full
data_k7 %>%
  filter(type == "Ukrainian") %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_event_plot(don_mean_usd, siren_count, "mean Ukrainian donations, air raid sirens")

#April only
data_k2 %>%
  filter(type == "Ukrainian") %>%
  filter(date <= ymd("2022-04-30") & date >= ymd("2022-04-01")) %>%
  create_event_plot(don_mean_usd, siren_count, "mean Ukrainian donations, air raid sirens")

#donation *value for Foreign type against important events & tweet count (total)
data_k7 %>%
  filter(type == "Foreign") %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_event_plot(don_mean_usd, tweet_count, "mean Foreign donations, air raid sirens")

#donation *value for Crypto type against important events & tweet count (total)
data_k7 %>%
  filter(type == "Crypto") %>%
  create_event_plot(don_mean_usd, tweet_count, "mean Crypto donations, air raid sirens")

####################################################################
#RQ2################################################################
####################################################################

create_emotion_plot <- \(.data, var_dep, ylab = "") {
  .data %>%
    ggplot(aes(x = date, y = { { var_dep } })) +
    geom_line() +
    geom_vline(data = events, aes(xintercept = date, color = coloring), size = 2, alpha = 0.5) +
    scale_color_manual(values = c(`1` = "red", `0` = "turquoise")) +
    xlab("Time") +
    ylab(ylab) +
    theme(legend.position = "none")
}

data_k7_sum <- data_k7 %>%
  group_by(date) %>%
  summarise(don_count = sum(don_count), don_mean_usd = sum(don_count * don_mean_usd) / sum(don_count), siren_count = unique(siren_count), tweet_count = unique(tweet_count))

#all time donation *count against important events, by emotion (blue - neg, red - pos) + tweet_count in blue
data_k7_sum %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_emotion_plot(don_count, "log of Total donation count") +
  geom_line(aes(y = siren_count), color = "brown")

#all time donation *count against important events, by emotion (blue - neg, red - pos) + tweet_count in blue
data_k7_sum %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_emotion_plot(don_count, "log of Total donation count") +
  geom_line(aes(y = tweet_count), color = "blue")

#Both positive and negative events immediately increase donation count?

#all time donation *value against important events, by emotion
data_k7_sum %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_emotion_plot(don_mean_usd, "log of Average donation")

#Positive events cause decrease in value, negative events cause increase in value?

###################################################################################################

#How is this not denoised????????????????????????? aaa

data_k2_sum <- data_k2 %>%
  group_by(date) %>%
  summarise(don_count = sum(don_count), don_mean_usd = sum(don_count * don_mean_usd) / sum(don_count))

# donation *count around Kerch (positive) & response strikes (negative)
data_k2_sum %>%
  filter(date <= ymd("2022-10-20") & date >= ymd("2022-09-29")) %>%
  create_emotion_plot(don_count, "log(Donation count)")

# donation *count around Vinnitsya (negative)
data_k2_sum %>%
  filter(date <= ymd("2022-07-25") & date >= ymd("2022-07-04")) %>%
  create_emotion_plot(don_count, "log(Donation count)")

# donation *value around Kerch (positive) & response strikes (negative)
data_k2_sum %>%
  filter(date <= ymd("2022-10-20") & date >= ymd("2022-09-29")) %>%
  create_emotion_plot(don_mean_usd, "log(Average donation)")

# donation *value around Vinnitsya (negative)
data_k2_sum %>%
  filter(date <= ymd("2022-07-25") & date >= ymd("2022-07-04")) %>%
  create_emotion_plot(don_mean_usd, "log(Average donation)")

####################################################################
#RQ3################################################################
####################################################################

create_emotion_plot_type <- \(.data, var_dep, ylab = "") {
  .data %>%
    ggplot(aes(x = date, y = { { var_dep } })) +
    geom_line() +
    geom_vline(data = events, aes(xintercept = date, color = coloring), size = 2, alpha = 0.5) +
    scale_color_manual(values = c(`1` = "red", `0` = "green")) +
    xlab("Time") +
    ylab(ylab) +
    facet_wrap(~ type, ncol = 1) +
    theme(legend.position = "none")
}

#all time donation *count against important events, *positive emotion, by type
data_k7 %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_emotion_plot_type(don_count, "log of Donation count, by type")

#all time donation *value against important events, *negative emotion, by type
data_k7 %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_emotion_plot_type(don_mean_usd, "log of Average donation, by type")

