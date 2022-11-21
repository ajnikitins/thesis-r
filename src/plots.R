library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(zoo)

data <- readRDS("data/data_complete.RDS")
summary(data)

#RQ1: relationships between emotion intensity (geolocated tweet count for world,
#air sirens for UAH) and donation count/value
#RQ2: relationships between different emotion types (events) and donation count/value
#RQ3: does sentiment explain heterogeneity between donor types? (UAH, foreign, crypto)

# One graph per RQ
# One graph = two plots of value & count of donations + zoomed-in version

#creating important events variable
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

#Would love to have labels on important events smh
#...would take me another million years
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
    scale_color_manual(values = c(`1` = "red", `0` = "green")) +
    xlab("Time") +
    ylab(ylab) +
    theme(legend.position = "none")
}

data_k7_sum <- data_k7 %>%
  group_by(date) %>%
  summarise(don_count = sum(don_count), don_mean_usd = sum(don_count * don_mean_usd) / sum(don_count))

#all time donation *count against important events, by emotion (blue - neg, red - pos)
data_k7_sum %>%
  filter(date >= ymd("2022-02-10")) %>%
  create_emotion_plot(don_count, "log of Total donation count")

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


#using regression residuals to deseasonalise
#...would take me another million years
