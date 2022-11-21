library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(RcppRoll)

data <- readRDS("data/data_complete.RDS")
summary(data)

#RQ1: relationships between emotion intensity (geolocated tweet count for world,
#air sirens for UAH) and donation count/value
#RQ2: relationships between different emotion types (events) and donation count/value
#RQ3: does sentiment explain heterogeneity between donor types? (UAH, foreign, crypto)

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
data_k2 <- data %>%
  group_by(type) %>%
  mutate(don_count = log(rollmean(don_count, k = 2, fill = NA)),
         don_mean_usd = log(rollmean(don_mean_usd, k = 2, fill = NA)),
         tweet_count = log(rollmean(tweet_count, k = 2, fill = NA)),
         siren_count = log(rollmean(siren_count, k = 2, fill = NA)))

#plot of counts by type
data_k7 %>%
  # filter(date <= ymd("2022-10-31") & date >= ymd("2022-02-10")) %>%
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

add_template_theming

#donation *count for Ukrainian type against important events & air raid siren count
#full
data_k7 %>%
  filter(type == "Ukrainian") %>%
  filter(date >= ymd("2022-02-24")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = don_count)) +
  geom_line(aes(y = siren_count), color = "green") +
  geom_vline(data = events, aes(xintercept = date, color = coloring), size = 2, alpha = 0.5) +
  scale_color_manual(values = c(`1` = "red", `0` = "green")) +
  xlab("Time") +
  ylab("n Ukrainian donations, air raid sirens") +
  theme(legend.position = "none") +
  NULL

#April only
data_k2 %>%
  filter(type == "Ukrainian") %>%
  filter(date <= ymd("2022-04-30") & date >= ymd("2022-04-01")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = don_count)) +
  geom_line(aes(y = siren_count_), color = "green") +
  geom_vline(data = events, aes(xintercept = date, color = coloring)) +
  xlab("Time") +
  ylab("n Ukrainian donations, air raid sirens") +
  theme(legend.position = "none") +
  NULL

#donation *count for Foreign type against important events & tweet count (total)
data_k7 %>%
  filter(type == "Foreign") %>%
  filter(date <= ymd("2022-10-31") & date >= ymd("2022-02-10")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = don_count)) +
  geom_line(aes(y = tweet_count), color = "green") +
  geom_vline(data = events, aes(xintercept = date, color = coloring)) +
  xlab("Time") +
  ylab("n Foreign donations, tweets") +
  theme(legend.position = "none") +
  NULL

#donation *count for Crypto type against important events & tweet count (total)
data_k7 %>%
  filter(type == "Crypto") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = don_count)) +
  geom_line(aes(y = tweet_count), color = "green") +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("n Crypto donations, tweets") +
  theme(legend.position = "none") +
  NULL

####################################################################

#donation *value for Ukrainian type against important events & air raid siren count
#full
data_k7 %>%
  filter(type == "Ukrainian") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = don_mean_usd)) +
  geom_line(aes(y = siren_count_all), color = "green") +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("n Ukrainian donations, air raid sirens") +
  theme(legend.position = "none") +
  NULL

#April only
data_k2 %>%
  filter(type == "Ukrainian") %>%
  filter(date <= ymd("2022-04-30") & date >= ymd("2022-04-01")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = don_mean_usd)) +
  geom_line(aes(y = siren_count_all), color = "green") +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("n Ukrainian donations, air raid sirens") +
  theme(legend.position = "none") +
  NULL

#donation *value for Foreign type against important events & tweet count (total)
data_k7 %>%
  filter(type == "Foreign") %>%
  filter(date <= ymd("2022-10-31") & date >= ymd("2022-02-10")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = don_mean_usd)) +
  geom_line(aes(y = tweet_count), color = "green") +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("n Foreign donations, tweets") +
  theme(legend.position = "none") +
  NULL

#donation *value for Crypto type against important events & tweet count (total)
data_k7 %>%
  filter(type == "Crypto") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = don_mean_usd)) +
  geom_line(mapping = aes(y = tweet_count), color = "green") +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("n Crypto donations, tweets") +
  theme(legend.position = "none") +
  NULL

####################################################################
#RQ2################################################################
####################################################################


#all time donation *count against important events, by emotion (blue - neg, red - pos)
data_k7 %>%
  ggplot(aes(x = date, y = don_count)) +
  geom_line() +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("log of Total donation count") +
  theme(legend.position = "none") +
  NULL

#Both positive and negative events immediately increase donation count?

#all time donation *value against important events, by emotion
data_k7 %>%
  ggplot(aes(x = date, y = don_mean_usd)) +
  geom_line() +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("log of Average donation") +
  theme(legend.position = "none") +
  NULL

#Positive events cause decrease in value, negative events cause increase in value?

###################################################################################################

#How is this not denoised????????????????????????? aaa

# donation *count around Kerch (positive) & response strikes (negative)
data_k2 %>%
  filter(date <= ymd("2022-10-20") & date >= ymd("2022-09-29")) %>%
  ggplot(aes(x = date, y = don_count)) +
  geom_line() +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("log(Average donation)") +
  theme(legend.position = "none") +
  NULL

# donation *count around Vinnitsya (negative)
data_k2 %>%
  filter(date <= ymd("2022-07-25") & date >= ymd("2022-07-04")) %>%
  ggplot(aes(x = date, y = don_count)) +
  geom_line() +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("log(Average donation)") +
  theme(legend.position = "none") +
  NULL

# donation *value around Kerch (positive) & response strikes (negative)
data_k2 %>%
  filter(date <= ymd("2022-10-20") & date >= ymd("2022-09-29")) %>%
  ggplot(aes(x = date, y = don_mean_usd)) +
  geom_line() +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("log(Average donation)") +
  theme(legend.position = "none") +
  NULL


# donation *value around Vinnitsya (negative)
data_k2 %>%
  filter(date <= ymd("2022-07-25") & date >= ymd("2022-07-04")) %>%
  ggplot(aes(x = date, y = don_mean_usd)) +
  geom_line() +
  geom_vline(data = events,
             aes(xintercept = date,
                 color = coloring)) +
  xlab("Time") +
  ylab("log of Average donation") +
  theme(legend.position = "none") +
  NULL

####################################################################
#RQ3################################################################
####################################################################

#all time donation *count against important events, *positive emotion, by type
data_k7 %>%
  filter(date <= ymd("2022-10-31") & date >= ymd("2022-02-10")) %>%
  ggplot(aes(x = date, y = don_count)) +
  geom_line() +
  geom_vline(data = filter(events, coloring == "0"),
             aes(xintercept = date, color = "blue")) +
  xlab("Time") +
  ylab("log of Donation count, by type") +
  facet_wrap(~type) +
  theme(legend.position = "none") +
  NULL

#all time donation *value against important events, *negative emotion, by type
data_k7 %>%
  filter(date <= ymd("2022-10-31") & date >= ymd("2022-02-10")) %>%
  ggplot(aes(x = date, y = don_mean_usd)) +
  geom_line() +
  geom_vline(data = filter(events, coloring == "1"),
             aes(xintercept = date,
                 color = coloring, colour = "blue")) +
  #how is this never blue lol
  xlab("Time") +
  ylab("log of Average donation, by type") +
  facet_wrap(~type) +
  theme(legend.position = "none") +
  NULL


#using regression residuals to deseasonalise
#...would take me another million years
