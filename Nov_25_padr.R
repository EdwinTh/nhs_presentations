# setup
library(tidyverse)
library(padr)
emergency <- readRDS("emergency.Rds")
emergency_sel <- emergency |> select(title, timeStamp)

emergency_sel |> count(title, sort = TRUE) |> View()
assault <- emergency |> 
  filter(title == "EMS: ASSAULT VICTIM")

# thicken
assault |> 
  thicken("month", "month") |> 
  count(month) |> 
  ggplot(aes(month, n)) + geom_line() + theme_bw()

assault |> 
  thicken("week", "week") |> 
  count(week) |> 
  ggplot(aes(week, n)) + geom_line() + theme_bw()

emergency_plot <- function(event, interval) {
  emergency_sel |> 
    filter(title == {{event}}) |> 
    thicken(interval, "interval") |> 
    count(interval) |> 
    ggplot(aes(interval, n)) + geom_line() + theme_bw()
}

emergency_sel("Fire: WOODS/FIELD FIRE", "week")

assault |> 
  thicken("month", "month", start_val = as.Date("2015-11-27")) |> 
  count(month)

# other arguments
?thicken()

# pad
assault_day <- assault |> 
  thicken("day", "day") |> 
  count(day)  

assault_day |> 
  head(30) |> 
  pad() |> fill_by_value() |> 
  ggplot(aes(day, n)) + geom_line() + geom_point() + theme_bw() +
  geom_smooth()

assault_day_padded <- assault_day |> 
  pad()

assault_day_padded |> 
  fill_by_value()

assault_day |> 
  mutate(n_cum = cumsum(n)) |>
  pad() |> 
  fill_by_value(n) |> 
  tidyr::fill(n_cum)

# pad is also great for data exploration.
is_complete <- function(x) {
  nrow(pad(x)) == nrow(x)
}

assault |> 
  thicken("day", "rolled") |> 
  count(rolled) |> 
  is_complete()

# within group padding
assault_twp <- emergency |> 
  filter(title == "EMS: ASSAULT VICTIM") |> 
  thicken("month", "month") |> 
  count(month, twp) 
assault_twp |> 
  pad(group = "twp", 
      start_val = min(assault_twp$month),
      end_val = max(assault_twp$month))

# other functions
tibble(year = c(2010, 2012, 2013, 2017), val = 1:4) |> 
  pad_int(by = "year")
seq(as.Date("2022-01-01"), length.out = 10, by = "year")

span_date(20220101, len_out = 10)

span_time("20210101", 2023)
