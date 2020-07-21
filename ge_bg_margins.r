library(plyr)
library(tidyr)
library(ggplot2)

bg_states = c("Colorado", "Maine", "New Mexico", "Virginia", "Minnesota", "Michigan", "Wisconsin", "Nevada", "New Hampshire", "Pennsylvania", "Florida", "Arizona", "North Carolina", "Ohio", "Georgia", "Iowa", "Texas") 

# Clean and format 2016 polling data
polls_2016 = data.frame(read.csv('http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv', stringsAsFactor = FALSE))
polls_2016 = polls_2016[which(polls_2016$type == "polls-only"),]
polls_2016 = polls_2016[which(polls_2016$state %in% bg_states),]
polls_2016 = polls_2016[which(polls_2016$population != "a"),]
polls_2016$margin = polls_2016$rawpoll_trump - polls_2016$rawpoll_clinton
polls_2016 = polls_2016[c("enddate", "margin")]
polls_2016$enddate = as.numeric(as.POSIXct(polls_2016$enddate, format="%m/%d/%Y"))
polls_2016$enddate = as.Date(as.POSIXct(polls_2016$enddate, origin="1970-01-01"))
polls_2016 = polls_2016[which((polls_2016$enddate > "2016-01-01") & (polls_2016$enddate < "2016-11-08")),]
polls_2016$enddate = format(polls_2016$enddate, format = "%m-%d")
polls_2016$enddate = as.Date(as.POSIXct(polls_2016$enddate, origin="2020-01-01", format = "%m-%d"))
polls_2016 = drop_na(polls_2016)

# Clean and format 2020 polling data
polls_2020 = data.frame(read.csv('https://projects.fivethirtyeight.com/polls-page/president_polls.csv', stringsAsFactor = FALSE))
polls_2020 = polls_2020[which(polls_2020$state %in% bg_states),]
polls_2020 = polls_2020[which(polls_2020$population_full != "a"),]
polls_2020$end_date = as.numeric(as.POSIXct(polls_2020$end_date, format="%m/%d/%y"))
polls_2020$end_date = as.Date(as.POSIXct(polls_2020$end_date, origin="1970-01-01"))
polls_2020 = polls_2020[which((polls_2020$end_date > "2020-01-01") & (polls_2020$end_date < Sys.Date())),]
polls_2020_trump = polls_2020[which(polls_2020$candidate_name == "Donald Trump"),]
polls_2020_trump$pct_trump = polls_2020_trump$pct
polls_2020_biden = polls_2020[which(polls_2020$candidate_name == "Joseph R. Biden Jr."),]
polls_2020_biden$pct_biden = polls_2020_biden$pct
polls_2020 = data.frame(join(polls_2020_trump, polls_2020_biden, by = "poll_id"))
polls_2020$margin = polls_2020$pct_trump - polls_2020$pct_biden
polls_2020 = polls_2020[c("end_date", "margin")]
polls_2020$end_date = as.Date(as.POSIXct(polls_2020$end_date, origin="2020-01-01", format = "%m-%d"))
polls_2020 = drop_na(polls_2020)  

ggplot() + geom_point(data = polls_2016, aes(x = enddate, y = margin), col = 'black', alpha = 0.25) + # 2016 margin dots
    stat_smooth(data = polls_2016, aes(x = enddate, y = margin, col = 'black'), span = 1/5, method = 'loess', se = FALSE, size = 2) + # 2016 margin trend line
    geom_point(data = polls_2020, aes(x = end_date, y = margin, col = 'red'), col = 'red', alpha = 0.25) + # 2020 margin dots
    stat_smooth(data = polls_2020, aes(x = end_date, y = margin, col = 'red'), span = 1/5, method = 'loess', se = FALSE, size = 2) + # 2020 margin trend line
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + ylim(-30, 30) + # Make the axes display 1-month intervals and the interval [-20, 20] for the margin
    labs(x = 'Month', y = 'Trump Relative Support', title = 'Trump\'s Average Battleground State Margin Among Registered or Likely Voters: 2016 vs. 2020', subtitle = 'January - June', caption = 'Data Source: FiveThirtyEight') + 
    scale_color_identity(guide = 'legend', name = 'Year', labels = c('2016', '2020')) + 
    theme(legend.position = c(0.05, 0.05), legend.justification = c('left', 'bottom'), plot.caption = element_text(hjust = 0))