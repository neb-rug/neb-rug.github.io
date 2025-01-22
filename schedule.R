library(tidyverse)

# General purpose calendar to show events adapted from
# Create a calendar for your syllabus ----
# Source: http://svmiller.com/blog/2020/08/a-ggplot-calendar-for-your-semester/

meetup_dates <- c(ymd(20241029))

talk_dates <- c(ymd(20241204, 20250218))

# What are the full dates of the semester? Here, I'll exclude exam week as I like to do.
# In this case: 6 January to 23 April
semester_dates <- seq(ymd(20241001), ymd(20250430), by=1)

# Custom function for treating the first day of the month as the first week
# of the month up until the first Sunday (unless Sunday was the start of the month)
wom <- function(date) {
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}

# Create a data frame of dates, assign to Cal
Cal <- tibble(date = semester_dates)  |>
  mutate(mon = lubridate::month(date, label = TRUE, abbr = FALSE), # get month label
         wkdy = weekdays(date, abbreviate = TRUE), # get weekday label
         wkdy = fct_relevel(wkdy, "Sun", "Mon", "Tue", "Wed", "Thu","Fri","Sat"), # make sure Sunday comes first
         semester = date %in% semester_dates, # is date part of the semester?
         day = lubridate::mday(date), # get day of month to add later as a label
         # Below: our custom wom() function
         week = wom(date))

# Create a category variable, for filling.
# I can probably make this a case_when(), but this will work.

Cal <- Cal |>
  mutate(category = ifelse(date %in% meetup_dates, "Meetup",
                           ifelse(date %in% talk_dates, "Talk","NA"))
  )

class_cal <- Cal |>
  filter(date >= "2025-01-01") |>
  ggplot(aes(wkdy, week)) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    panel.grid.major.x = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  # geom_tile and facet_wrap will do all the heavy lifting
  geom_tile(alpha=0.8, aes(fill=category), color="black", linewidth=.45) +
  facet_wrap(~mon, scales = "free", ncol=3) +
  # fill in tiles to make it look more "calendary" (sic)
  geom_text(aes(label=day), colour = "grey50") +
  # put your y-axis down, flip it, and reverse it
  scale_y_reverse(breaks=NULL) +
  # manually fill scale colors to something you like...
  scale_color_manual(values = c("FALSE" = "white", "TRUE" = "black"), guide = "none") +
  scale_fill_manual(values=c("Talk"="salmon",
                             "Meetup" = "goldenrod",
                             "NA" = "white" # I like these whited out...
  ),
  #... but also suppress a label for a non-class semester day
  breaks=c("Talk", "Meetup")) +
  ggtitle(year(Cal$date[1]))
