####
# Discussion 4 Work

#######
# Discussion 4

# Name: Aidan Patt

### Notes on Github Repository

# make read me file look professional
# e.g include r packages using

# make sure to have github contain saved file png from discussion


# combined yy and yyyy
#coalesce( Xyy, yy, YYYY)

# three coluns one with yy, other with yy and the yyyy
# collapses other two into one

# had to change ot character
# then lubridate


# try to put buoys together and create visualizations


library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(tibble)
library(readr)


file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

load_buoy_data1 <- function(year) {
  path <- paste0(file_root, year, tail)
  
  
  if (year < 2007) {
    header <- scan(path, what = 'character', nlines = 1)
    buoy <- read.table(path, fill = TRUE, header = TRUE, sep = "")
    buoy <- add_column(buoy, mm = NA, .after = "hh")
    buoy <- add_column(buoy, TIDE = NA, .after = "VIS")
    
  } else {
    header <- scan(path, what = 'character', nlines = 1)  
    buoy <- fread(path, header = FALSE, skip = 1, fill = TRUE)
    
    setnames(buoy, header)
  }
  
  return(buoy)
}

all_data1 <- lapply(1985:2024, load_buoy_data1)

combined_data1 <- rbindlist(all_data1, fill = TRUE)







combined_data1 <- combined_data1 %>%
  mutate(
    YY = as.character(YY),
    `#YY` = as.character(`#YY`),
    YYYY = as.character(YYYY)
  )

# Combine year columns safely using coalesce
combined_data1 <- combined_data1 %>%
  mutate(YYYY = coalesce(YYYY, `#YY`, YY))
combined_data1 <- combined_data1 %>%
  mutate(BAR = coalesce(as.numeric(BAR), as.numeric(PRES)),  # Convert BAR and PRES to numeric
         WD = coalesce(as.numeric(WD), as.numeric(WDIR)))

combined_data1 <- combined_data1 %>%
  select(-TIDE, -TIDE.1, -mm,- WDIR, -PRES,-`#YY`,-YY)

combined_data1$datetime <- ymd_h(paste(combined_data1$YYYY, combined_data1$MM, combined_data1$DD, combined_data1$hh, sep = "-"))

combined_data1 <- combined_data1 %>%
  mutate(across(everything(), 
                ~ na_if(as.numeric(as.character(.)), 99) %>%
                  na_if(999) %>%
                  na_if(9999)))

#summary(combined_data)
#str(combined_data)
#str(combined_data$datetime)
if (!inherits(combined_data1$datetime, "POSIXct")) {
  combined_data1$datetime <- ymd_h(paste(combined_data1$YYYY, combined_data1$MM, combined_data1$DD, combined_data1$hh, sep = "-"))
}



# loading data for a different buoy
file_root = "https://www.ndbc.noaa.gov/view_text_file.php?filename=44029h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

all_data2 <- lapply(c(seq(2004,2008,1), seq(2011,2024,1)), load_buoy_data1)

combined_data2 <- rbindlist(all_data2, fill = TRUE)

head(combined_data2)
ggplot(data = combined_data2, mapping = aes(x = ATMP, y = WVHT)) +
  geom_point()

library(esquisse)

##### HW: use ggplot to make a data visualization ####

esquisser(data = combined_data1)

ggplot(combined_data1) +
  aes(x = WTMP, y = DEWP) +
  geom_point(colour = "#112446") +
  theme_minimal()

# why is the combined data2 different????
ggplot(combined_data2) +
  aes(x = WTMP, y = DEWP) +
  geom_point(colour = "#112446") +
  theme_minimal()