ImportBAEA <- function(
existing=deployed_recent
existing = deployed_all
import = TRUE
  if (import == TRUE) {
    baea <- readRDS("C:/Work/R/Projects/baea_ibm/Data/BAEA/BAEA.rds")
#    saveRDS(baea, "C:/Work/R/Projects/baea_ibm/Data/BAEA/BAEA.rds")
    if (!is.null(existing)) {
      max(baea$date)
      max(existing$date)
      existing <- subset(existing, date > (as.Date(max(baea$date)) -
        lubridate::days(3)))
      baea <- subset(baea, date <= (as.Date(max(baea$date)) -
        lubridate::days(3)))
      max(baea$date)
      min(existing$date)
    # 3 days are removed from baea to ensure that AddSegmentTimeLength, etc. was
    # done on a full dataset. The baea and existing datasets should not overlap.
      if(!("sunrise" %in% colnames(existing))) {
        existing <- AddSolarTimes(existing)
      }
      existing <- AddStepLengthAndAngles(existing)
      existing <- AddStepTime(existing, by = "serial")
      existing <- AddTimeStepProportion(existing)
      existing <- AddFirstLastDistance(existing)
      baea_full <- rbind(baea, existing)
      baea_full <- dplyr::distinct(baea_full)
      baea_full <- baea_full[with(baea_full,order(id,datetime)),]
      row.names(baea_full) <- NULL
      date <- Sys.Date()
      outfile <- paste("C:/Work/R/Data/BAEA/Archive/BAEA_", date, ".csv",
        sep ="")
      if (!file.exists(outfile)) {
        writeLines(noquote(paste("Merging existing and import")))
        write.csv(baea_full, file=outfile, row.names=FALSE)
        writeLines(noquote(c("Writing: ", outfile, sep = "")))
      }
      saveRDS(baea_full, "C:/Work/R/Projects/baea_ibm/Data/BAEA/BAEA.rds")
        # rewrites import file
      writeLines(noquote(
        "Writing: \"C:/Work/R/Projects/baea_ibm/Data/BAEA/BAEA.rds\""))
      baea <- baea_full
    }
  }
  if (import == FALSE) {
  writeLines(noquote(paste("BAEA.rds was NOT imported")))
    if (!is.null(existing)) {
      if(!("sunrise" %in% colnames(existing))) {
        existing <- AddSolarTimes(existing)
      }
      existing <- AddStepLengthAndAngles(existing, by = "id")
      existing <- AddStepTime(existing, by = "id")
      existing <- AddTimeStepProportion(existing)
      existing <- AddFirstLastDistance(existing)
      writeLines(noquote(paste("Coverted existing to baea", sep="")))
      baea <- existing
      } else {
        writeLines(noquote("Nothing imported or converted"))
      }
  }
  return(baea)
}

  by = "id"
  datetime = "datetime"
  tz = "Etc/GMT+5"
  df <- existing
  df$by <- df[,by]
  df$datetime <- df[,datetime]
  df$date <- as.Date(df[,datetime], tz=tz)
  df$datetime <- as.character(df$datetime)  # convert to character
  df$date <- as.character(df$date)  # convert to character



  first <- plyr::ddply(df, plyr::.(date, by), function(x) x[1, ])# first records
  last <- plyr::ddply(df, plyr::.(date, by), function(x) x[(nrow(x)), ])
    # last records

  first$first <- "First"  # populates "first" column
  last$last <- "Last"  # populates "last" column
  first <- subset(first, select = c(by, first, date, datetime, long_utm,
    lat_utm))  # subsets first records
  colnames(first) <- c("by", "first", "date", "datetime","long_utm_first",
    "lat_utm_first")  # need to id first locations
  last <- subset(last, select = c(by, last, date, datetime, long_utm,
    lat_utm))  # subsets last records
  colnames(last) <- c("by", "last", "date", "datetime","long_utm_last",
    "lat_utm_last")  # need to id last locations
  first_date <- subset(first, select=c(by, date, long_utm_first,
    lat_utm_first))  # all first locations
  last_date <- subset(last, select=c(by, date, long_utm_last, lat_utm_last))
    # all last locations
  first_last_date <- merge(first_date, last_date, by = c("by","date"),
    all.x = TRUE)  # first and last record locations
  first_last <- merge(first, last, all=TRUE)  # merges first and last records
  first_last_datetime <- subset(first_last, select = c(by, first, last,
    datetime))  # only first and last records and their times
  df <- merge(df, first_last_datetime, by=c("by", "datetime"), all.x=TRUE)
  df <- merge(df, first_last_date, by=c("by", "date"), all.x=TRUE)
      #adds the lat and long for the first and last records to the dataframe
  df$dist_first <- sqrt((df$long_utm - df$long_utm_first)^2 +
      (df$lat_utm - df$lat_utm_first)^2)
  df$dist_last <- sqrt((df$long_utm - df$long_utm_last)^2 +
      (df$lat_utm - df$lat_utm_last)^2)
  df$date <- as.Date(df$date, "%Y-%m-%d") #convert date into date format
  df$datetime <- strptime(df$datetime, "%Y-%m-%d %H:%M:%S")  # back to POSIXct
  df$datetime <- as.POSIXct(df$datetime, tz=tz, usetz=FALSE)  # returns to tz
  drops <- c("long_utm_first", "lat_utm_first", "long_utm_last",
             "lat_utm_last", "by")  # vector of columns to drop
  df <- df[ ,!(names(df) %in% drops)]
  return(df)









library(plyr)
library(dplyr)
library(ggplot)
library(multcompView)

set.seed(0)
lev <- gl(3, 10)
y <- c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)

a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
HSD <- tHSD
flev <- "lev"

generate_label_df <- function(HSD, flev){
 # Extract labels and factor levels from Tukey post-hoc
 Tukey.levels <- HSD[[flev]][,4]
 Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
 plot.labels <- names(Tukey.labels[['Letters']])

 # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between
 # upper quantile and label placement
 boxplot.df <- ddply(d, flev, function (x) max(fivenum(x$y)) + 0.2)

 # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
     stringsAsFactors = FALSE)

 # Merge it with the labels
   labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)

return(labels.df)
}

p_base <- ggplot(d, aes(x=lev, y=y)) + geom_boxplot() +
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1,
    label = labels))

p_base









