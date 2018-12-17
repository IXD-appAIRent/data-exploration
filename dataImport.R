library(plyr)

data.senate.read = function(filename) {
  header <- scan(filename, nlines = 1, what = character(), sep = ";")
  header[1] = "dt"
  
  df = read.table(file = filename, header = FALSE, sep = ";", skip = 5)
  names(df) <- header
  
  
  df[2:length(df)] <- lapply(df[2:length(df)], as.numeric)
  df$mean = rowMeans(df[2:length(df)], na.rm = TRUE)
  
  df$dtPosix = as.POSIXct(df$dt, format = "%d.%m.%Y %H:%M")
  
  df
}

data.senate.merge = function(df, pollutant) {
  if (file.exists(paste0("data/senateData2017/", pollutant, "_all_hourly_2017-01-01-0_2017-12-31-23.csv"))) {
    df.all = data.senate.read(paste0("data/senateData2017/", pollutant, "_all_hourly_2017-01-01-0_2017-12-31-23.csv"))
    df = merge(x = df, y = df.all[, c("dtPosix", "mean")], by.x = "dtPosix", by.y = "dtPosix", all.y=TRUE, all.x = TRUE)
    df = rename(df, replace = c("mean" = paste0(pollutant, ".all")))
  }
    
  if (file.exists(paste0("data/senateData2017/", pollutant, "_background_hourly_2017-01-01-0_2017-12-31-23.csv"))) {
    df.background = data.senate.read(paste0("data/senateData2017/", pollutant, "_background_hourly_2017-01-01-0_2017-12-31-23.csv"))
    df = merge(x = df, y = df.background[, c("dtPosix", "mean")], by.x = "dtPosix", by.y = "dtPosix", all.y=TRUE, all.x = TRUE)
    df = rename(df, replace = c("mean" = paste0(pollutant, ".background")))
  }
  
  if (file.exists(paste0("data/senateData2017/", pollutant, "_traffic_hourly_2017-01-01-0_2017-12-31-23.csv"))) {
    df.traffic = data.senate.read(paste0("data/senateData2017/", pollutant, "_traffic_hourly_2017-01-01-0_2017-12-31-23.csv"))
    df = merge(x = df, y = df.traffic[, c("dtPosix", "mean")], by.x = "dtPosix", by.y = "dtPosix", all.y=TRUE, all.x = TRUE)
    df = rename(df, replace = c("mean" = paste0(pollutant, ".traffic")))
  }
  
  if (file.exists(paste0("data/senateData2017/", pollutant, "_suburb_hourly_2017-01-01-0_2017-12-31-23.csv"))) {
    df.suburb = data.senate.read(paste0("data/senateData2017/", pollutant, "_suburb_hourly_2017-01-01-0_2017-12-31-23.csv"))
    df = merge(x = df, y = df.suburb[, c("dtPosix", "mean")], by.x = "dtPosix", by.y = "dtPosix", all.y=TRUE, all.x = TRUE)
    df = rename(df, replace = c("mean" = paste0(pollutant, ".suburb")))
  }
  
  df
}


wind.deg.name = function(value) {
  name = ""
  if (is.na(value))
    name = "NA"
  else if (value >= 348.75 || value <= 11.25)
    name = "N"
  else if (value > 11.25 && value <= 33.75)
    name = "NNE"
  else if (value > 33.75 && value <= 56.25)
    name = "NE"
  else if (value > 56.25 && value <= 78.75)
    name = "ENE"
  else if (value > 78.75 && value <= 101.25)
    name = "E"
  else if (value > 101.25 && value <= 123.75)
    name = "ESE"
  else if (value > 123.75 && value <= 146.25)
    name = "SE"
  else if (value > 146.25 && value <= 168.75)
    name = "SSE"
  else if (value > 168.75 && value <= 191.25)
    name = "S"
  else if (value > 191.25 && value <= 213.75)
    name = "SSW"
  else if (value > 213.75 && value <= 236.25)
    name = "SW"
  else if (value > 236.25 && value <= 258.75)
    name = "WSW"
  else if (value > 258.75 && value <= 281.25)
    name = "W"
  else if (value > 281.25 && value <= 303.75)
    name = "WNW"
  else if (value > 303.75 && value <= 326.25)
    name = "NW"
  else if (value > 326.25 && value <= 348.75)
    name = "NNW"
  
  name
}

temp.c.group = function(value) {
  name = NA
  if (is.na(value))
    name = NA
  else if (value >= -20 && value <= -18)
    name = -19
  else if (value >= -17 && value <= -15)
    name = -16
  else if (value >= -14 && value <= -12)
    name = -13
  else if (value >= -11 && value <= -9)
    name = -10
  else if (value >= -8 && value <= -6)
    name = -7
  else if (value >= -5 && value <= -3)
    name = -4
  else if (value >= -2 && value <= 0)
    name = -1
  else if (value >= 1 && value <= 3)
    name = 2
  else if (value >= 4 && value <= 6)
    name = 5
  else if (value >= 7 && value <= 9)
    name = 8
  else if (value >= 10 && value <= 12)
    name = 11
  else if (value >= 13 && value <= 15)
    name = 14
  else if (value >= 16 && value <= 18)
    name = 17
  else if (value >= 19 && value <= 21)
    name = 20
  else if (value >= 22 && value <= 24)
    name = 23
  else if (value >= 25 && value <= 27)
    name = 26
  else if (value >= 28 && value <= 30)
    name = 29
  else if (value >= 31 && value <= 33)
    name = 32
  else if (value >= 34 && value <= 36)
    name = 35
  else if (value >= 37 && value <= 39)
    name = 38
  else if (value >= 40 && value <= 42)
    name = 41
  
  name
}


data.import = function() {
    df = read.table(file = "data/owm_berlin_2017.csv", header = TRUE, sep = ",")
    df$dtPosix = as.POSIXct(df$dt_iso)
    df$temp.c = df$temp - 273.15
    df$temp.c.group = sapply(df$temp.c, temp.c.group)
    
    #weekdays
    weekday = factor(as.POSIXlt(df$dtPosix)$wday, levels = (0:6))
    levels(weekday) = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")
    df$weekday = weekday
    
    #hours
    df$hours = factor(as.POSIXlt(df$dtPosix)$hour, levels = (0:23))
    
    df = data.senate.merge(df, "chb")
    df = data.senate.merge(df, "cht")
    df = data.senate.merge(df, "co")
    df = data.senate.merge(df, "no2")
    df = data.senate.merge(df, "no")
    df = data.senate.merge(df, "nox")
    df = data.senate.merge(df, "o3")
    df = data.senate.merge(df, "pm10")
    df = data.senate.merge(df, "so2")
    
    df = df[colSums(!is.na(df)) > 0]
    
    df$wind.deg.name = sapply(df$wind_deg, wind.deg.name)
    
    
      
    wind.deg.name(df$wind_deg)
    
    df
}



