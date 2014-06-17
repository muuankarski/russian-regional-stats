# This file is part of the rustfare program (https://github.com/muuankarski/rustfare)

# Copyright (C) 2012-2013 Markus Kainu <markuskainu@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.




#' Download data from Rosstat Regional Statistics
#' 
#' Download data based on indicator and aggregation level 
#' from Rosstat Regional Statistics
#'
#' @param indicator Character string. Select indicator from the \code{indicator} column 
#' of data frame returned by function \code{RosstatIndicator}. More than one indicator is NOT allowed.
#' @param level Character string. Defines the administrative level of aggregation 
#' \itemize{
#'   \item \code{federal} at the federal level
#'   \item \code{federal_district} at the federal district level (8 regions)
#'   \item \code{region} at the regional level (83 regions)
#' } More than one level is NOT allowed.
#'  
#' @return data frame with 7 columns
#' \itemize{
#'  \item \code{region} Region name in Russian
#'  \item \code{region_en} Region name in English
#'  \item \code{year} Year of observation
#'  \item \code{value} Value of the indicator
#'  \item \code{level} Aggregation level
#'  \item \code{id_shape} id number for mergin with shapefile from GADM
#'  \item \code{indicator} name of the indicator
#' }
#' 
#' @export
#' @examples # dat <- GetRosstat("life_expectancy_men","federal_district")
#' @author Markus Kainu <markuskainu(at)gmail.com> 

GetRosstat2 <- function(code = 31501, level = "federal_district"){
  library(reshape2)
  library(stringr)
  
  #http://db5d6ea0e82f092e072c-37f3681af01a3d9c9cbb80b9eeb1d9f9.r33.cf2.rackcdn.com/data/data_31501_full.xls
  # mistä saan indikaattorin perusteella
  dat.url <- paste("http://db5d6ea0e82f092e072c-37f3681af01a3d9c9cbb80b9eeb1d9f9.r33.cf2.rackcdn.com/data/data_",
                   code,"_full.xls", sep="")
  
  sdat <- read.csv("/home/aurelius/workspace/ropengov/rustfare/data/fedstat_indicators/livingstand_tbl.csv")
  koodi <- sdat[sdat$code %in% code,]
  
  yearDat1 <- c(1995,1996,1997,1998,1999,
                rep(2000,90),
                rep(2001,90),
                rep(2000,69))
  
  if (koodi$type == 1) {
    # 2. reshape the data
    library(gdata)
    dat <- read.xls(dat.url, header=TRUE, 
                    skip=1, 
                    sheet=1, 
                    dec=",",
                    stringsAsFactor=F)
    dat[,-1] <- as.numeric(gsub(",", "", as.matrix(dat[,-1])))
    names(dat)[1] <- "region"
    library(reshape2)
    dat.l <- melt(dat, id.vars="region")
    dat.l[,2] <- as.numeric(gsub("X", "", dat.l[,2]))
    y.long <- dat.l
  }
  
  if (koodi$type == 2) {
    library(gdata)
    dat <- read.xls(dat.url, header=TRUE, 
                    skip=2, 
                    sheet=1, 
                    dec=",",
                    stringsAsFactor=F)
    tdat <- as.data.frame(t(dat))
    names(tdat) <- dat[,1]
    tdat <- tdat[-1,]
    tdat$year <- yearDat1
    tdat$region <- rownames(tdat)
    library(reshape2)
    dat <- melt(tdat, id.vars=c("region","year"))
    # 2. reshape the data
    dat$value <- as.numeric(gsub("\\.", "", dat$value))
    y.long <- dat
  }
  
  
  # preparing the data for region merge
  library(stringr)
  y.long$list <- str_extract_all(y.long$region, "[а-яА-я]+")
  y.long$one <- sapply(y.long$list, function(x){as.character(x[1])})
  y.long$two <- sapply(y.long$list, function(x){as.character(x[2])})
  y.long$three <- sapply(y.long$list, function(x){as.character(x[3])})
  y.long$four <- sapply(y.long$list, function(x){as.character(x[4])})
  y.long$five <- sapply(y.long$list, function(x){as.character(x[5])})
  y.long$six <- sapply(y.long$list, function(x){as.character(x[6])})
  y.long$x <- paste(y.long$one,
                    y.long$two,
                    y.long$three,
                    y.long$four,
                    y.long$five,
                    y.long$six,
                    sep="")
  y.long$only_txt <- str_replace_all(y.long$x,
                                     "NA",
                                     "")
  y.long$name_begin <- str_sub(y.long$only_txt, 1, 7)
  y.long$name_end <- str_sub(y.long$only_txt, -5)
  y.long$id_name <- tolower(paste(y.long$name_begin,
                                  y.long$name_end,
                                  sep=""))
  if (koodi$type == 1) {
    y.long$year <- y.long$variable
    y.long$variable <- "empty"
  }
  if (koodi$type == 2) {
    #y.long$year <- y.long$year
  }
  regiodata <- subset(y.long, select=c("region",
                                       "year",
                                       "value",
                                       "variable",
                                       "id_name"))
  # translation
  if (level == "federal") {
    load(url("http://research.muuankarski.org/rustfare/data/RosstatRegionKey.RData"))
    key <- subset(RosstatRegionKey, level == "federal")
    regiodata <- merge(regiodata,key, 
                       by="id_name")
    regiodata$indicator <- code
    output <- subset(regiodata, select=c("region","region_en",
                                         "year","value","level",
                                         "variable",
                                         "id_shape","indicator"))
    merge(output,koodi,by.x="indicator",by.y="code",all.x=TRUE)
  }
  if (level == "federal_district") {
    load(url("http://research.muuankarski.org/rustfare/data/RosstatRegionKey.RData"))
    key <- subset(RosstatRegionKey, level == "federal_district")
    regiodata <- merge(regiodata,key, 
                       by="id_name")
    regiodata$indicator <- code
    output <- subset(regiodata, select=c("region","region_en",
                                         "year","value","level",
                                         "variable",
                                         "id_shape","indicator"))
    merge(output,koodi,by.x="indicator",by.y="code",all.x=TRUE)
  }
  
  if (level == "region") {
    load(url("http://research.muuankarski.org/rustfare/data/RosstatRegionKey.RData"))
    key <- subset(RosstatRegionKey, level == "region")
    regiodata <- merge(regiodata,key, 
                       by="id_name")
    regiodata$indicator <- code
    output <- subset(regiodata, select=c("region","region_en",
                                         "year","value","level",
                                         "variable",
                                         "id_shape","indicator"))
    merge(output,koodi,by.x="indicator",by.y="code",all.x=TRUE)
  } else {
    regiodata
  }
  
}
