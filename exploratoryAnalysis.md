# Gender Bias

## Notes
use gc() - garbage Collection -  to free up memory

## Introduction
My daughters attend Creekside Elementary (School.Code = 6117469), so that may show up periodically.

## Required Packages

```r
list.of.packages <- c("ggplot2", "ggmap", "rjson", "tm", "plyr", "stringr", 
    "RJSONIO", "RCurl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, 
    "Package"])]
if (length(new.packages)) install.packages(new.packages)
rm(list.of.packages, new.packages)

# Maps
library(ggplot2)
library(ggmap)

# JSON
library(rjson)
library(RJSONIO)
```

```
## 
## Attaching package: 'RJSONIO'
## 
## The following objects are masked from 'package:rjson':
## 
##     fromJSON, toJSON
```

```r

# Text tools
library("tm")
library("plyr")
library("stringr")

# Demographics
library("RCurl")
```

```
## Loading required package: bitops
```



## Prepare Data

### Load Raw Data 

Read in the data:

```r
file.location <- "../Data Sets/san diego research file/"

scores.file.name <- "ca2012_all_37_csv_v3.txt"
entities.file.name <- "ca2012entities_csv.txt"
subgroups.file.name <- "Subgroups.txt"
tests.file.name <- "Tests.txt"

scores <- read.csv(paste0(file.location, scores.file.name), header = TRUE, sep = ",", 
    strip.white = TRUE)
entities <- read.csv(paste0(file.location, entities.file.name), header = TRUE, 
    sep = ",", strip.white = TRUE)
subgroups <- read.csv(paste0(file.location, subgroups.file.name), header = TRUE, 
    sep = ",", strip.white = TRUE)
tests <- read.csv(paste0(file.location, tests.file.name), header = TRUE, sep = ",", 
    strip.white = TRUE)
```


### Clean Data



```r
# School Codes of 0 or 1 are not normal schools and I want to filter them
# out:
scores <- scores[scores$School.Code != 0 & scores$School.Code != 1, ]
entities <- entities[entities$School.Code != 0 & entities$School.Code != 1, 
    ]

# Mean.Scale.Score comes in as factor, should be numeric
scores$Mean.Scale.Score <- as.numeric(as.character(scores$Mean.Scale.Score))
```

```
## Warning: NAs introduced by coercion
```

```r

# Percentage.Advanced coming in as a factor, change this to numeric
scores$Percentage.Advanced <- as.numeric(scores$Percentage.Advanced)

# Only need entities from San Diego County
entities <- entities[entities$County.Name == "San Diego", ]

# Remove duplicate columns from subgroups and rename 1st column to
# Subgroup.ID
subgroups <- subgroups[, -1]
names(subgroups)[1] <- "Subgroup.ID"

# There is no information in the subgroups file on Subgroup.ID == 1. Remove
# this from scores scores.sg1 <- scores[scores$Subgroup.ID == 1, ] # This
# contains all the differentiation in Percentage.Advanced
scores <- scores[scores$Subgroup.ID != 1, ]

# Remove duplicate columns from tests
tests <- tests[, names(tests) != "Test.ID.Num"]
# names(tests)[1] <- 'Test.ID'

# Rename scores$Test.Id to scores$Test.ID for consistency
names(scores)[names(scores) == "Test.Id"] <- "Test.ID"

# Limit focus to CST Mathematics - this test covers grades 2 - 7
scores <- scores[scores$Test.ID == 8, ]

# Limit focus to the male and female subgroups
scores <- scores[scores$Subgroup.ID == 3 | scores$Subgroup.ID == 4, ]

# Remove NA scores
scores <- scores[!is.na(scores$Mean.Scale.Score), ]
```



### Merge Data

Attempting to add columns to scores data set. Starting with 4191 obs. Should be the same when I'm done.

```r
# Merge entities - exclude redundant information
scores <- merge(x = scores, y = entities[, (names(entities) != "County.Code" & 
    names(entities) != "District.Code" & names(entities) != "Charter.Number" & 
    names(entities) != "Test.Year")], by = "School.Code", all.x = TRUE)

# Merge Subgroups
scores <- merge(x = scores, y = subgroups, by = "Subgroup.ID", all.x = TRUE)

# Merge Tests
scores <- merge(x = scores, y = tests, by = "Test.ID", all.x = TRUE)
```



## Explore Data

Start by just trying to get a feel for what's there

### Creekside delta on CST Mathematics test by grade by gender

* What about the same thing but focused on Creekside.
* Narrow the focus to one test ("CST Mathematics"). Try to correlate with reports from the web tool.

issues: 
* Where are the NAs coming from? I don't think they should be there  


```r
# par(mar=c(5,4,4,2)) : default values
par(mar = c(5, 5, 4, 2))

# Filter scores to just Creekside and just the test 'CST Mathematics'

creekside.scores <- scores[scores$School.Name == "Creekside Elementary" & scores$Test.Name == 
    "CST Mathematics" & scores$All.Students.1 == "Gender", ]
creekside.scores <- creekside.scores[!is.na(creekside.scores$Test.Name), ]
creekside.scores <- creekside.scores[order(creekside.scores$Grade, creekside.scores$All.Students), 
    ]

# tbl <- aggregate(Mean.Scale.Score ~ c(Subgroup.ID, Grade),
# data=creekside.scores, mean, na.action=na.omit) tbl <- merge(x = tbl, y =
# subgroups, by='Subgroup.ID', all.x=TRUE)

barplot(creekside.scores$Mean.Scale.Score, names.arg = c("2nd F", "2nd M", "3rd F", 
    "3rd M", "4th F", "4th M", "5th F", "5th M"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


That's interesting, and seems to show a gender gap in 3rd, 4th, and 5th grades. This is unexpected since Creekside is the top elementary school in the district and in my opinion has most of the attributes you would expect to see in a progressive school: fairly affluent, college educated parents, etc

```
Let's try this same metric but summarize across all schools:
```

### CST Mathematics test by gender by grade


```r

# par(mar=c(5,4,4,2)) : default values
par(mar = c(5, 4, 4, 2))

# Filter scores to just the test 'CST Mathematics'

cstM.scores <- scores[scores$Test.Name == "CST Mathematics" & scores$All.Students.1 == 
    "Gender", ]
cstM.scores <- cstM.scores[!is.na(cstM.scores$Test.Name), ]
cstM.scores <- cstM.scores[!is.na(cstM.scores$Mean.Scale.Score), ]  # Again, why are there NAs.
# Can I just get rid of them?
cstM.scores <- cstM.scores[order(cstM.scores$Grade, cstM.scores$All.Students), 
    ]

tbl <- ddply(cstM.scores, .(All.Students, Grade), function(x) mean(x$Mean.Scale.Score))
tbl <- tbl[order(tbl$Grade, tbl$All.Students), ]

barplot(tbl$V1, names.arg = c("2nd F", "2nd M", "3rd F", "3rd M", "4th F", "4th M", 
    "5th F", "5th M", "6th F", "6th M", "7th F", "7th M"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


Interesting. Technically, the difference is still there, but I wonder if it's statistically significant.

```
Next let's see if I can break this down by zip code
```


```r

# par(mar=c(5,4,4,2)) : default values
par(mar = c(5, 4, 4, 2))

# Filter scores to just the test 'CST Mathematics'

cstM.scores <- scores[scores$Test.Name == "CST Mathematics" & scores$All.Students.1 == 
    "Gender", ]
cstM.scores <- cstM.scores[!is.na(cstM.scores$Test.Name), ]
cstM.scores <- cstM.scores[!is.na(cstM.scores$Mean.Scale.Score), ]  # Again, why are there NAs.
# Can I just get rid of them?
cstM.scores <- cstM.scores[order(cstM.scores$Grade, cstM.scores$All.Students), 
    ]

tbl <- ddply(cstM.scores, .(All.Students, Grade, Zip.Code), function(x) mean(x$Mean.Scale.Score))
tbl <- tbl[order(tbl$Grade, tbl$All.Students), ]

barplot(tbl$V1)
```





```r
# for each zip code for each school code for each grade (2-7)

# num = 0

scores.female <- data.frame(zip.code = as.integer(), school.code = as.integer(), 
    grade = as.integer(), school.name = as.character(), score = as.numeric(), 
    stringsAsFactors = FALSE)
scores.male <- data.frame(zip.code = as.integer(), school.code = as.integer(), 
    grade = as.integer(), school.name = as.character(), score = as.numeric(), 
    stringsAsFactors = FALSE)

for (zip in unique(scores$Zip.Code)) {
    for (code in unique(scores$School.Code[scores$Zip.Code == zip])) {
        for (grade in 2:7) {
            filter <- (scores$Zip.Code == zip & scores$School.Code == code & 
                scores$Grade == grade)
            if (sum(filter) == 2) {
                newrow.f <- list(zip, code, grade, as.character(scores$School.Name[filter & 
                  scores$All.Students == "Females"]), scores$Mean.Scale.Score[filter & 
                  scores$All.Students == "Females"])
                newrow.m <- list(zip, code, grade, as.character(scores$School.Name[filter & 
                  scores$All.Students == "Males"]), scores$Mean.Scale.Score[filter & 
                  scores$All.Students == "Males"])
                # Append values to scores.female
                scores.female[dim(scores.female)[1] + 1, ] <- newrow.f
                # Append values to scores.male
                scores.male[dim(scores.male)[1] + 1, ] <- newrow.m
            }
        }
    }
}
```


Prototype function to test understanding of ddply

```r
a <- data.frame(zip = c(1, 1, 2, 2, 3, 3), gender = rep(c("M", "F"), 3), score = sample(1:10, 
    6, replace = TRUE), name = c("a", "a", "b", "b", "c", "c"))
ddply(a[a$gender == "F", ], .(zip, name), summarise, avg = mean(score))
```



Create a summary of scores male and female scores per school. Filter out schools that do not have both a male and female score.

```r
filter.F <- scores$All.Students == "Females"
filter.M <- scores$All.Students == "Males"

agg.F <- ddply(scores[filter.F, ], .(School.Code, School.Name, Zip.Code), summarise, 
    avg = mean(Mean.Scale.Score))

agg.M <- ddply(scores[filter.M, ], .(School.Code, School.Name, Zip.Code), summarise, 
    avg = mean(Mean.Scale.Score))

# Filter out schools that do not have both a male and female score
agg.F <- agg.F[agg.F$School.Code %in% agg.M$School.Code, ]
agg.M <- agg.M[agg.M$School.Code %in% agg.F$School.Code, ]

# Check that school codes are equal. This means I can compare scores from
# agg.F[n, ] & agg.M[n, ] If all codes are equal, combine into one data
# frame
if (all(agg.F$School.Code == agg.M$School.Code)) {
    agg.byschool <- data.frame(School.Code = agg.F$School.Code, School.Name = agg.F$School.Name, 
        Zip.Code = agg.F$Zip.Code, avg.M = agg.M$avg, avg.F = agg.F$avg, delta = (agg.M$avg - 
            agg.F$avg))
} else {
    print("PROBLEM!!!!")
}
```





```r
barplot(scores.male$score - scores.female$score)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-111.png) 

```r
for (g in 2:7) {
    barplot((scores.male$score - scores.female$score)[scores.female$grade == 
        g])
}
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-112.png) ![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-113.png) ![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-114.png) ![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-115.png) ![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-116.png) ![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-117.png) 



## Next Actions: Test Scores


* Continue to look at math score by grade and gender. Next break that down by zip code.
* Build the data manually. For each unique zip code, create a data frame with Zip.Code, School.Code, Mean.Scale.Score(of CST Mathematics), Grade, Gender == Males, then the same thing with Gender == Females 

This needs to contain all possibilities such that it will be possible to subtract the two to get a delta.  

Once I have this I need to see how best to plot the results to look at the entire set: heat map? - one color set for males higher, one color set for females higher, higher intensity for higher delta.

------------------------

* There is a column that tallys how many from each subgroup were tested
* How will I combine this with Census Data?
* How are the sub subgroups created? Are the same numbers in each?
* Variables to explore
  * Total.STAR.Enrollment
  * Test.Type
  * Grade
  * Students.with.Scores

## Notes / Lessons Learned: Test Scores

My biggest challenges so far have been wading through all the data and trying to merge all the different tables in a way I can make sense of.

------------------------
------------------------

## Demographics

Now let's attempt to get demographic information for each school. I think this can be done in the following way:  

* From above, I have a list of each school and the zip code it is in.
* Using the Google maps geocoding api, I should be able to submit the school name and zip code and have returned the lat / lng of each school
* Submitting the lat / lng to the http://mcdc2.missouri.edu/ I should be able to get demographics per specified radius

### Geocode schools

Turns out there is a function (probably in ggmap?) that does this called geocode

Geocode schools from agg.byschool

```r
if (file.exists("agg.byschool")) {
    load("agg.byschool")
} else {
    agg.byschool <- data.frame(agg.byschool, lon = rep(NA, nrow(agg.byschool)), 
        lat = rep(NA, nrow(agg.byschool)))
    for (i in 1:nrow(agg.byschool)) {
        loc <- geocode(paste(agg.byschool$School.Name[i], agg.byschool$Zip.Code[i], 
            sep = " "))
        agg.byschool$lon[i] <- as.numeric(loc[1])
        agg.byschool$lat[i] <- as.numeric(loc[2])
    }
    save(agg.byschool, file = "agg.byschool")
}
```



Let's put these on a map to do a sanity check

```r
san.diego.map <- qmap("poway", zoom = 9, color = "bw", legend = "topleft")
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=poway&zoom=9&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=poway&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r
san.diego.map + geom_point(aes(x = lon, y = lat), data = agg.byschool)
```

```
## Warning: Removed 23 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

23 values were not plotted. These most likely have incorrect geocodes but for now I will ignore these.


### School Demographics
proto: This loads Creekside Demographics

```r
lat = 32.944139
lon = -117.077471
radius = 2
name = "creekside"
url = paste("http://mcdc.missouri.edu/cgi-bin/broker", "?_PROGRAM=websas.caps10acs.sas&_SERVICE=appdev", 
    "&latitude=", lat, "&longitude=", lon, "&sitename=", name, "&radii=", radius, 
    "&dprofile=on&eprofile=on&sprofile=on&hprofile=on", "&cntypops=on&printdetail=on&_debug=", 
    sep = "")
html <- getURL(url)
pattern <- "href=\"/tmpscratch/caps10acs(.*?).csv\""
match.info <- regexec(pattern = pattern, text = html)
num.start <- match.info[[1]][2]
num.length <- attr(match.info[[1]], "match.length")[2]
num <- substr(html, num.start, num.start + num.length - 1)
file.location <- paste("http://mcdc.missouri.edu/tmpscratch/", "caps10acs", 
    num, ".csv", sep = "")
file.location
data.tmp <- read.csv(file = file.location)
```


This function retrieves demographic information from mcdc.missouri.edu based on lat, lon, and radius. If no information exists the radius will be increased (up to 10) until information is found. If still no information is available, NA is returned.

```r
get.demographics <- function(school.code, lat, lon, radius) {
    data.newline <- NA
    while (all(is.na(data.newline), radius < 10)) {
        url = paste("http://mcdc.missouri.edu/cgi-bin/broker", "?_PROGRAM=websas.caps10acs.sas&_SERVICE=appdev", 
            "&latitude=", lat, "&longitude=", lon, "&sitename=", "&radii=", 
            radius, "&dprofile=on&eprofile=on&sprofile=on&hprofile=on", "&cntypops=on&printdetail=on&_debug=", 
            sep = "")
        html <- getURL(url)
        pattern <- "href=\"/tmpscratch/caps10acs(.*?).csv\""
        match.info <- regexec(pattern = pattern, text = html)
        num.start <- match.info[[1]][2]
        num.length <- attr(match.info[[1]], "match.length")[2]
        num <- substr(html, num.start, num.start + num.length - 1)
        file.location <- paste("http://mcdc.missouri.edu/tmpscratch/", "caps10acs", 
            num, ".csv", sep = "")
        data.newline <- tryCatch(read.csv(file = file.location), error = function(err) {
            return(NA)
        }, warning = function(war) {
            return(NA)
        })
        radius <- radius + 1
    }
    if (is.na(data.newline)) {
        return(NA)
    }
    return(cbind(School.Code = school.code, data.newline))
}
```




```r
# Check for this file on disk. If there load it.
if (file.exists("demographics.byschool")) {
    load("demographics.byschool")
}

# If it wasn't on disk, maybe it's in memory. If not, this is a first run
first.run <- !exists("demographics.byschool")

# Initialize radius
radius <- 2

for (row in 1:nrow(agg.byschool)) {
    code <- agg.byschool$School.Code[row]
    
    # Check to see if we need to get info
    if (first.run) {
        get.info <- TRUE
    } else if (code %in% demographics.byschool$School.Code) {
        get.info <- FALSE
    } else {
        get.info <- TRUE
    }
    
    data.newline <- NA
    
    # Get new info
    if (get.info) {
        lat <- agg.byschool$lat[row]
        lon <- agg.byschool$lon[row]
        data.newline <- get.demographics(code, lat, lon, radius)
    }
    
    # Check to see if we have new information
    new.info <- !is.na(data.newline)
    
    if (new.info) {
        if (first.run) {
            demographics.byschool <- data.newline
        } else {
            demographics.byschool <- rbind(demographics.byschool, data.newline)
        }
    }
    
    if (row%%10 == 0) {
        print(row)
    }
}
```

```
## [1] 10
## [1] 20
## [1] 30
## [1] 40
## [1] 50
## [1] 60
## [1] 70
## [1] 80
## [1] 90
## [1] 100
## [1] 110
## [1] 120
## [1] 130
## [1] 140
## [1] 150
## [1] 160
## [1] 170
## [1] 180
## [1] 190
## [1] 200
## [1] 210
## [1] 220
## [1] 230
## [1] 240
## [1] 250
## [1] 260
## [1] 270
## [1] 280
## [1] 290
## [1] 300
## [1] 310
## [1] 320
## [1] 330
## [1] 340
## [1] 350
## [1] 360
## [1] 370
## [1] 380
## [1] 390
## [1] 400
## [1] 410
## [1] 420
## [1] 430
## [1] 440
## [1] 450
## [1] 460
## [1] 470
## [1] 480
## [1] 490
## [1] 500
## [1] 510
## [1] 520
## [1] 530
## [1] 540
```

```r
save(demographics.byschool, file = "demographics.byschool")
```


Now need to clean up this data. First let's break it into groups:
1. Age
1. Age and Sex
1. Race
1. Hispanic or Latino (Any Race)
1. Household Income and Benefits (In 2011 Inflation-Adjusted Dollars)
1. Family Income and Benefits (In 2011 Inflation-Adjusted Dollars)
1. Other Income Measures (In 2011 Inflation-Adjusted Dollars)
1. Poverty Status Over the Last 12 Months
1. Employment Status
1. Children With All Parents Working
1. Commuting to Work
1. Workers by Occupation
1. Workers by Industry
1. Class of Worker
1. Households by Type
1. Persons by Household Type / Group Quarters
1. Relationship
1. Marital Status
1. Fertility
1. Grandparents as Caregivers
1. School Enrollment
1. Educational Attainment
1. Veteran Status
1. Residene 1 Year Ago
1. Place of Birth and Citizenship Status
1. Year of Entry
1. World Region of Birth of Foreign Born
1. Language Spoken at Home
1. Housing Occupancy and Tenure
1. Units in Structure
1. Year Structure Built
1. Year Householder Moved into Unit
1. Vehicles Available
1. House Heating Fuel
1. Selected Characteristics
1. Occupants Per Room
1. Home Values
1. Selected Monthly Owner Costs
1. Gross Rent





### Next Actions: Demographics

### Lessons Learned: Demographics

------------------------
------------------------

## Merge Scores with Demographics

### Merge Data

Prototype for understanding plot parameters

```r
s <- runif(nrow(cars), min = -100, max = 100)
c <- rep(1, length(s))
c[s >= 0] <- 2
s <- abs(s)

ggplot() + geom_point(aes(x = speed, y = dist, size = s, color = c), data = cars) + 
    geom_density2d(aes(x = speed, y = dist), data = cars)

```


First create a base map:

```r
san.diego <- get_map(location = c(-117, 32.9), zoom = 10)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=32.9,-117&zoom=10&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r
san.diego.map <- ggmap(san.diego, darken = c(0.6, "white"))
```


Let's start simple. Plot schools on map colored by whether the male score was higher or the female score. Then size point based on magnitude of difference.

```r
color <- rep("Female", nrow(agg.byschool))
color[agg.byschool$delta >= 0] <- "Male"
agg.byschool <- cbind(agg.byschool, color)

points <- geom_point(aes(x = lon, y = lat, size = abs(delta), color = color), 
    data = agg.byschool, alpha = 0.8)

san.diego.map + points + scale_size(range = c(1, 10))
```

```
## Warning: Removed 42 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 

