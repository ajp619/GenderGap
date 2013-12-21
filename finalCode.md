# Gender Bias

## Introduction
The idea for this project first came to me after hearing an observation from another parent that boys at my daughter's elementary school (Creekside Elementary - School.Code = 6117469) were scoring higher in math and science than girls. That can't be right, I thought. My daughters attend one of the top rated schools in the county; The thought that there could be a systemic issue causing girls to underperform caught my attention. What follows is an introductory exploration of this issue with the goal of identifying first, the existence of the so called "Gender Gap" and second, if it does exist, if there is any demographic information that could account for such a gap.

I will be using the following data:

* From the California Department of Education, 
http://star.cde.ca.gov/star2012/researchfilelist.aspx?ps=true&lstCounty=37&lstDistrict=&lstSchool=&lstCntyNam=San%20Diego&rf=true

> Countywide/Districtwide files
> 
> 2012 San Diego County-wide research file, All Subgroups, comma delimited (CSV; 6MB ) 
> Entity files
> 
> 2012 Entities List, fixed width (TXT; 201KB ) 
> 2012 Entities List, comma delimited (CSV; 185KB ) 
> 2012 Entities List, XML (XML; 227KB ) 
> 2012 Entities List, XSD (XSD; 1KB )

* From the University of Missouri Data Center, I used their "Circular Area Profiles (CAPS) - Version 10C" web interface to gather localized demographic information for each school in my study:
http://mcdc.missouri.edu/websas/caps10c.html

As this is in conjunction with a programming class, I will also be pointing out aspects of the code I feel are interesting. If you're not interested in the details of the data munging you can [skip ahead](#start explore).

## Required Packages

I like this bit of code that checks to see if a package is installed and if not, installs it. I wish I could remember where I found it so I could give proper credit. Probably somewhere on Stack Overflow.


```r
list.of.packages <- c("ggplot2", "ggmap", "rjson", "tm", "plyr", "stringr", 
    "RJSONIO", "RCurl", "rpart", "rattle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, 
    "Package"])]
if (length(new.packages)) install.packages(new.packages)
rm(list.of.packages, new.packages)

# Maps / Geocode
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

```r

# Regression Tree
library(rpart)
library(rattle)
```

```
## Rattle: A free graphical interface for data mining with R.
## Version 2.6.26 r77 Copyright (c) 2006-2013 Togaware Pty Ltd.
## Type 'rattle()' to shake, rattle, and roll your data.
```



## Prepare Data

### Load Raw Data 

Read in the data. One trick I like here is that I'm saving the variables scores, entities, subgroups, and tests at different stages of the analysis by writing them to file names named after the chunk in which they are defined, however, when they are loaded, they come in as they were defined. This allows me to speed up running the code without loosing the ability to get back to different states of the code.


```r
file.location <- "san diego research file/"

if (file.exists("scores.load")) {
    load("scores.load")
} else {
    scores.file.name <- "ca2012_all_37_csv_v3.txt"
    scores.path <- paste0(file.location, scores.file.name)
    scores <- read.csv(scores.path, header = TRUE, sep = ",", strip.white = TRUE)
    save(scores, file = "scores.load")
}

if (file.exists("entities.load")) {
    load("entities.load")
} else {
    entities.file.name <- "ca2012entities_csv.txt"
    entities.path <- paste0(file.location, entities.file.name)
    entities <- read.csv(entities.path, header = TRUE, sep = ",", strip.white = TRUE)
    save(entities, file = "entities.load")
}

if (file.exists("subgroups.load")) {
    load("subgroups.load")
} else {
    subgroups.file.name <- "Subgroups.txt"
    subgroups.path <- paste0(file.location, subgroups.file.name)
    subgroups <- read.csv(subgroups.path, header = TRUE, sep = ",", strip.white = TRUE)
    save(subgroups, file = "subgroups.load")
}

if (file.exists("tests.load")) {
    load("tests.load")
} else {
    tests.file.name <- "Tests.txt"
    tests.path <- paste0(file.location, tests.file.name)
    tests <- read.csv(tests.path, header = TRUE, sep = ",", strip.white = TRUE)
    save(tests, file = "tests.load")
}
```


### Clean Data

This section evolved as I did my exploratory analysis and found things that were causing problems, however, or further research, the impact of filtering out school codes not equal to 0 or 1 and removing scores from subgroup.id equal to 1 should be investigated in more detail.


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

In this section I had to give myself the following note:

>Attempting to add columns to scores data set. Starting with 4191 obs. Should be the same when I'm done.

It's easy to add rows if your data has duplicates you don't expect.


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



## Explore Test Scores
<a name="start explore"/>

Let's start by taking a look at some information from Creekside.

### Creekside delta on CST Mathematics test by grade by sex

To gain some confidence in my results I compared it to the report [here](http://star.cde.ca.gov/star2012/ViewReport.aspx?ps=true&lstTestYear=2012&lstTestType=C&lstCounty=37&lstDistrict=68296-000&lstSchool=6117469&lstGroup=8&lstSubGroup=3). You can see that the male scores match.


```r
# par(mar=c(5,4,4,2)) : default values
par(mar = c(5, 5, 4, 2))

# Filter scores to just Creekside and just the test 'CST Mathematics'

creekside.scores <- scores[scores$School.Name == "Creekside Elementary", ]

creekside.scores <- creekside.scores[!is.na(creekside.scores$Test.Name), ]
creekside.scores <- creekside.scores[order(creekside.scores$Grade, creekside.scores$All.Students), 
    ]

barplot(creekside.scores$Mean.Scale.Score, names.arg = c("2nd F", "2nd M", "3rd F", 
    "3rd M", "4th F", "4th M", "5th F", "5th M"))
```

![plot of chunk creekside](figure/creekside.png) 


That's interesting, and seems to show a gender gap in 3rd, 4th, and 5th grades. This is unexpected since Creekside is the top elementary school in the district and in my opinion has most of the attributes you would expect to see in a progressive school: fairly affluent, college educated parents, etc

```
Let's try this same metric but summarize across all schools:
```

### CST Mathematics test by sex by grade


```r

# par(mar=c(5,4,4,2)) : default values
par(mar = c(5, 4, 4, 2))

cstM.scores <- scores[order(scores$Grade, scores$All.Students), ]

tbl <- ddply(cstM.scores, .(All.Students, Grade), function(x) mean(x$Mean.Scale.Score))
tbl <- tbl[order(tbl$Grade, tbl$All.Students), ]

barplot(tbl$V1, names.arg = c("2nd F", "2nd M", "3rd F", "3rd M", "4th F", "4th M", 
    "5th F", "5th M", "6th F", "6th M", "7th F", "7th M"))
```

![plot of chunk by_grade](figure/by_grade.png) 


Interesting. Technically, the difference is still there, but I wonder if it's statistically significant.

```
Next let's see if I can break this down by school
```

### CST Mathematics test by school by sex

Create a summary of male and female scores per school. Filter out schools that do not have both a male and female score.

```r

if (file.exists("agg.byschool.by_school")) {
    load("agg.byschool.by_school")
} else {
    filter.F <- scores$All.Students == "Females"
    filter.M <- scores$All.Students == "Males"
    
    agg.F <- ddply(scores[filter.F, ], .(School.Code, School.Name, Zip.Code), 
        summarise, avg = mean(Mean.Scale.Score))
    
    agg.M <- ddply(scores[filter.M, ], .(School.Code, School.Name, Zip.Code), 
        summarise, avg = mean(Mean.Scale.Score))
    
    # Filter out schools that do not have both a male and female score
    agg.F <- agg.F[agg.F$School.Code %in% agg.M$School.Code, ]
    agg.M <- agg.M[agg.M$School.Code %in% agg.F$School.Code, ]
    
    # Check that school codes are equal. This means I can compare scores from
    # agg.F[n, ] & agg.M[n, ] If all codes are equal, combine into one data
    # frame
    if (all(agg.F$School.Code == agg.M$School.Code)) {
        agg.byschool <- data.frame(School.Code = agg.F$School.Code, School.Name = agg.F$School.Name, 
            Zip.Code = agg.F$Zip.Code, avg.M = agg.M$avg, avg.F = agg.F$avg, 
            delta = (agg.M$avg - agg.F$avg))
    } else {
        print("PROBLEM!!!!")
    }
    save(agg.byschool, file = "agg.byschool.by_school")
}
```


Let's plot this information on a map to make better sense of it. First we need the latitude and longitude of each school.

Geocode schools from agg.byschool

```r
if (file.exists("agg.byschool.geocode")) {
    load("agg.byschool.geocode")
} else {
    agg.byschool <- data.frame(agg.byschool, lon = rep(NA, nrow(agg.byschool)), 
        lat = rep(NA, nrow(agg.byschool)))
    for (i in 1:nrow(agg.byschool)) {
        # this geocode function from ggmaps is amazing!
        loc <- geocode(paste(agg.byschool$School.Name[i], agg.byschool$Zip.Code[i], 
            sep = " "))
        agg.byschool$lon[i] <- as.numeric(loc[1])
        agg.byschool$lat[i] <- as.numeric(loc[2])
    }
    save(agg.byschool, file = "agg.byschool.geocode")
}
```


Let's put these on a map for a sanity check:

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

![plot of chunk school_map](figure/school_map.png) 

23 values were not plotted. These most likely have incorrect geocodes but for now I will ignore these.


Now let's take a look at the test score deltas. To do this we will plot the data on the map again, but this time we will size the markers by the magnitude of the delta and color the marker by which sex had the higher score.

First create a base map:

```r
san.diego <- get_map(location = c(-117, 32.9), zoom = 10)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=32.9,-117&zoom=10&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r
san.diego.map <- ggmap(san.diego, darken = c(0.8))  # The darken parameter really helps
```


Now put everything together:

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

![plot of chunk map_delta](figure/map_delta.png) 


This has some surprising information for someone familiar with the layout of San Diego. Rancho Santa Fe, Del Mar, La Jolla, the 56 corridor appear to be fairly biased toward higher Male test scores, while areas south of the 8 seem to have higher Female test scores. It would be interesting to combine this information with demographic information to try to get a clearer picture.


## Demographics

The University of Missouri has a great tool for extracting demographic information by radius. We are going to take advantage of the great work they've done by submitting calls to their web api and downloading the results. They've even made the download easy by creating a csv file.


### Get Demographics

The results returned by the U of Missouri need a little cleaning. Specifically, the results are character vectors that format numbers with "," and "$". We need to be able to remove those and convert the results to numeric. The only columns we want to keep in character format are "sitename" and "period".

```r
clean.df <- function(some.df) {
    for (r in 1:nrow(some.df)) {
        some.df[r, ] <- gsub(pattern = "[\\$|,]", replacement = "", x = some.df[r, 
            ])
    }
    numeric.columns <- !names(some.df) %in% c("sitename", "period")
    some.df[, numeric.columns] <- as.numeric(some.df[, numeric.columns])
    return(some.df)
}
```



This function retrieves demographic information from mcdc.missouri.edu based on lat, lon, and radius. If no information exists the radius will be increased (up to 10 in steps of 1) until information is found. If still no information is available, NA is returned.

Code note: Creating the radius expansion feature and the error checking feature really helped automate this process.

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
        data.newline <- tryCatch(read.csv(file = file.location, colClasses = "character"), 
            error = function(err) {
                return(NA)
            }, warning = function(war) {
                return(NA)
            })
        radius <- radius + 1
    }
    if (all(is.na(data.newline))) {
        return(NA)
    }
    
    data.newline <- clean.df(data.newline)
    return(cbind(School.Code = school.code, data.newline))
}
```


This section puts it all together by going through each school.code from agg.byschool and looking up the associated demographic information from the University of Missouri tool.

Code note: This section takes about 30 minutes to run through the ~540 schools. Printing the row every 10 iterations was a great way to make sure things were progressing and not hung up on something. I also save the results every 10 iterations so I don't loose everything if there is a hang up. This works because I implemented this in a way that it can pick up where it left off if necessary.

```r
# Check for this file on disk. If there load it.
if (file.exists("demographics.byschool")) {
    load("demographics.byschool")
}
```


This section can be run manually to update the data frame demographics.byschool.

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
    new.info <- !(all(is.na(data.newline)))
    
    if (new.info) {
        if (first.run) {
            demographics.byschool <- data.newline
            first.run <- FALSE
        } else {
            demographics.byschool <- rbind(demographics.byschool, data.newline)
        }
    }
    
    if (row%%10 == 0) {
        print(row)
        save(demographics.byschool, file = "demographics.byschool")
    }
}
save(demographics.byschool, file = "demographics.byschool")
```



### Merge Scores with Demographics

Looking at the lat and lon of the schools in agg.byschool that are not in demographics.byschool it is clear that something is wrong with the geocodes of those schools, which should be in the range lat:(32.5, 33.5) and lon:(-117.5, -116). There's only a few so let's just get rid of them.

```r
# Which schools are in agg.byschool but not demographics.byschool
agg.byschool[which(!(agg.byschool$School.Code %in% demographics.byschool$School.Code)), 
    ]
```

```
##     School.Code          School.Name Zip.Code avg.M avg.F   delta      lon
## 105     6037790  Magnolia Elementary    92009 438.8 430.7   8.125  121.053
## 140     6038210          Mission Mid    92026 345.3 355.3 -10.000   -2.345
## 161     6038459 Highlands Elementary    91941 387.5 373.9  13.620 -115.735
## 173     6038590 Spring Valley Middle    91941 348.0 346.0   2.000   -6.562
## 253     6039630           Fulton K-8    92114 350.7 335.8  14.875  -99.985
## 263     6039754    Horton Elementary    92102 353.1 362.3  -9.240 -112.055
## 278     6039929          Perkins K-8    92113 357.3 344.0  13.350  114.193
## 328     6040646     Olive Elementary    92084 369.5 368.1   1.425  121.015
## 333     6059604                 Dana    92106 385.1 389.5  -4.350  106.771
## 500     6114169 Discovery Elementary    92069 446.1 446.4  -0.275 -122.858
## 511     6115570               Museum    92103 384.6 361.9  22.683    8.916
## 529     6118970             Heritage    91913 448.5 442.2   6.260  -80.718
##        lat  color
## 105 14.344   Male
## 140 51.361 Female
## 161 49.503   Male
## 173 52.501   Male
## 253 20.399   Male
## 263 53.493 Female
## 278 22.269   Male
## 328 14.362   Male
## 333 -6.159 Female
## 500 49.200 Female
## 511 50.109   Male
## 529 42.754   Male
```

```r

# Remove these schools from agg.byschool
agg.byschool <- agg.byschool[which(agg.byschool$School.Code %in% demographics.byschool$School.Code), 
    ]

# Now merge agg.byschool with demographics.byschool into agg.byschool.
# Remove duplicate columns
dup.names <- c("sitename", "Longitude", "Latitude")
agg.byschool <- merge(x = agg.byschool, y = demographics.byschool[, !(names(demographics.byschool) %in% 
    dup.names)], all.x = TRUE)
```


As a final step, let's add a column called bias that summarizes which sex had the higher test score.

```r
bias <- rep("Male", times = nrow(agg.byschool))
bias[agg.byschool$delta < 0] <- "Female"
bias <- as.factor(bias)
agg.byschool <- cbind(agg.byschool, bias = bias)
```


## Final Analysis

### Regression Tree
Now that we have all this merged data, let's see what a regression tree can make of it.

```r

remove.names <- c("delta", "avg.M", "avg.F", "School.Name", "color")
filter <- !(names(agg.byschool) %in% remove.names)

form = formula(bias ~ .)

tree.m <- rpart(form,
                 data=agg.byschool[,filter],
                 method="class",
                 parms=list(split="information"),
                 control=rpart.control(usesurrogate=2,  # How to treat missing values
                                       maxsurrogate=0,
                                       minsplit=30,
                                       maxdepth=20))
drawTreeNodes(tree.m)
eval(parse(text=genPlotTitleCmd("Characteristics Prediting Bias")))
```

![plot of chunk tree](figure/tree.png) 

```r
print(tree.m)
```

```
## n= 530 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##    1) root 530 209 Male (0.39434 0.60566)  
##      2) pctProfessional< 18.85 483 203 Male (0.42029 0.57971)  
##        4) pctWalkToWork< 0.45 12   1 Female (0.91667 0.08333) *
##        5) pctWalkToWork>=0.45 471 192 Male (0.40764 0.59236)  
##         10) MovedIn2005orLater>=6058 342 157 Male (0.45906 0.54094)  
##           20) pctDiffHouse>=25.85 11   0 Female (1.00000 0.00000) *
##           21) pctDiffHouse< 25.85 331 146 Male (0.44109 0.55891)  
##             42) pctLaborForce>=59.5 319 146 Male (0.45768 0.54232)  
##               84) pctInElementary>=37.75 161  72 Female (0.55280 0.44720)  
##                168) HUsNoMort< 4166 147  60 Female (0.59184 0.40816)  
##                  336) pctTransOccs< 13.25 111  37 Female (0.66667 0.33333)  
##                    672) WalkToWork>=518 51   8 Female (0.84314 0.15686) *
##                    673) WalkToWork< 518 60  29 Female (0.51667 0.48333)  
##                     1346) pctAge45_54>=13.05 49  19 Female (0.61224 0.38776)  
##                       2692) pctDiffHouseSameCounty>=8.75 39  11 Female (0.71795 0.28205) *
##                       2693) pctDiffHouseSameCounty< 8.75 10   2 Male (0.20000 0.80000) *
##                     1347) pctAge45_54< 13.05 11   1 Male (0.09091 0.90909) *
##                  337) pctTransOccs>=13.25 36  13 Male (0.36111 0.63889)  
##                    674) DiffCntySameState< 696 21   9 Female (0.57143 0.42857) *
##                    675) DiffCntySameState>=696 15   1 Male (0.06667 0.93333) *
##                169) HUsNoMort>=4166 14   2 Male (0.14286 0.85714) *
##               85) pctInElementary< 37.75 158  57 Male (0.36076 0.63924)  
##                170) Hval500>=2260 137  57 Male (0.41606 0.58394)  
##                  340) School.Code< 6.04e+06 87  42 Female (0.51724 0.48276)  
##                    680) pctAge60_64< 5.15 70  28 Female (0.60000 0.40000)  
##                     1360) Women20to34< 685.5 24   3 Female (0.87500 0.12500) *
##                     1361) Women20to34>=685.5 46  21 Male (0.45652 0.54348)  
##                       2722) AvgHHSuppSecInc< 9687 35  15 Female (0.57143 0.42857)  
##                         5444) School.Code< 6.039e+06 13   1 Female (0.92308 0.07692) *
##                         5445) School.Code>=6.039e+06 22   8 Male (0.36364 0.63636) *
##                       2723) AvgHHSuppSecInc>=9687 11   1 Male (0.09091 0.90909) *
##                    681) pctAge60_64>=5.15 17   3 Male (0.17647 0.82353) *
##                  341) School.Code>=6.04e+06 50  12 Male (0.24000 0.76000) *
##                171) Hval500< 2260 21   0 Male (0.00000 1.00000) *
##             43) pctLaborForce< 59.5 12   0 Male (0.00000 1.00000) *
##         11) MovedIn2005orLater< 6058 129  35 Male (0.27132 0.72868)  
##           22) pctBuiltBefore1940>=1.85 40  20 Female (0.50000 0.50000)  
##             44) HawnPI2>=94 10   0 Female (1.00000 0.00000) *
##             45) HawnPI2< 94 30  10 Male (0.33333 0.66667) *
##           23) pctBuiltBefore1940< 1.85 89  15 Male (0.16854 0.83146)  
##             46) pctRetailTrade< 10.15 30  12 Male (0.40000 0.60000)  
##               92) pctGovWorkers>=17.1 12   3 Female (0.75000 0.25000) *
##               93) pctGovWorkers< 17.1 18   3 Male (0.16667 0.83333) *
##             47) pctRetailTrade>=10.15 59   3 Male (0.05085 0.94915) *
##      3) pctProfessional>=18.85 44   5 Male (0.11364 0.88636) *
```


### Scatter Plots
The most interesting thing that comes out of this is that the first split is on [pctProfessional](#pctProfessional) which, as we can see below, is pretty well correlated to Average Household Income which is what I suspected above.

```r
plot(agg.byschool$pctProfessional ~ agg.byschool$AvgHHInc)
lm.pctp <- lm(agg.byschool$pctProfessional ~ agg.byschool$AvgHHInc)
abline(lm.pctp, col = "red")
```

![plot of chunk professional_vs_income](figure/professional_vs_income.png) 


So let's take a look at some plots of test score delta against both [pctProfessional](#pctProfessional) and Average Household Income. For the pctProfessional plot I will add in the split line determined by the regression tree. For the Average Household Income plot, I chose to add in a line at $110,000. Both plots show that above these values, there is a clear tendancy for Males to have higher test scores.

```r
plot(agg.byschool$delta ~ agg.byschool$pctProfessional)
abline(h = 0, col = "blue")
abline(v = 18.85, col = "blue")
```

![plot of chunk scatter](figure/scatter1.png) 

```r

plot(agg.byschool$delta ~ agg.byschool$AvgHHInc)
abline(h = 0, col = "blue")
abline(v = 110000, col = "blue")
lm.income <- lm(agg.byschool$delta ~ agg.byschool$AvgHHInc)
abline(lm.income, col = "red")
```

![plot of chunk scatter](figure/scatter2.png) 


### Map

Finally, let's go back to the map we saw earlier and attempt to layer on this new information.

The below map represents each school with a bubble sized according to the difference between male and female scores. Each point is then colored by which sex scored higher. Finally, income is indicated by scaling the transparency. If higher incomes are more likely to have higher male scores, this should bias the color towards the male color.


```r
with(agg.byschool, filter <- AvgHHInc < 2e+05 & AvgHHInc > 75000)

points.m <- geom_point(aes(x = lon, y = lat, size = abs(delta), color = bias, 
    alpha = AvgHHInc), data = agg.byschool[filter, ])
s.range <- c(3, 12)
san.diego.map + points.m + scale_size(range = s.range)  # + scale_colour_brewer(palette = 'Set1')
```

```
## Warning: Removed 151 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


Now let's reverse the scale to accentuate the lower incomes range. If higher incomes are more likely to have higher male scores, this should bias the color towards the female color.

```r
# Flip the income scale
points.f <- geom_point(aes(x = lon, y = lat, size = abs(delta), color = bias, 
    alpha = -AvgHHInc), data = agg.byschool[filter, ])

san.diego.map + points.f + scale_size(range = s.range)  # + scale_colour_brewer(palette = 'Set1') 
```

```
## Warning: Removed 151 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


## Conclusion

There is pretty clear evidense of a correlation between income and the liklihood that Male children score better on standardized Math tests.

## Appendix

<a name="pctProfessional"> pctProfessional is the percentage of workers in the area that are "Professional, scientific, management, and administrative"

