Title
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **MD** toolbar button for help on Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

Can I do a table? Seems like I had trouble with this

|  Sepal.Length|  Sepal.Width|  Petal.Length|
|-------------:|------------:|-------------:|
|           5,1|          3,5|           1,4|
|           4,9|          3,0|           1,4|
|           4,7|          3,2|           1,3|
|           4,6|          3,1|           1,5|
|           5,0|          3,6|           1,4|
|           5,4|          3,9|           1,7|

[Go Here](#start analysis)

 



```r
summary(cars)
```

```
##      speed           dist    
##  Min.   : 4.0   Min.   :  2  
##  1st Qu.:12.0   1st Qu.: 26  
##  Median :15.0   Median : 36  
##  Mean   :15.4   Mean   : 43  
##  3rd Qu.:19.0   3rd Qu.: 56  
##  Max.   :25.0   Max.   :120
```


You can also embed plots, for example:


```r
plot(cars)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



```r
n <- 50
df <- expand.grid(x = 1:n, y = 1:n)[sample(n^2, 0.5 * n^2), ]
p <- qplot(x, y, data = df, geom = "tile")
```

```
## Error: could not find function "qplot"
```

```r
p
```

```
## Error: object 'p' not found
```

```r
p + theme_nothing()
```

```
## Error: object 'p' not found
```

```r
p + theme_nothing(legend = TRUE)  # no difference
```

```
## Error: object 'p' not found
```

```r
p + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 
    0)) + theme_nothing()
```

```
## Error: object 'p' not found
```

```r

qplot(1:10, 1:10) + theme_nothing() + theme(panel.background = element_rect(fill = "black"))
```

```
## Error: could not find function "qplot"
```

```r


df$class <- factor(sample(0:1, 0.5 * n^2, replace = TRUE))
p <- qplot(x, y, data = df, geom = "tile", fill = class)
```

```
## Error: could not find function "qplot"
```

```r
p
```

```
## Error: object 'p' not found
```

```r
p + theme_nothing()
```

```
## Error: object 'p' not found
```

```r
p + theme_nothing(legend = TRUE)
```

```
## Error: object 'p' not found
```

```r

p <- p + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 
    0))
```

```
## Error: object 'p' not found
```

```r
p
```

```
## Error: object 'p' not found
```

```r
p + theme_nothing()
```

```
## Error: object 'p' not found
```

```r
p + theme_nothing(legend = TRUE)
```

```
## Error: object 'p' not found
```


U of Missouri proto:

```r

lat = 32.944139
lon = -117.077471
radius = 2.5
name = ""
url = paste("http://mcdc.missouri.edu/cgi-bin/broker", "?_PROGRAM=websas.caps10acs.sas&_SERVICE=appdev", 
    "&latitude=", lat, "&longitude=", lon, "&sitename=", name, "&radii=", radius, 
    "&dprofile=on&eprofile=on&sprofile=on&hprofile=on", "&cntypops=on&printdetail=on&_debug=", 
    sep = "")
library("RCurl")
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



```r
t <- "<p>Access the aggregated data as a csv file here: <b><a href=\"/tmpscratch/caps10acs3547.csv\">caps10acs3547.csv</a></b> <pr><hr><br>\n</html>\n<br>\n<TABLE "
pattern <- "href=\"/tmpscratch/caps10acs(.*?).csv\""
r <- regexpr(pattern = pattern, text = t)
regmatches(t, r)
```

```
## [1] "href=\"/tmpscratch/caps10acs3547.csv\""
```



```r
r = regexpr(pattern = "href=\"/tmpscratch/caps10acs(.*?).csv\"", text = html)
```

```
## Error: object 'html' not found
```

```r
regmatches(html, r)
```

```
## Error: object 'html' not found
```



http://mcdc.missouri.edu/cgi-bin/broker?_PROGRAM=websas.caps10acs.sas&_SERVICE=appdev&latitude=32.944139&longitude=-117.077471&sitename=creekside&radii=5&dprofile=on&eprofile=on&sprofile=on&hprofile=on&cntypops=on&printdetail=on&_debug=


```r
f <- "http://mcdc.missouri.edu/tmpscratch/caps10acsNA.csv"
d <- tryCatch(read.csv(f), error = function(e) {
    return("")
}, warning = function(w) {
    return("")
})
```



## <a name="start analysis"/>Analysis Here

Oops that is the bottom. 

Experimenting, I found a solution using <div/> but an obvious solution is to place your own anchor point in the page wherever you like, thus:

<a name="abcde"/>
before the line you want to 'link' to. Don't forget the quotation marks around it. Then a markdown link like:

[link text](#abcde)
anywhere in the document takes you there.

The <div/> solution inserts a "dummy" division just to add the id property, and this is potentially disruptive to the page structure, but the <a name="abcde"/> solution ought to be quite innocuous.

(PS: It might be OK to put the anchor in the line you wish to link to, as follows:

## <a name="head1"/>Heading One
but this depends on how Markdown treats this. I note, for example, the Stack Overflow answer formatter is happy with this!)
