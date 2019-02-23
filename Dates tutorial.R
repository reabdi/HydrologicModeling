# Kelly Metz/Chuck Kroll 
# Jan 20, 2014/Jan 29, 2015/Jan 27, 2016
# ERE 445/645
# Dates in R tutorial

# Description: An overview of the different date/time formats available in R,
# with examples of use.

# Variables ---------------------------------------------------------------
# avg = average pH from Metro over each quarter-year
# a1 = today's date in "Date" format
# a2 = today's date in "POSIXct" format
# a3 = today's date in "POSIXlt" format
# b1 = Sept 30, 1960 in "Date" format
# b2 = Sept 30, 1960 in "POSIXct" format
# b3 = Sept 30, 1960 in "POSIXlt" format
# c1 = Oct 1, 1960 in "Date" format
# c2 = Oct 1, 1960 in "POSIXct" format
# c3 = Oct 1, 1960 in "POSIXlt" format
# count = the number of days in each quarter for avg
# d1-d5 = dates in in various formats
# dates = array of dates for input1
# i = index variable for loops
# input1 = full data frame of Metro effluent pH
# mat = matrix with columns for dates in various R date formats
# mat2 = matrix of dates formatted in "POSIXlt"
# n = the quarter of interest, used as index
# numdays1 = the number of days with data for each month of input1
# pH = pH data from input1
# ---------------------------------------------------------------------------

rm(list=ls(all=T))

# 'Date' class --------------------------------------------------------------

# The 'Date' class has a built-in function to access today's date.
a1 <- Sys.Date()
a1

# There are also a few functions that can be used to extract certain components
# of the date.
months(a1)
weekdays(a1) 
quarters(a1)
julian(a1)

# These could be used to distinguish the start of the new water year.
b1 <- as.Date("1960-09-30")  #date of the end of 1960 water year
quarters(b1)
months(b1)
month.name
c1 <- as.Date("1960-10-01")  #date of beginning of 1961 water year
quarters(c1)
months(c1)


# A vector of these dates can be incorporated as part of a larger matrix or data
# frame, although they may show up as the number of days since 1970-01-01.
mat <- matrix(NA,3,3)
rownames(mat) <- c("a","b","c")
colnames(mat) <- c("Date","POSIXct","POSIXlt")
mat[,1] <- c(a1,b1,c1)
mat



# 'POSIXct' class -----------------------------------------------------------
#  The POSIXct data type is the number of seconds since the start of January 1, 1970. Negative numbers represent the number of 
#  seconds before this time, and positive numbers represent the number of seconds afterwards.


# The 'POSIXct' class can also access today's date AND time.
a2 <- Sys.time()
a2

# The same functions used with the 'Date' class can still be used.
months(a2)
weekdays(a2)
quarters(a2)
julian(a2)

# These can be used in much the same way
b2 <- as.POSIXct("1960-09-30")  #date of the end of 1960 water year
quarters(b2)
months(b2)
c2 <- as.POSIXct("1960-10-01")  #date of beginning of 1960 water year
quarters(c2)
months(c2)

# A vector of these dates can also be including in a larger matrix or data
# frame, although they may appear as the number of seconds since 1970-01-01
# 00:00:00 UTC
mat[,2] <- c(a2,b2,c2)
mat


# 'POSIXlt' class ---------------------------------------------------------
#  The POSIXlt data type is a list (multidimentional vector), and the entries in the list have the specific meanings

# There is no specific function (that I know of) to access todays date formatted
# in the POSIXlt class, although it readily converts from POSIXct.

a3 <- as.POSIXlt(Sys.time())
a3

# The same functions can still be used, although they may not be the best way to
# access the components for use in a code.  Instead, POSIXlt allows you to
# access the number associated with every component of a date.
months(a3)
weekdays(a3)
quarters(a3)
julian(a3)
a3$sec #seconds, 0-61 (somehow???)
a3$min #minutes, 0-59 
a3$hour #hours, 0-23
a3$mday #day of the month, 1-31
a3$mon #(whole) months after the first of the year, 0-11
a3$year #years since 1900
a3$wday #day of the week (start Sunday), 0-6
a3$yday #day of the year, 0-365
a3$isdst #daylight savings time flag, yes=(+), no=0, ?=(-)

# Notice that some of the values do not look how you'd expect
a3$mon
a3$year
a3$wday
a3$yday

# These require a quick conversion by adding the origin (usually 1)
a3$mon + 1
a3$year + 1900
a3$wday + 1
a3$yday + 1

# These capabilities open up more possibilities for how to distinguish the first
# day of the water year and leap years.
b3 <- as.POSIXlt("1960-09-30")
quarters(b3)
b3$mon + 1
b3$mday
c3 <- as.POSIXlt("1960-10-01")
quarters(c3)
c3$mon + 1
c3$mday

# One drawback is that it cannot be part of a larger matrix.
mat[,3] <- c(a3,b3,c3)
# This is because POSIXlt makes the date components into a list, so there are 
# actually 9 values behind each date. Instead you must keep the dates as a
# separate array.
mat2 <- c(a3,b3,c3)
mat2


# Adding one day to current date/time -------------------------------------

# Date uses the number of days since 1970-01-01, so to add a day:
a1 + 1

# POSIXct and POSIXlt both ultimately count to number of seconds since the
# 1970-01-01 00:00:00 UTC, so you need to add the number of seconds in a day to
# add one day:
a2 + 86400 
a3 + 86400

# POSIXlt is additionally separated into several components in a list, so you 
# can also add to the day of the month (This is more intuitive and sometimes
# makes more sense in your code):
a3$mday <- a3$mday+1
a3
a3[[1]]
a3[[2]]
a3[[3]]
a3[[4]]
a3[[5]]
a3[[6]]

# Note on different formats -----------------------------------------------

# If your date has an unusual format, you will get an error when you try 
# converting it to an R date format. To specify the format, you will need to 
# look at the help for "strptime" to find how to specify your format (look under
# "Details" section).

help(strptime)

# Most commonly used with dates (there are others):
# "%b" = Three-letter abbreviated month name
# "%B" = Full month name
# "%d" = Day of the month as decimal number (01-31)
# "%m" = Month as decimal number (01-12)
# "%Y" = Year with century (eg. 2014)
# "%y" = Year without century (00-99).  00-68 are taken to be the 21st century (eg. "68" means 2068 not 1968) and 69-99 are 20th century

# Other characters and spaces are inserted as-is.  Some examples (with POSIXlt,
# note as.Date and as.POSIXct work the same):

d1 <- "01/31/2000"
class(d1)
d1_new<-as.POSIXlt(d1, format="%m/%d/%Y")
class(d1_new)

d2 <- "January 31, 2000"
as.POSIXlt(d2, format="%B %d, %Y")

d3 <- "1:00PM Jan 31, 2000"
as.POSIXlt(d3, format="%I:%M%p %b %d, %Y")

d4 <- "01-31-1968"
as.POSIXlt(d4, format="%m-%d-%Y")

d5 <- "01-31-68"
as.POSIXlt(d5, format="%m-%d-%y")



# NOTE: if converting from a number, you will need to specify the origin and 
# possibly apply some scaling. For example, Excel's "Jan 21, 2014" is 
# represented as 41660 days since Dec 31, 1899, and with some trial-an-error you
# can get these conversions to work out:
as.Date(41660, origin="1899-12-30")
as.POSIXct(41660*86400, origin="1899-12-30 5:00 UTC")
as.POSIXlt(41660*86400, origin="1899-12-30 5:00 UTC")




# Example of Using a Date Format ------------------------------------------

# Read in the data
input1 <- read.delim(file="Metro_pH_2008.txt", header=T, sep="\t")
colnames(input1)

# convert the dates column to POSIXlt and store as a separate variable
dates <- as.POSIXlt(input1[,1], format="%d-%b-%y")

pH <- as.numeric(input1[,9])

# Find the number of days in each month that you have data for
numdays1 <- matrix(0,nrow=12, ncol=2)
for(i in seq(1,length(dates))) {
  numdays1[dates$mon[i]+1,2] <- numdays1[(dates$mon[i]+1),2] + 1
}
numdays1
numdays1[,1] <- month.name
numdays1

# Find the average over each quarter-year
avg <- matrix(0, nrow=4, ncol=2)
count <- array(0,4)
for(i in seq(1, length(dates))){
  n <- as.numeric(substr(quarters(dates[i]),2,2))
  avg[n,2] <- avg[n,2] + pH[i]
  count[n] <- count[n] + 1
}
avg[,2] <- avg[,2]/count
avg[,1] <- paste("Q",1:4, sep="")
avg
