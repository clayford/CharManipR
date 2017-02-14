# Character Manipulation in R
# Clay Ford
# UVa Library - Research Data Services
# Spring 2017


# Packages used in this workshop

# install.packages("stringr")
# install.packages("qdapRegex")
# install.packages("pdftools")
# install.packages("lubridate")
# install.packages("dplyr")

library(stringr)
library(qdapRegex)
library(lubridate)
library(pdftools)



# Part 1 - Spruce Trees ---------------------------------------------------

# The file 139_treecores_rings.txt contains tree core and ring data for mature
# black spruce sites in interior Alaska, downloaded from the Long Term Ecological
# Research Network, http://www.lternet.edu/, in November 2014.

# http://dx.doi.org/10.6073/pasta/1b180022b278c934bdd16b633780524f

tree.dat <- read.csv("http://people.virginia.edu/~jcf2d/data/139_treecores_rings.txt", 
                     stringsAsFactors = FALSE, 
                     na.strings = "-0.999")

# stringsAsFactors = FALSE ensures all character data is read in as character
# and not factor. na.strings = "-0.999" replaces -0.999 with NA.

str(tree.dat)

# Let's look at the Date.collected column.
head(tree.dat$Date.collected)
is.character(tree.dat$Date.collected)

# Currently stored as character. We would probably want to format as date, but
# let's work with it as character for a moment.

# Let's make a new column called Year that stores the year of collection using 
# the str_sub function from the stringr package. start = -4 means start at the
# end of the string and count back 4.
tree.dat$Year <- str_sub(tree.dat$Date.collected, start = -4)

# check the result
table(tree.dat$Year)
is.character(tree.dat$Year)

# We can convert to Date class using the mdy() function in the lubridate 
# package. The Date class allows us to calculate things like elapsed number of
# days.
tree.dat$Date.collected <- mdy(tree.dat$Date.collected)
head(tree.dat$Date.collected)
is.character(tree.dat$Date.collected)

# lubridate has a function for every permutation of m, d, y

# lubridate also has functions that allow us to extract parts of a date. For 
# example, we can use year() to extract the year from a date, provided it is
# formatted as Date and nor character:
tree.dat$Year <- year(tree.dat$Date.collected)
table(tree.dat$Year)

# But notice the result is numeric instead of character
is.character(tree.dat$Year)
is.numeric(tree.dat$Year)

# This may matter depending on what we plan to do with the new Year column.

# Let's look at Site.
# Are all site codes length 7?
head(tree.dat$Site)
nchar(tree.dat$Site)
all(nchar(tree.dat$Site)==7)

# Do all follow pattern TKN0000?
all(grepl(pattern = "TKN[0-9]{4}", tree.dat$Site))

# Which ones do not follow the pattern?
grep(pattern = "TKN[0-9]{4}", tree.dat$Site, invert = TRUE, value = TRUE)


############
# YOUR TURN!
############
  
# Fix the Site column so every entry has "TKN"
 

############


# Let's check the Tree.Species column with table
table(tree.dat$Tree.Species)

# "Picgla" is short for Picea glauca (white spruce)
# "Picmar" is short for Picea mariana (black spruce)

# Say we wanted the full latin name in our data instead of the abbreviation:
tree.dat$Tree.Species <- sub(pattern = "Picgla", replacement = "Picea glauca", 
                             tree.dat$Tree.Species)
tree.dat$Tree.Species <- sub(pattern = "Picmar", replacement = "Picea mariana", 
                             tree.dat$Tree.Species)

table(tree.dat$Tree.Species)

# Another, perhaps more efficient, way:
# tree.dat$Tree.Species <- ifelse(tree.dat$Tree.Species == "Picgla", 
#                                 "Picea glauca", "Picea mariana")

# These data have two columns with comments on the condition of the inner and 
# outer cores. Let's have a look:

tree.dat$Condition.of.inner.core
tree.dat$Condition.of.outer.core

# Tabulate the comments and include NA
table(tree.dat$Condition.of.inner.core, useNA = "ifany")
table(tree.dat$Condition.of.outer.core, useNA = "ifany")

# Any leading/trailing white space?
grep(pattern = "(^ | $)", tree.dat$Condition.of.inner.core)
# use value = TRUE to see which ones
grep(pattern = "(^ | $)", tree.dat$Condition.of.inner.core, value = TRUE)

# same with outer core comments
grep(pattern = "(^ | $)", tree.dat$Condition.of.outer.core, value = TRUE)

# trim white space with trimws()
tree.dat$Condition.of.inner.core <- trimws(tree.dat$Condition.of.inner.core)
tree.dat$Condition.of.outer.core <- trimws(tree.dat$Condition.of.outer.core)


# Let's say we want to add a column to the tree.dat data frame called
# "inner.rot" that contains a logical TRUE/FALSE value based on whether or not
# the Condition.of.inner.core column contains the sequence of characters "rot",
# as in "rotten", "Rotten", "rotting", etc

# We can make life a littler easier by making everything lower case.
tree.dat$Condition.of.inner.core <- tolower(tree.dat$Condition.of.inner.core)

# Now we use a regex with grepl()
tree.dat$inner.rot <- grepl(pattern = "\\brot", tree.dat$Condition.of.inner.core)
table(tree.dat$inner.rot)

# compare inner.rot TRUEs to corresponding original comment
subset(tree.dat, inner.rot, select = c(Condition.of.inner.core, inner.rot))

# Notice sometimes "rot" is preceded by "almost". What if we don't want to
# count "rot" if it is preceded by "almost"? We can use a Negative Lookbehind.

# TIP: use the qdapRegex cheat function to see a regex cheat sheet for R!
cheat()

# select "rot" if preceded by a word boundary but not preceded by "almost ".
# Notice we have to include the space! In addition we have to set perl = TRUE!
tree.dat$inner.rot <- grepl(pattern = "(?<!almost )\\brot", 
                            tree.dat$Condition.of.inner.core, perl = TRUE)
table(tree.dat$inner.rot)

# we can compare new column to the one it was derived from
subset(tree.dat, inner.rot, select = c(Condition.of.inner.core, inner.rot))

############
# YOUR TURN!
############

# How many trees have Condition.of.outer.core containing either "good" or
# "great"? Hint: you can sum() TRUE/FALSE vectors.


############


# Part 2 - Earthquakes ----------------------------------------------------


# The "earthquakes.csv" file contains all Earthquake data recorded by the USGS
# in the last 30 days as of 09-Dec-2016, with one record per earthquake;
# downloaded from the following site:
# http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php

quake.dat <- read.csv("http://people.virginia.edu/~jcf2d/data/earthquakes.csv", 
                      stringsAsFactors = FALSE)
str(quake.dat)

# quick peek at columns of interest
summary(quake.dat$mag)
table(quake.dat$type)

# format the time column: "2016-12-09T18:50:28.140Z"

# ymd_hms() will do this for us
quake.dat$time2 <- ymd_hms(quake.dat$time)
head(quake.dat$time2)

# Let's look at the place column
head(quake.dat$place)

# Some questions we might want to answer based on information in the "place"
# column:
# - Number of earthquakes in California
# - Number of earthquakes in the USA
# - summary of distance (mean, min, max)
# - which place had the most earthquakes?

# There place pattern appears to be:
# distance in km direction of city, state (or city, country)

# Before we start, are there any missing places?
any(is.na(quake.dat$place))
any(quake.dat$place == "")

# Extract distance

# The pattern appears to be numbers at the beginning followed by "km". Is this
# pattern always true?
all(grepl(pattern = "^[0-9]+km", quake.dat$place))

# Which records do not follow this pattern?
grep(pattern = "^[0-9]+km", quake.dat$place, value = TRUE, invert = TRUE)

# Let's extract distances from the places column that follow the pattern 
# ^[0-9]+km and save to a new column called "distance"
quake.dat$distance <- str_extract(quake.dat$place, pattern = "^[0-9]+")

# sample a few and check
quake.dat[sample(nrow(quake.dat), 10), c("place","distance")]

# convert to numeric
is.character(quake.dat$distance)
quake.dat$distance <- as.numeric(quake.dat$distance)

# Let's look at a summary
summary(quake.dat$distance)

# Note we could have used lookahead. Only extract numbers followed by "km"; here
# we do it for the first 15 rows
str_extract(quake.dat$place[1:15], pattern = "^[0-9]+(?=km)")

# Notice we don't set perl=TRUE for stringr functions when using lookahead and
# lookbehind! It's not even an option!

# Extract US States

# Sometimes states are abbreviated, sometimes they're spelled out, but they
# appear to always come at the end of the string. This is good time to use R's
# built-in state.name and state.abb vectors.

state.name
state.abb

# Using paste with collapse="|" we can build a really big regular expression! 
# Notice we include the '$' to indicate the state name or abbreviation comes at
# the end of the string.
regx <- paste0('(',paste(c(state.name, state.abb), collapse = "|"),')$')

# Now use it with str_extract to extract state
quake.dat$us.state <- str_extract(quake.dat$place, pattern = regx)

# again let's sample a few and check
quake.dat[sample(nrow(quake.dat), 10), c("place","us.state")]

# How many non-missing do we have? (ie, how many earthquakes were in the US?)
sum(!is.na(quake.dat$us.state))

# proportion of earthquakes in US states:
mean(!is.na(quake.dat$us.state))

# Notice we have a mix of state names and abbreviations (well, just for CA)
table(quake.dat$us.state)

# Quick way to fix
quake.dat$us.state <- sub(pattern = "CA", replacement = "California", quake.dat$us.state)


# counts of earthquakes by state
table(quake.dat$us.state)
sort(table(quake.dat$us.state), decreasing = TRUE)
# state vs type
table(quake.dat$us.state, quake.dat$type)

# Extract the place

# Everything after "of " appears to be the place the earthquake was detected. We
# could extract the place to help us determine which place had the most
# earthquakes in our data.

# This is a good time for a look behind:
# Get one or more of everything that follows "of "
quake.dat$place2 <- str_extract(quake.dat$place, pattern = "(?<=of ).+")
# and sub CA with California
quake.dat$place2 <- sub("CA","California", quake.dat$place2)

# Which places had the most earthquakes?
sort(table(quake.dat$place2), decreasing = TRUE)[1:10]


############
# YOUR TURN!
############

# Using the place2 column, how many earthquakes did New Zealand have?


############


# Part 3 - screenplay -----------------------------------------------------

# Fantastic Mr. Fox (2009)

# Below we work with a screenplay. The goal is to create a data frame with one
# row per character line, with a column for the character, a column for the
# line, and a column for the word count

# Something like this

# FOX       Should we take the short cut or the scenic route?  10
# MRS. FOX  Let's take the short cut.                           5

# This is a PDF, so we'll use the pdf_text function from the pdftools package to
# help us read it in. We can't just use readLines()

URL2 <- "http://www.pages.drexel.edu/~ina22/splaylib/Screenplay-Fantastic_Mr_Fox.pdf"

fmf <- pdf_text(URL2)
length(fmf)

# each page is element in the vector.
# length = 103 (103 page screenplay)

# look at page 2
fmf[2]

# We can make it one line per element by string splitting at the carriage
# return/new line (\r\n) and unlisting:
fmf <- unlist(strsplit(fmf, split = "\r\n"))

# Note: I use Windows and see "\r\n"; Mac users may see just "\n"

# We can see what we've got by writing to the clipboard and pasting into a text
# file (In RStudio: File...New File...Text File)
writeClipboard(fmf)


# Pattern:
# - character name is about 20 spaces indented, ALLCAPS
# - characters' lines immediately follow and are about 9 spaces indented
# - sometimes scene direction is in the middle of dialog in parens ()

# Let's remove everything we don't want like scene directions and page numbers

# remove scene direction, which is basically any line that has text at the
# beginning of the string. We'll drop them by keeping the inverse
fmf <- grep(pattern = "^[([:alnum:]|[:punct:])]", fmf, invert = TRUE, value = TRUE)

# remove the page numbers
fmf <- grep(pattern = "^ +[0-9]{1,3}\\.$", fmf, invert = TRUE, value = TRUE)

# And let's drop the character directions in parentheses; These are strings that
# begin with 1 or more spaces followed by a left parenthesis and any character.
fmf <- grep(pattern = "^ *\\(.", fmf, invert = TRUE, value = TRUE)

# Some lines show passage of time with the word "LATER". 
grep(pattern = "LATER", fmf, value = TRUE)

# Let's drop these lines, again by keeping the inverse
fmf <- grep(pattern = "LATER", fmf, invert = TRUE, value = TRUE)

# And drop the first three lines (author, date)
fmf <- fmf[-c(1:3)]

# Sometimes dialog is continued on the next page. So a character's line is
# identified as something like FOX (cont'd). Let's remove all lines that end
# with "(cont'd)"
fmf <- grep(pattern = "\\(cont'd\\)$", fmf, invert = TRUE, value = TRUE)

# check our progress
writeClipboard(fmf)

# place character names in a vector

# the pattern is a string beginning with 12 or more spaces followed by three or
# more capital letters; we'll also go ahead and trim the whitespace
character <- trimws(grep(pattern = "^ {12,}[A-Z]{3,}", fmf, value = TRUE))

# Notice there is a lot of (O. S.) and (V. O.), to indicate voice over and off
# screen dialog.
table(character)

# Let's remove (O. S.) and (V. O.)
character <- sub(pattern = "(\\(O.S.\\)$| \\(V.O.\\)$)", 
                 replacement = "", character)

# Get character lines in a vector and have the lines paired with a character.

# PROBLEM: character lines "wrap" over multiple lines. We want to unwrap.

# For example, change this:
# I told you. You probably just ate some
# bad gristle.

# to this:
# I told you. You probably just ate some bad gristle.

# STRATEGY: write a conditional loop that wraps text (ie combines vector 
# elements) if a line is preceded by "" (ie, nothing); the corresponding regex
# is "^$"

# First replace character names in fmf vector with ""; identify element indices 
# containing the match, then use the indices to indicate location of
# substitution.
i <- grep(pattern = "^ {12,}[A-Z]{3,}", fmf)    # indices
fmf[i] <- ""

# Let's also trim white space preceding lines
fmf <- trimws(fmf)

# Create an empty character vector to store dialog
line <- character(length = length(fmf))

# set counter to 0; think of k as new line
k <- 0

# loop through each element of fmf;
# if we find nothing (""), increment k;
# If we find something, it gets pasted into the kth position
for(i in 1:length(fmf)){
  if(grepl(pattern = "^$", fmf[i])){
    k <- k + 1
  } 
  else {
    line[k] <- paste(line[k], fmf[i])
  }
}

head(line)
tail(line)

# trim whitespace and empty lines
line <- trimws(line[line!=""])

# inspect
writeClipboard(line)

# line and character should be equal in length
length(character) == length(line)

# Now place both in data frame; be sure to set stringsAsFactors = FALSE
fmfDF <- data.frame(character, line, stringsAsFactors = FALSE)

# add word count for each character line to data frame using the str_count() 
# function from the stringr package; it counts the number of strings that match
# a certain pattern. Below we count 1 or more NOT space characters

s <- c("hello, is it me you're looking for?")
str_count(s, pattern = "\\S+")

# add word count column to data frame
fmfDF$count <- str_count(fmfDF$line, pattern = "\\S+")

# total lines and word count for the characters
# Top 20
library(dplyr)
fmfDF %>% 
  group_by(character) %>% 
  summarize(totalWords = sum(count)) %>% 
  arrange(desc(totalWords)) %>% 
  top_n(n = 10, totalWords)

# all dialog by FOX
fox <- fmfDF$line[fmfDF$character=="FOX"]

