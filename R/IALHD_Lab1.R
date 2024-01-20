# IALHD_Lab 1.

# Ross Marriott, 29 Aug 2018.

######################################################

# # Preliminaries: Example converting spss .sav files into R data objects .rda
# 
# setwd('C:/Users/User/Documents/UWA_work/IALHD_contract/draft_scripts/Lab1')
# 
# library(haven) # for read_spss()
# 
# spss_source_file_dir <- "C:/Users/User/Documents/UWA_work/IALHD_contract/source_files/IALHD SPSS files/SPSS Data"
# 
# vashmds <- as.data.frame(read_spss(file=paste(spss_source_file_dir,"vashmds.sav",sep="/")))
# vascancer <- as.data.frame(read_spss(file=paste(spss_source_file_dir,"vascancer.sav",sep="/")))
# 
# datecols <- which(names(vashmds) %in% names(which(unlist(lapply(vashmds,function(x){class(x)=="Date"})))))
# vashmds[,datecols] <- lapply(vashmds[,datecols],function(x){as.Date(as.character(x))})
# save(vashmds,file="vashmds.rda")
# 
# datecol <- which(names(vascancer) %in% names(which(unlist(lapply(vascancer,function(x){class(x)=="Date"})))))
# vascancer[,datecol] <- as.Date(as.character(vascancer[,datecol]))
# 
# # save in R format:
# save(vashmds,file="vashmds.rda")
# save(vascancer,file="vascancer.rda")

###################################

# # Code for TS1-3:
# 
# load("vascancer.rda")
# View(vascancer)
# vascancer[21:35,1:4]

############################################################

# Training Session 1: R Syntax solutions.

#######################################
# Part A
#######################################

# Load R libraries:
library(DescTools) # for date / time functions.
library(Hmisc) # for Lag()
library(sjmisc) # frq() function (replicating SPSS function)
library(plyr)

# *** NOTE: Always note the warning messages! ***
#
# In this case, the names of some functions previously loaded are overwritten
# by functions in packages subsequently loaded (are "masked"). Not a problem
# for our labs though, as we are not using these functions. 

##########################
# Additional info on "masking" (optional)
# E.g., to find out about which of the loaded packages define a "%nin%" function
# type this into the R console:
getAnywhere("%nin%")
# For more information on any function type this into R console, e.g.
help("%nin%")
# A help page will appear with links to documentation on what this function does
# in each of the different packages. In this case it does the same thing in each
# of the loaded packages: sjmisc, Hmisc, DescTools.
# Note: to define a function for a specific package loaded can use package::function
##########################

#######################################
# Q1. Open vashmds data file using your preferred R stats software

# * * * * * * * * * * * * * 
##########################
# Instructions/tips for loading and running syntax:
##########################

# Copy the .rda files to a suitably named folder for Lab 1, which
# will be your working directory for this training session.

# Open R studio and create a new R script file (Shortcut key=Ctrl+Shift+N)
# and save it with a suitable name (e.g., IALHD_lab1.R) to this working directory.

# Either: (i) close R studio, then open this file from Windows Explorer (or using
# Finder on a Macintosh OS). R studio should open automatically with the working
# directory set for this session. Check the working directory by typing getwd()
# into the R console (lower left) pane. Or (ii) Set working directory
# path using setwd function: type help(setwd) into the R console pane, then press
# <Enter> key for details.

# Tip: type text into the top left pane (syntax editor) and save regularly.
# Send selected line or chunk of code to R using the "Run" button or suitable
# short-cut key combination (e.g., Ctrl+Enter) to execute syntax.
# * * * * * * * * * * * * * 

# Check working directory (is it right?) # Note forward slashes used in R.
getwd()

load("vashmds.rda")

# look at general properties of this dataset (I have previously assigned this
# R data object the name "vashmds")

# view first 6 rows
head(vashmds)
# view last 6 rows
tail(vashmds)
# view the type of R object that this is
class(vashmds)
# view properties of the variables in this object
summary(vashmds)
# An alternative way of viewing this 
View(vashmds) 
# (can also click the "Show in new window button in top left of this pane)
# but recommend primarily viewing R objects in the Command Window pane
# (lower left) instead.
# View the type of an individual variable / column of this data frame:
class(vashmds$admdate)

#######################################
# Q2. Provide total row count for dataset
nrow(vashmds)

#######################################
# Q3. Number of records by year.-separations
# create a new dataset so as not to overwrite the last one
vashmds1 <- vashmds
vashmds1$yearsep <- Year(vashmds1$sepdate)

# Number of hospital separations per year across the study period:
frq(vashmds1$yearsep)

#######################################
# Q4. Create morbseq variable

morbseq.fn <- function(x){return(c(1:length(x)))}
vashmds2 <- ddply(vashmds1, "rootlpno", transform,
                  morbseq = morbseq.fn(rootlpno))
# NOTE: need to ensure that data is sorted by date within
#       groupings of rootlpno first (this has been done for you here)

# visually inspect your new dataset vashmds2, e.g.
dim(vashmds2)
vashmds2[1:30,c(1:6,
                which(names(vashmds2) %in% tail(names(vashmds2))))]
# Also see Q1 for some other useful commands to inspect data 

#######################################
# Q5. How many patients in the dataset

# Create a new named object so as not to overwrite the last one
vashmds3 <- vashmds2

# Number of patients with at least one record:
length(unique(vashmds3$rootlpno[vashmds3$morbseq==1]))

# There are 2933 patients with at least one record

# This is one way to calculate the number of patients with at least
# two records:
sum(vashmds3$morbseq==2)

# There are 1772 patients with at least 2 records
# NOTE: summing converts logical {TRUE,FALSE} to numeric {1,0} class
# automatically.

# Therefore there are this many with a single record:
2933 - 1772

#######################################
# Q6. Descriptives for inpatient data set
summary.stats1 <- function(x){round(c("N"=length(x),"Min"=min(x),
                                      "Max"=max(x),"Mean"=mean(x),
                                      "Std. Dev."=sd(x)),3)}
summary.stats1(vashmds3[vashmds3$morbseq==1,"age"])

#######################################
# Q7. Descriptives for number of admissions per patient

vashmds4 <- ddply(vashmds3, "rootlpno", transform, 
                  totrec = max(morbseq))

# Check:
vashmds4[1:30,c(1:6,
                which(names(vashmds4) %in% tail(names(vashmds4))))]
# Tip: you could write a function to replicate this line of code
# with the single input of data frame if desired.

summary.stats2 <- function(x){round(c("N valid"=sum(!is.na(x)),"N missing"=sum(is.na(x)),
                                      "Mean"=mean(x,na.rm=TRUE),"Median"=median(x,na.rm=TRUE),
                                      "Std. Dev."=sd(x,na.rm=TRUE),
                                      "Range"=diff(range(x,na.rm=TRUE)),
                                      "Min"=min(x,na.rm=TRUE),"Max"=max(x,na.rm=TRUE)),3)}
summary.stats2(vashmds4[vashmds4$morbseq==1,"totrec"])

# NOTE 1: you can add / remove any functions to summary.stats2 as desired to customise
# NOTE 2: it is good practice to consider how you wish to treat missing values (NAs)
# for calculating various summary statistics. Here setting the "na.rm" argument to TRUE
# ignores them. For any of the in-built functions you can type help() to obtain further details

# Now save this renamed and modified data frame for later use
save(vashmds4, file="vashmds2.rda") # an R data object, to be retrieved using load()

#######################################
# Part B
#######################################

#######################################
# Q8. Open vascancer data using your preferred stats software

load("vascancer.rda")

# Inspect
nrow(vascancer)
dim(vascancer)
head(vascancer)
class(vascancer$candate) # etc.

# Sort into preferred sequence, and save in a different named dataframe:
vasc.a <- vascancer[order(vascancer$rootlpno,vascancer$candate),]

#######################################
# Q9. Create morbidity sequence variable
vasc.a <- ddply(vasc.a, "rootlpno", transform,
                     morbseq = morbseq.fn(rootlpno))
# NOTE: morbseq.fn is the same as that previously created.

# Check:
vasc.a[20:33,]

# NOTE: If you don't specify which columns to print, R will print
# all of them (in this case there are 5, which is not too many)

#######################################
# Q10. Run a frequency on morbseq
frq(vasc.a$morbseq)

#######################################
# Q11. Reconstruct file so that one record per person, containing all
#      cancer data i.e., long to wide transformation.

# Firstly rename it (create a copy with a different name)
vasc.b <- vasc.a

# Assign first and second cancer details to new EOR variables
vasc.b$cansite1 <- ifelse(vasc.b$morbseq==1,vasc.b$cansite,Lag(vasc.b$cansite))
vasc.b$cantis1 <- ifelse(vasc.b$morbseq==1,vasc.b$cantis,Lag(vasc.b$cantis))
vasc.b$candate1 <- as.Date(ifelse(vasc.b$morbseq==1,as.character(vasc.b$candate),
                           as.character(Lag(vasc.b$candate))))
vasc.b$cansite2 <- ifelse(vasc.b$morbseq==2,vasc.b$cansite,NA)
vasc.b$cantis2 <- ifelse(vasc.b$morbseq==2,vasc.b$cantis,NA)
vasc.b$candate2 <- as.Date(ifelse(vasc.b$morbseq==2,as.character(vasc.b$candate),NA))

# NOTE: manipulations of date objects can be tricky when using ifelse() - by converting to
# character class in the nested statement we ensure that the value remains the same,
# prior to converting back to date class.

# Check:
vasc.b[19:28,]

# Here we delete the first time cancer records before the redundant columns, which
# may be useful for checking the "manual" row deletion step

# To manually delete the first time cancer records for 10147452 & 11177518 identify the
# corresponding unique row.names to exclude (numbers on the left of each data frame row):
vasc.b[vasc.b$rootlpno %in% c("10147452","11177518"),] # they are: 20 & 27
# Then delete like this:
vasc.c <- vasc.b[row.names(vasc.b) %in% c("20","27") == FALSE, ]

# Check:
vasc.c[19:28,]
nrow(vasc.b)
nrow(vasc.c)

# However, note in practice you should code something less specific than this.

# Delete cansite, cantis, candate, morbseq, totrec variables
vasc.c <- vasc.c[,names(vasc.c) %in% c("cansite","cantis","candate",
                                       "morbseq","totrec") == FALSE]

# NOTE: this is a similar operation to the row deletions, but it is on the column
# instead of row attributes of the data frame. Data frame attributes are indexed
# like this: data_frame_obj[rows,columns] - rows always listed before columns.

# Check data before saving:
# Check:
vasc.c[19:28,]

#######################################
# Q12. Save resulting file for next lab

# Rename it first, then save to working directory
vascancer2 <- vasc.c
save(vascancer2, file = "vascancer2.rda")
