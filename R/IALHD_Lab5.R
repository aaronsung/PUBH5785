# IALHD_Lab 5.

# Ross Marriott, 12 Sep 2018.

# # Import libraries, containing specialist functions:
# library(haven)
# 
# # Read in SPSS file.
# 
# spss_source_file_dir <- "C:/Users/User/Documents/UWA_work/IALHD_contract/source_files/IALHD SPSS files/SPSS Data"
# bphsurgery <- as.data.frame(read_spss(file=paste(spss_source_file_dir,"bphsurgery.sav",sep="/")))
# 
# # NOTE: need to fix up date formatting in order to get consistent date-time calculations with other 
# #       software packages
# datecols <- which(names(bphsurgery) %in% names(which(unlist(lapply(bphsurgery,function(x){class(x)=="Date"})))))
# bphsurgery[,datecols] <- lapply(bphsurgery[,datecols],function(x){as.Date(as.character(x))})
# 
# # save in R format:
# save(bphsurgery,file="bphsurgery.rda")

############################################################

# Training Session 5: R Syntax solutions.

# Load R libraries:
library(DescTools) # for date / time functions.
library(Hmisc) # for spss.get, Lag
library(sjmisc) # frq() function (replicating SPSS function)
library(plyr)

# Check working directory (is it right?) # Note forward slashes used in R.
getwd()

#######################################
# Q1. Open bphsurgery data file, which has been saved to your working directory
#     and familiarise yourself with its structure

load("bphsurgery.rda")

# Get dimensions of the data frame
dim(bphsurgery)
# View first 6 rows
head(bphsurgery)
# View last 6 rows
tail(bphsurgery)
# view summary statistics of all the variables
summary(bphsurgery)
##################
# NOTE: summary() is particularly useful for viewing the ranges of values &
#       the numbers of missing values ("NA's" for numeric or Date variables)
##################
# View the class of all variables ("class" = format type in R jargon)
as.data.frame(lapply(bphsurgery,class))
# Which variables are in Date format? (tricky: be careful with brackets)
names(which(unlist(lapply(bphsurgery,function(x){class(x)=="Date"}))))
# How many unique rootlpnos?
length(unique(bphsurgery$rootlpno))
# Average number of rows per roolpno
nrow(bphsurgery) / length(unique(bphsurgery$rootlpno))
# Number of unique Charlson comorbidity scores
length(unique(bphsurgery$charlson))
# View these unique values in ascending order
sort(unique(bphsurgery$charlson))

#######################################
# Q2. Create bphsurg variable

bphsurgery$yearsep <- Year(bphsurgery$sepdate)
bphsurgery$bphsurg <- NA # set default value
proc_vars <- paste("proc",seq(1,3),sep="")

# NOTE: since we are updating values of bphsurg on a row-by-row
#       basis, by referring to the corresponding row value within
#       each respective proc column in turn, it is (perhaps arguably)
#       simpler to code in an i loop to step through the rows. However
#       if we were to calculate bphsurg on only one proc column, some
#       nested ifelse() statements, to calculate all values at once,
#       might be preferable here. The issue is computing time, as 
#       determined by the size of your data set & computer processing speed.

for(proc in proc_vars){
  for(i in 1:nrow(bphsurgery)){
    if(bphsurgery$yearsep[i] < 1988 && !is.na(bphsurgery[i,proc])){
      if(bphsurgery[i,proc] == 56.01) bphsurgery$bphsurg[i] <- 1
      else
      if(bphsurgery[i,proc] >= 56.02 &&
         bphsurgery[i,proc]  < 56.06) bphsurgery$bphsurg[i] <- 0
    } # end nested if loop 1
    
    else
    if(bphsurgery$yearsep[i] >= 1988 && !is.na(bphsurgery[i,proc])){
      if(bphsurgery[i,proc] >= 60.20 &&
         bphsurgery[i,proc]  < 60.30) bphsurgery$bphsurg[i] <- 1
      else
      if(bphsurgery[i,proc] >= 60.30 &&
         bphsurgery[i,proc]  < 60.70) bphsurgery$bphsurg[i] <- 0
    } # end nested if loop 2    
  } # end i loop
} # end proc loop

# Create a variable to identify bphsurg name
bphsurgery$bphsurg_name <- ifelse(is.na(bphsurgery$bphsurg),"",
                              (ifelse(bphsurgery$bphsurg==1,"TURP","OP")))
# Check 1:
table("bphsurg_name"=bphsurgery$bphsurg_name,
      "bphsurg"=bphsurgery$bphsurg,exclude=NULL)

#######################################
# Q3. Create fileseq, morbseq, indexseq variables.

# Create fileseq (original row order)
bphsurgery$fileseq <- seq(1,nrow(bphsurgery))

# Sort into preferred sequence before creating sequence variables:
bphsurgery <- bphsurgery[order(bphsurgery$rootlpno,bphsurgery$admdate), ]
# Check: was this is already in preferred sequence?
identical(seq(1,nrow(bphsurgery)),bphsurgery$fileseq)

# Create morbseq
morbseq.fn <- function(x){return(c(1:length(x)))}
bphsurgery <- ddply(bphsurgery, "rootlpno", transform,
                    morbseq = morbseq.fn(rootlpno))

# Create indexseq (use "vector file" approach)
# Get first date of non-missing bphsurg for each rootlpno (store in vector file)
indexsub <- bphsurgery[!is.na(bphsurgery$bphsurg), ]
indexdates <- aggregate(indexsub$admdate,
                        list("rootlpno"=indexsub$rootlpno),min)

# Merge data from vector file back into bphsurgery (i.e., the indexdate)
bphsurgery <- merge(bphsurgery,indexdates,by.x="rootlpno",by.y="rootlpno",
                    all.x = TRUE)
names(bphsurgery)[which(names(bphsurgery)=="x")] <- "indexdate"
# Important to re-sort the file after running the merge procedure
bphsurgery <- bphsurgery[order(bphsurgery$fileseq), ]

bphsurgery$indexseq <- as.numeric(bphsurgery$admdate==bphsurgery$indexdate &
                                  !is.na(bphsurgery$bphsurg))
# For each rootlpno, use ddply() to fill in the indexseq starting from 1
indexseq.fn <- function(x){
  n_x <- length(x)
  indexseq <- numeric(n_x)
  i <- which(x==1)
  if(n_x > 1 && sum(x) != 0) {
    indexseq[i:n_x]=seq(1,n_x-(i-1))
  } else {
    indexseq <- x
  }  
  return(indexseq)
}
bphsurgery <- ddply(bphsurgery, "rootlpno", transform,
                    indexseq = indexseq.fn(indexseq))

#######################################
# Q4. Create dead, survival variables

bphsurgery$dead <- as.numeric(!is.na(bphsurgery$dthdate) &
                              bphsurgery$dthdate <= as.Date("1996-12-31"))

bphsurgery$followup <- as.Date("1996-12-31")
bphsurgery$followup <- as.Date(ifelse(bphsurgery$dead == 1 & 
                                      bphsurgery$dthdate < bphsurgery$followup,
                                      as.character(bphsurgery$dthdate), 
                                      as.character(bphsurgery$followup)))

bphsurgery$survival <- difftime(bphsurgery$followup,bphsurgery$admdate,
                                units="days")
bphsurgery$survival <- as.numeric(bphsurgery$survival)

# Remove redundant columns
bphsurgery <- bphsurgery[ ,names(bphsurgery) != "indexdate"]

# Check 2:
bphsurgery[2041:2047,
           which(names(bphsurgery)=="proc3"):
           which(names(bphsurgery)=="survival")]

#######################################
# Q5. Actuarial survival analysis of post-operative mortality
#     One year intervals, single factor of bphsurg

# Select the index cases
bph_indexsub <- bphsurgery[bphsurgery$indexseq==1, ]
bph_indexsub$surgery <- factor(bph_indexsub$bphsurg_name,
                               levels=c("OP","TURP"))

library(survival)

# Calculate cut points for 365-day intervals
postop_time.breaks <- seq(0,max(bph_indexsub$survival,na.rm=TRUE),by=365)

mod1 <- survfit(Surv(survival, dead) ~ surgery, data=bph_indexsub)
(mod1.surv.yr <- summary(mod1,times=postop_time.breaks))

# NOTE: enclosing an assignment statement within round brackets also
#       prints the result to the R console, which can be useful

OP.values <- unlist(lapply(mod1.surv.yr[which(names(mod1.surv.yr)=="strata")],
                                function(x){which(x=="surgery=OP")}))
OP.surv <- lapply(mod1.surv.yr[names(mod1.surv.yr) %in% c("time","surv","strata")],
                  function(x){return(x[OP.values])})
TURP.values <- unlist(lapply(mod1.surv.yr[which(names(mod1.surv.yr)=="strata")],
                                function(x){which(x=="surgery=TURP")}))
TURP.surv <- lapply(mod1.surv.yr[names(mod1.surv.yr) %in% c("time","surv","strata")],
                  function(x){return(x[TURP.values])})

plot(OP.surv$time,OP.surv$surv,
     ylim=c(0.5,1),type='s',
     xlab=c("Survival Time in Days"),
     ylab=c("Cum Survival"),font.lab=2)
lines(TURP.surv$time,TURP.surv$surv,
      type='s', col='lightgrey',lwd=3)
grid(nx=NA,ny=NULL,lwd=1,lty=2)
legend("topright",
       title=expression(bold("Surgery")),
       legend=c("OP","TURP"),
       col=c("black","lightgrey"),
       lwd=c(1,3))

# Univariate Cox regression
mod2 <- coxph(Surv(survival, dead) ~ surgery, data=bph_indexsub)
summary(mod2)

#######################################
# Q6. Compare summary statistics for patient age and sepyear

# NOTE: you could use aggregate(), or the grpmean() function from the sjstats
#       package, but here the by() function works well to get results from 
#       our user-defined function summary.stats1():

summary.stats1 <- function(x){round(c("Mean"=mean(x,na.rm=TRUE),
                                      "Std. Dev"=sd(x,na.rm=TRUE),
                                      "N"=length(x)),2)}
by(bph_indexsub$age,
   list("Surgery"=bph_indexsub$surgery),
   summary.stats1)
by(bph_indexsub$yearsep,
   list("Surgery"=bph_indexsub$surgery),
   summary.stats1)

#######################################
# Q7. Multivariable Cox regression: surgery, yearsep, fractal_poly(age)
bph_indexsub$yearsep <- as.numeric(bph_indexsub$yearsep)

# NOTE: need to enclose any arithmetic operations within model formula
#       within I() otherwise R will interpret some operators (e.g. "*") 
#       differently than intended

mod3 <- coxph(Surv(survival, dead) ~ surgery + yearsep + 
                I(age^2) + I((age^2)*log(age)), data=bph_indexsub)
summary(mod3)

#######################################
# Q8. Multivariable Cox regression 2: add fractal_poly(charlson)
#     with a valence of (-1/2,0)

bph_indexsub$chmsqr <- (bph_indexsub$charlson + 0.01)^(-0.5)
bph_indexsub$chln <- log(bph_indexsub$charlson + 0.01)
bph_indexsub$anycom <- as.factor(as.numeric(bph_indexsub$charlson>0))

mod4 <- coxph(Surv(survival, dead) ~ surgery + yearsep + 
                I(age^2) + I((age^2)*log(age)) + 
                chmsqr + chln + anycom, data=bph_indexsub)
summary(mod4)
