# IALHD_Lab 3.

# Ross Marriott, 5 Sep 2018.

############################################################

# Training Session 3: R Syntax solutions.

#######################################
# Part A
#######################################

# Load R libraries:
library(DescTools) # for date / time functions.
library(Hmisc) # for spss.get, Lag
library(sjmisc) # frq() function (replicating SPSS function)
library(plyr)

# Check working directory (is it right?)
getwd()

#######################################
# Q1. Open vaslinks4 data file using your preferred software:
load("vaslinks4.rda")

#######################################
# Q2. Select records where yearsep >= 1985:
vaslinks4.1 <- vaslinks4[vaslinks4$yearsep >= 1985, ]

# Re-create the morbseq variable
morbseq.fn <- function(x){return(c(1:length(x)))}
vaslinks4.1 <- ddply(vaslinks4.1, "rootlpno", transform,
                     morbseq = morbseq.fn(rootlpno))

# Select records where morbseq==1
vaslinks4.2 <- vaslinks4.1[vaslinks4.1$morbseq==1,]

#######################################
# Q3. Get frequency distribution for these records by yearsep
frq(vaslinks4.2$yearsep)

#######################################
# Q4. Discard these subsets from the workspace and repeat for all data
rm(vaslinks4.1,vaslinks4.2)
vaslinks4.1 <- vaslinks4[vaslinks4$yearsep >= 1985 &
                         vaslinks4$morbseq == 1, ]

#######################################
# Q5. # Frequencies of first-time hospital admissions by year of separation
frq(vaslinks4.1$yearsep)

# Discard the subset to clear the workspace
rm(vaslinks4.1)

# NOTE in R we do not have to remove filters as we are working on copies
# of the original file in virtual memory. We just revert to using the
# original named object instead.

#######################################
# Q6. Create a transfer sequence variable

# vaslinks4 must be sorted by separation date within rootlpno:
vaslinks4 <- vaslinks4[order(vaslinks4$rootlpno,vaslinks4$sepdate),]

vaslinks4$transseq <- 0
vaslinks4$transseq <- ifelse(vaslinks4$morbseq >= 2 & 
                             Lag(vaslinks4$sepdate) > vaslinks4$admdate,
                             1,vaslinks4$transseq)
vaslinks4$transseq <- ifelse(vaslinks4$morbseq >= 2 & Lag(vaslinks4$septype) == 2 &
                             Lag(vaslinks4$sepdate) == vaslinks4$admdate,
                             1,vaslinks4$transseq)
attach(vaslinks4)
for(i in 1:nrow(vaslinks4)){
  if(morbseq[i] >= 2 && transseq[i] == 1 &&
     transseq[i-1] >= 1) vaslinks4$transseq[i] <- vaslinks4$transseq[i-1] + 1
}
detach(vaslinks4)

# NOTE: We use the ifelse() function for the first two conditional statements
# for increased efficiency. We increment through the rows for the third, because
# in this case the calculation is incremental.

# get frequencies for transseq to check
frq(vaslinks4$transseq)

#######################################
# Q7. Syntax to refer final separation date to other records in the transfer set

# Reverse sort vaslinks4 by fileseq
vaslinks4 <- vaslinks4[order(-vaslinks4$fileseq),]

vaslinks4$findate <- vaslinks4$sepdate
for(i in 2:nrow(vaslinks4)){
  if(vaslinks4$transseq[i-1] > 0) vaslinks4$findate[i] <- vaslinks4$findate[i-1]
}

# Return to original fileseq order
vaslinks4 <- vaslinks4[order(vaslinks4$fileseq),]

#######################################
# Q8. Create a length of stay (los) variable.

# calculate a los variable (ignoring transfers)
vaslinks4$los <- as.numeric(vaslinks4$sepdate - vaslinks4$admdate)

# Update for same-day admissions
vaslinks4$los <- ifelse(vaslinks4$sepdate==vaslinks4$admdate,
                        1,vaslinks4$los)

# Calculate a total los variable (totlos)
vaslinks4$totlos <- as.numeric(vaslinks4$findate - vaslinks4$admdate)
vaslinks4$totlos <- ifelse(vaslinks4$totlos==0,1,vaslinks4$totlos)

# Check with manual screen shots
vaslinks4[1839:1846,
          which(names(vaslinks4)=="vasseq"):
            which(names(vaslinks4)=="totlos")]

#######################################
# Q9. Descriptives on los and totlos

# Select the non-transfer records (transseq==0)
vaslinks4.1 <- vaslinks4[vaslinks4$transseq == 0, ]

# Summary statistics
summary.stats1 <- function(x){round(c("N"=length(x),"Minimum"=min(x),
                                      "Max"=max(x),"Mean"=mean(x),
                                      "Std. Deviation"=sd(x)),3)}
apply(vaslinks4.1[,c("los","totlos")],2,
      summary.stats1)

# If all OK, rename vaslinks4 and save for future use.
vaslinks5 <- vaslinks4
save(vaslinks5,file="vaslinks5.rda")