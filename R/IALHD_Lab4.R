# IALHD_Lab 4.

# Ross Marriott, 5 Sep 2018.

############################################################

# Training Session 4: R Syntax solutions.

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
# Q2. Create a reversal indicator and survival time on each record

# Create followup variable
vaslinks4$followup <- as.Date("1996-12-31")
vaslinks4$followup <- as.Date(ifelse(vaslinks4$dead == 1 & 
                                     vaslinks4$dthdate < vaslinks4$followup,
                                     as.character(vaslinks4$dthdate), 
                                     as.character(vaslinks4$followup)))

# Option 1: Vector file approach

# (to get same result as from that implemented for SAS)

# Get date of first vasovasostomy for each rootlpno (store in vector file)
vasosub <- vaslinks4[vaslinks4$treat==2, ]
vasodates <- aggregate(vasosub$admdate,list("rootlpno"=vasosub$rootlpno),min)

# Merge data from vector file back into vaslinks4 (i.e., the vasodate)
vaslinks4 <- merge(vaslinks4,vasodates,by.x="rootlpno",by.y="rootlpno",
                   all.x = TRUE)
names(vaslinks4)[which(names(vaslinks4)=="x")] <- "vasodate"

# Important to re-sort the file after running the merge procedure
vaslinks4 <- vaslinks4[order(vaslinks4$fileseq),]

# Use vasodate info to calculate the binary indicator (reverse)
vaslinks4$reverse <- ifelse(!is.na(vaslinks4$vasodate) & 
                            vaslinks4$admdate <= vaslinks4$vasodate &
                            vaslinks4$vasodate < vaslinks4$followup, 1, 0)

# Calculate timerev

# NOTE: Take care with date-time calculations in R, especially when use in ifelse()
#       Changes to format within ifelse() can result in errors.

vaslinks4$timerev <- ifelse(vaslinks4$revseq > 1, # POST VASO
                       difftime(vaslinks4$followup,vaslinks4$admdate,units="days"),
                     (ifelse(vaslinks4$revseq == 1, # VASO
                       difftime(vaslinks4$vasodate,vaslinks4$admdate,units="days"),
                     (ifelse(is.na(vaslinks4$vasodate), # NO VASO
                       difftime(vaslinks4$followup,vaslinks4$admdate,units="days"),
                       # ELSE IS POST VASO:
                       difftime(vaslinks4$vasodate,vaslinks4$admdate,units="days"))))))

# NOTE: we can use nested ifelse() statements here - the brackets help - please make
#       sure that you place the brackets in the right places and have the right number
#       of brackets.

# Check against manual:
vaslinks4[1523:1536,
          which(names(vaslinks4)=="fileseq"):which(names(vaslinks4)=="timerev")]

# Option 2: Using upside down file

# (to get same result as from that implemented for SPSS)

vaslinks4a <- vaslinks4[,names(vaslinks4) %in% 
                          c("vasodate","reverse","timerev")==FALSE]
vaslinks4a <- vaslinks4a[order(-vaslinks4a$fileseq),]
# mimic scratch variables:
reverse <- 0; date <- as.Date("1996-12-31")
vaslinks4a$reverse <- 0
vaslinks4a$reverse[1] <- ifelse(vaslinks4a$treat[1]==2,1,0)
for(i in 2:nrow(vaslinks4a)){
  if(i>2) reverse = next_reverse
  if(vaslinks4a$rootlpno[i] != vaslinks4a$rootlpno[i-1]) reverse = 0
  if(vaslinks4a$rootlpno[i] == vaslinks4a$rootlpno[i-1] &&
     reverse==1) vaslinks4a$reverse[i] <- 1
  if(vaslinks4a$treat[i] == 2 && vaslinks4a$admdate[i] <= vaslinks4a$followup[i]){
    vaslinks4a$reverse[i] <- 1
    reverse = 1
  }
  next_reverse = reverse
}
vaslinks4a$timerev <- NA
vaslinks4a$timerev[1] <- difftime(vaslinks4a$followup[1],vaslinks4a$admdate[1],
                                  units="days")
for(i in 2:nrow(vaslinks4a)){
  if(i>2) date = next_date
  if(vaslinks4a$rootlpno[i] != vaslinks4a$rootlpno[i-1]) {
    date = vaslinks4a$followup[i]}
  if(vaslinks4a$treat[i] == 2 && vaslinks4a$admdate[i] <= vaslinks4a$followup[i]) {
    date = vaslinks4a$admdate[i]
  }
  vaslinks4a$timerev[i] <- difftime(date,vaslinks4a$admdate[i],
                                    units="days")
  next_date = date
}

rm(reverse,date)

vaslinks4a <- vaslinks4a[order(vaslinks4a$fileseq),]

vaslinks4a[1523:1536,
          which(names(vaslinks4a)=="fileseq"):which(names(vaslinks4a)=="timerev")]

# NOTE: values for reverse and timerev are the same for the index cases but not necessarily so
# for the non-index cases (i.e., where treat==2) from the two methods. Since we are using only 
# the index cases for the subsequent analysis this is not important. Note also that the code for
# Option 1 could be easily edited in order to get consistent results with those from SPSS.

#######################################
# Q3. Life table analysis of vasovasostomy:
#     Vasectomy index case, reverse as status
#     variable and timerev as time variable.

library(survival)

timerev.breaks <- seq(0,max(vaslinks4$timerev,na.rm=TRUE),by=365)

mod1 <- survfit(Surv(timerev, reverse) ~ 1, 
                data = vaslinks4[vaslinks4$vasseq == 1, ])

# NOTE: we fit the model to all data, then extract predicted values
#       for each year from the fitted model. This is not fitting to
#       aggregated data, which would be a different (simpler) approach

# For plotting the fitted curve at the end of each 365 day period
mod1.surv.yr <- summary(mod1,times=timerev.breaks)
plot(mod1.surv.yr$time,mod1.surv.yr$surv,
     ylim=c(0.975,1),type='s',
     xlab=c("Survival Time to Vasovasostomy in Days"),
     ylab=c("Cum Survival"),font.lab=2)
grid(nx=NA,ny=NULL,lwd=1,lty=2)

#######################################
# Q4. Create age group variable 
#     (less than 30 = 1, otherwise = 0)

vaslinks4$age30 <- as.numeric(vaslinks4$age<30)

#######################################
# Q5. Life table analysis of vasovasostomy by age group

mod2 <- survfit(Surv(timerev, reverse) ~ age30, 
                data = vaslinks4[vaslinks4$vasseq == 1, ])

# For plotting the fitted curve at the end of each 365 day period
mod2.surv.yr <- summary(mod2,times=timerev.breaks)

# NOTE: mod2.surv.yr is a "list" object. It is generally preferable
#       to use functions for extracting further statistics from these,
#       however instead we write some custom functions here to extract,
#       using lapply(), the relevant parts that we need for each age group:
age30.0.values <- unlist(lapply(mod2.surv.yr[which(names(mod2.surv.yr)=="strata")],
                                function(x){which(x=="age30=0")}))
age30.0 <- lapply(mod2.surv.yr[names(mod2.surv.yr) %in% c("time","surv","strata")],
                  function(x){return(x[age30.0.values])})
age30.1.values <- unlist(lapply(mod2.surv.yr[which(names(mod2.surv.yr)=="strata")],
                                function(x){which(x=="age30=1")}))
age30.1 <- lapply(mod2.surv.yr[names(mod2.surv.yr) %in% c("time","surv","strata")],
                  function(x){return(x[age30.1.values])})

# First we plot results where age30=0:
plot(age30.0$time,age30.0$surv,
     ylim=c(0.96,1),type='s',
     xlab=c("Survival Time to Vasovasostomy in Days"),
     ylab=c("Cum Survival"),font.lab=2)

# Then we add results for age30=1:
lines(age30.1$time,age30.1$surv,
      type='s', col='lightgrey',lwd=3)

# Finally we add gridlines (optional)
grid(nx=NA,ny=NULL,lwd=1,lty=2)

# And add a legend
legend("topright",
       title=expression(bold("Age group")),
       legend=c("< 30 yr","30+ yr"),
       col=c("lightgrey","black"),
       lwd=c(3,1))

# Cox regression with single factor age group
mod3 <- coxph(Surv(timerev,reverse)~age30,
              data=vaslinks4[vaslinks4$vasseq == 1, ])
summary(mod3)

#######################################
# Q6. create paternity indicator and survival time on each record

vaslinks4$patern <- ifelse(!is.na(vaslinks4$bthdate) & 
                           vaslinks4$bthdate <= vaslinks4$followup, 1, 0)

# calculate timepat: date of admission to birthdate, dthdate or 31/12/96
vaslinks4$timepat <- ifelse(vaslinks4$patern==0,
                            difftime(vaslinks4$followup,vaslinks4$admdate,
                                     units="days"),
                            difftime(vaslinks4$bthdate,vaslinks4$admdate,
                                     units="days"))

# Check:
vaslinks4[3363:3366,
          which(names(vaslinks4)=="fileseq"):
          which(names(vaslinks4)=="timepat")]

#######################################
# Q7. Life table analysis for vasovasostomy index records

mod4 <- survfit(Surv(timepat, patern) ~ 1, 
                data = vaslinks4[vaslinks4$revseq == 1, ])

timepat.breaks <- seq(0,max(vaslinks4$timepat,na.rm=TRUE),by=365)

# For plotting the fitted curve at the end of each 365 day period
mod4.surv.yr <- summary(mod4,times=timepat.breaks)

# Inspect table to get estimate
mod4.surv.yr

# And plot
plot(mod4.surv.yr$time,mod4.surv.yr$surv,
     ylim=c(0.4,1),type='s',
     xlab=c("Survival Time to Paternity on Birth Certificate"),
     ylab=c("Cum Survival"),font.lab=2)
grid(nx=NA,ny=NULL,lwd=1,lty=2)

#######################################
# Q8. Create two new variables: year1990 and elapse6

# mimic SPSS scratch variables and set for i==1:
flag <- 0; date <- as.Date("1996-12-31") 
vaslinks4$year1990 <- as.numeric(vaslinks4$yearsep >= 1990)
vaslinks4$elapse6 <- 0

for(i in 1:nrow(vaslinks4)){
  if(i>2) {flag = next_flag; date = next_date}
  if(i>1) {
    if(vaslinks4$rootlpno[i] != vaslinks4$rootlpno[i-1]) flag = 0
  } # end if 1
  if(vaslinks4$vasseq[i] == 1){
    date = vaslinks4$admdate[i]
    flag = 1
  } # end if 2
  if(vaslinks4$revseq[i] == 1 && flag == 1){
    if(difftime(vaslinks4$admdate[i],date,
                units="days") >= 2190) {
      vaslinks4$elapse6[i] <- 0
    }else{
      vaslinks4$elapse6[i] <- 1
    }
  } #end if 3
  next_flag = flag
  next_date = date
} # end i loop

rm(flag,date)

#######################################
# Q9. Multivariable Cox regression

mod5 <- coxph(Surv(timepat,patern) ~ age30 + year1990 + elapse6,
              data = vaslinks4[vaslinks4$revseq==1 &
                               vaslinks4$vasseq>=1, ])
summary(mod5)
logLik(mod5)

# Univariate age30 model
mod5.age30 <- coxph(Surv(timepat,patern) ~ age30,
                    data = vaslinks4[vaslinks4$revseq==1 &
                                     vaslinks4$vasseq>=1, ])
summary(mod5.age30)

# Univariate year1990 model
mod5.year1990 <- coxph(Surv(timepat,patern) ~ year1990,
                       data = vaslinks4[vaslinks4$revseq==1 &
                                        vaslinks4$vasseq>=1, ])
summary(mod5.year1990)

# Univariate elapse6 model
mod5.elapse6 <- coxph(Surv(timepat,patern) ~ elapse6,
                      data = vaslinks4[vaslinks4$revseq==1 &
                                       vaslinks4$vasseq>=1, ])
summary(mod5.elapse6)

# Save modified vaslinks4 file to working directory
save(vaslinks4, file="vaslinks6.rda")
