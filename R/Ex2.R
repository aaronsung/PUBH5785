# Load R libraries:
library(DescTools) # for date / time functions.
library(Hmisc) # for spss.get, Lag
library(sjmisc) # frq() function (replicating SPSS function)
library(plyr)
library(dplyr) # left_join() function # NOTE: important to load dplyr after plyr

load(file='vashmds.rda')
load(file='vasdeath.rda')
load(file='vasbirth.rda')
load(file='vascancer2.rda')

vashmds <- vashmds[order(vashmds$rootlpno,
                         vashmds$sepdate), ]

# Sort into preferred sequence:
vasdeath <- vasdeath[order(vasdeath$rootlpno),]

# Investigate people with more than one death record
vasdeath$duplicate <- duplicated(vasdeath$rootlpno)

dupes<-vasdeath[vasdeath$rootlpno %in% vasdeath[vasdeath$duplicate==1,"rootlpno"],]

# Remove the record that we don't want to keep
vasdeath2 <- vasdeath[row.names(vasdeath) %in% row.names(dupes)[1]==FALSE, ]

# Check
nrow(vasdeath) - nrow(vasdeath2)

vasdeath2[vasdeath2$rootlpno=="14171943", ] # OK - 1 record successfully removed.
# remove dupes object from workspace, duplicate column from vasdeath2
rm(dupes)
vasdeath2 <- vasdeath2[,names(vasdeath2) != "duplicate"]
# Now try merging again (Merge 1).
vaslinks <- left_join(vashmds,vasdeath2,
                      by=c("rootlpno" = "rootlpno"), keep = FALSE)
nrow(vaslinks)
nrow(vashmds)
# These should be the same.
# save at this step as "vaslinks"
save(vaslinks, file="vaslinks.rda")

#Q5

# Sort data first by rootlpno.
vascancer2 <- vascancer2[order(vascancer2$rootlpno), ]

vaslinks2 <- merge(vaslinks,vascancer2,
                      by.x="rootlpno" ,by.y="rootlpno", all.x=TRUE)

save(vaslinks2, file="vaslinks2.rda")

#Q6
#vasbirth<-vasbirth[order(vasbirth$rootlpno),]
#vasbirth$duplicate<-duplicated(vasbirth$rootlpno)

vaslinks3<-merge(vaslinks2,vasbirth,by.x="rootlpno" ,by.y="rootlpno", all.x=TRUE)

save(vaslinks3, file="vaslinks3.rda")

##
##Part B
##

#Q.8

vaslinks3$yearsep <- Year(vaslinks3$sepdate)

vaslinks3$treat <- 0
for (proc in c("proc1","proc2","proc3")){
  for(i in 1:nrow(vaslinks3)){
    if(vaslinks3$yearsep[i] < 1988 && !is.na(vaslinks3[i,proc])){
      if(vaslinks3[i,proc] == 56.36 ||
         vaslinks3[i,proc] == 59.81) vaslinks3$treat[i] <- 1
      else
        if(vaslinks3[i,proc] == 56.34 ||
           vaslinks3[i,proc] == 56.37) vaslinks3$treat[i] <- 2
    }
    else
      if(vaslinks3$yearsep[i] >= 1988 && !is.na(vaslinks3[i,proc])){
        if(vaslinks3[i,proc] >= 63.70 &&
           vaslinks3[i,proc] <= 63.79) vaslinks3$treat[i] <- 1
        else
          if(vaslinks3[i,proc] >= 63.80 &&
             vaslinks3[i,proc] <= 63.89) vaslinks3$treat[i] <- 2
      }
  } # end i loop
} # end proc loop

frq(vaslinks3$treat)

# Q9. Create fileseq variable numbering all values from 1 to 9406
vaslinks4 <- vaslinks3
vaslinks4$fileseq <- seq(1,nrow(vaslinks4))

#######################################
# Q10. Create a morbseq variable at the end of each record
morbseq.fn <- function(x){return(c(1:length(x)))}
vaslinks4 <- ddply(vaslinks4, "rootlpno", transform,
                   morbseq = morbseq.fn(rootlpno))

#######################################
# Q11. Create a vasseq variable. Set to 0 before vasectomy,
#      1 for vasectomy record and increment post vasectomy
flag1 = 0 # proxy for SPSS scratch variable
vaslinks4$vasseq <- 0
attach(vaslinks4)
for(i in 1:nrow(vaslinks4)){
  if(morbseq[i]==1) flag1 = 0
  if(flag1 == 1) vaslinks4$vasseq[i] = vaslinks4$vasseq[i-1] + 1
  else
    if(treat[i]==1 && flag1==0){
      vaslinks4$vasseq[i] = 1
     flag1 = 1
    }
  else
      if(flag1==0) vaslinks4$vasseq[i] = 0
}
detach(vaslinks4); rm(flag1)

# Inspect
stack(table(vaslinks4$vasseq,exclude=NULL))
frq(vaslinks4$vasseq)

#######################################
# Q12. # Create revseq variable. Set to 0 before vasovasostomy,
#        set to 1 for vasovasostomy record and
#        increment post vasovasostomy.
flag1 = 0 # proxy for SPSS scratch variable
vaslinks4$revseq <- 0
attach(vaslinks4)
for(i in 1:nrow(vaslinks4)){
  if(morbseq[i]==1) flag1 = 0
  if(flag1 == 1) vaslinks4$revseq[i] = vaslinks4$revseq[i-1] + 1
  else
    if(treat[i]==2 && flag1==0){
      vaslinks4$revseq[i] = 1
      flag1 = 1
    }
    else
      if(flag1==0) vaslinks4$revseq[i] = 0
}
detach(vaslinks4); rm(flag1)
# Inspect
frq(vaslinks4$revseq)
# Compare to screen in manual:
vaslinks4[1:20, which(names(vaslinks4)=="yearsep"):
            which(names(vaslinks4)=="revseq")]

#######################################
# Q13. Create dead variable. Set to 0 for records for living
#      individuals and 1 if dead.
vaslinks4$dead <- as.numeric(!is.na(vaslinks4$dthdate))
# NOTE: The operation inside the outer brackets returns
#       a logical TRUE for non-missing (NA) dthdate values,
#       FALSE otherwise, and as.numeric() coerces these values
#       to be {1, 0} respectively (as seen in previous example)
# Checks:
head(vaslinks4[vaslinks4$dead==1,c("rootlpno","dthdate")])
head(vaslinks4[vaslinks4$dead==0,c("rootlpno","dthdate")])

#######################################
# Q14. If all OK, save this file as vaslinks4
save(vaslinks4, file="vaslinks4.rda")