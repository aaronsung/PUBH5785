# IALHD_Lab 2.

# Ross Marriott, 3 Sep 2018.

# Preliminaries: data file conversions from SPSS format.

# library(haven)
# 
# spss_source_file_dir <- "C:/Users/User/Documents/UWA_work/IALHD_contract/source_files/IALHD SPSS files/SPSS Data"
# 
# vasdeath <- as.data.frame(read_spss(file=paste(spss_source_file_dir,"vasdeath.sav",sep="/")))
# vasbirth <- as.data.frame(read_spss(file=paste(spss_source_file_dir,"vasbirth.sav",sep="/")))
# 
# datecol <- which(names(vasdeath) %in% names(which(unlist(lapply(vasdeath,function(x){class(x)=="Date"})))))
# vasdeath[,datecol] <- as.Date(as.character(vasdeath[,datecol]))
# 
# datecol <- which(names(vasbirth) %in% names(which(unlist(lapply(vasbirth,function(x){class(x)=="Date"})))))
# vasbirth[,datecol] <- as.Date(as.character(vasbirth[,datecol]))
# 
# head(vasdeath)
# class(vasdeath$dthdate)
# head(vasbirth)
# class(vasbirth$bthdate)
# 
# # save in R format:
# save(vasdeath,file="vasdeath.rda")
# save(vasbirth,file="vasbirth.rda")

############################################################

# Training Session 2: R Syntax solutions.

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
# Q1. Open vashmds data file using your preferred software:
load("vashmds.rda")

# NOTE: the name of this data frame should be "vashmds".
# The name of the imported object (& other details) will appear in the 
# "Environment" tab of the top right pane in R studio.

# view first 12 rows and columns up to "proc3)
vashmds[1:12,c(1:which(names(vashmds)=="proc3"))]
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
dim(vashmds) # gives the number of rows and columns, respectively.

# Sort into preferred sequence:
vashmds <- vashmds[order(vashmds$rootlpno,
                         vashmds$sepdate), ]

#######################################
# Q2. Open and inspect the vasdeath and vasbirth data files as R objects:

load("vasdeath.rda")
head(vasdeath)

load("vasbirth.rda")
head(vasbirth)

#######################################
# Q3. Merge dthdate and dthcode from vasdeath onto end of each record
#     in vashmds

# Note: Usually we'd sort data, check for duplicates etc first, but 
#       that's part of the next question. In this question, we'll see
#       what happens when we don't do this.

vaslinks <- merge(vashmds,vasdeath,by.x="rootlpno",by.y="rootlpno",
                  all.x = TRUE)

nrow(vaslinks)
nrow(vashmds)

# These should be the same.

#######################################
# Q4. Why doesn't step 3 work at first?

# Because we haven't checked for duplicates.
# Sorting is also recommended prior to performing merging 
# operations, although not so problematic here.

# Note: One would expect a 'death' file to have only one record per
# rootlpno (person) but this is not the case, there are legitimate
# circumstances when a person's death is registered more than once.

# Sort into preferred sequence:
vasdeath <- vasdeath[order(vasdeath$rootlpno),]

# Investigate people with more than one death record
vasdeath$duplicate <- ifelse(row.names(vasdeath) != row.names(vasdeath)[1] & 
                             vasdeath$rootlpno == Lag(vasdeath$rootlpno),1,0)
(dupes <- vasdeath[vasdeath$rootlpno %in% vasdeath[vasdeath$duplicate==1,
                                                   "rootlpno"], ])

# In this case HMDS should be contacted to determine which is the correct
# death record for rootlpno 14171943.

# Remove the record that we don't want to keep
vasdeath2 <- vasdeath[row.names(vasdeath) %in% row.names(dupes)[1]==FALSE, ]

# Check
nrow(vasdeath) - nrow(vasdeath2)
vasdeath2[vasdeath2$rootlpno=="14171943", ] # OK - 1 record successfully removed.

# remove dupes object from workspace, duplicate column from vasdeath2
rm(dupes)
vasdeath2 <- vasdeath2[,names(vasdeath2) != "duplicate"]

# Now try merging again (Merge 1).
vaslinks <- merge(vashmds,vasdeath2,by.x="rootlpno",by.y="rootlpno",
                  all.x = TRUE)

nrow(vaslinks)
nrow(vashmds)

# These should be the same.

# save at this step as "vaslinks"
save(vaslinks, file="vaslinks.rda")

#######################################
# Q5. Add cancer info (vascancer2) to the end of the vaslinks file.

# We know that vascancer2 has one record per rootlpno so we don't need
# to look for any duplicates.

load(file="vascancer2.rda") # from previous lab

# Sort data first by rootlpno.
vascancer2 <- vascancer2[order(vascancer2$rootlpno), ]

# Merge 2.
vaslinks2 <- merge(vaslinks,vascancer2,by.x="rootlpno",by.y="rootlpno",
                   all.x = TRUE)

# Inspect for some non-missing values of dthdate and candate2 to check:
# (might want to widen the R console pane first by dragging pane borders
# with mouse)
head(vaslinks2[!is.na(vaslinks2$candate2),])
tail(vaslinks2[!is.na(vaslinks2$dthdate),])

# Check nrow should still be equal to those for vashmds
nrow(vaslinks2)==nrow(vashmds)
# should return TRUE

# save as this step as "vaslinks2"
save(vaslinks2, file="vaslinks2.rda")

#######################################
# Q6. Add birth reg. data (vasbirth) to vaslinks2 file

# Again, we know that we should sort and check for duplicates, but that's
# part of the next question so we won't do it yet.

# Merge 3.
vaslinks3 <- merge(vaslinks2,vasbirth,by.x="rootlpno",
                   by.y="rootlpno",all.x = TRUE)

# Check nrows the same?
nrow(vaslinks3)==nrow(vaslinks2)
# should return TRUE

# NOTE: sorting of vasbirth dataframe is not necessary for accurate matching
# using the merge() function in R (although it can be important with the other
# software packages).

# Inspect again to check
head(vaslinks3[!is.na(vaslinks3$bthdate),
               c(1:6,which(names(vaslinks3) %in% tail(names(vaslinks3))))])

# To check with presented image in the manual, first sort by preferred sequence
# (merge operations affect row ordering in the merged data frame - default is to
# sort by rootlpno, but not by sepdate)
vaslinks3 <- vaslinks3[order(vaslinks3$rootlpno,vaslinks3$sepdate),]
vaslinks3[1172:1190,
          which(names(vaslinks3)=="proc1"):
          which(names(vaslinks3)=="bthdate")]

#######################################
# Q7. Step 6 does not fail in the absence of sorting vasbirth beforehand,
#     although note that we did have to sort after merging because the merge
#     function only sorts on rootlpno, and so does not preserve original order
#     in vaslinks2. Type help(merge) into the R console pane and press <Enter>
#     for details.

# NOTE: This is not the only method for merging into the EOR loading area in R,
# and some other methods (not implemented here) can be used to avoid this issue

# if all OK, save at this step as "vaslinks3"
save(vaslinks3, file="vaslinks3.rda")

#######################################
# Part B
#######################################

#######################################
# Q8. Add a year of separation variable, yearsep (from sepdate)
#     Create a variable, treat, set to 1 if record mentions vasectomy and
#     set to 2 if the record mentions a vasovasostomy (& default set to 0)

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

# NOTE: In general, we seek to avoid loops where possible as these are
#       inefficient. In R, there are some alternatives to be explored
#       such as by using apply() or various functions from the plyr
#       package. Here we have used an additional i loop for the purpose of
#       approximating the script laid out for the other software packages.

#       In the above syntax an i loop is used to step through the rows
#       in each calculation step to ensure that the full set of conditional
#       statements is done for a row, prior to moving on to the next row. Some
#       alternatives to this could be writing a function to do this and using
#       apply() to replicate it across the subsetted columns proc1 ... proc3
#       and/or to explore the use of (nested) ifelse() statements for this 
#       purpose.

# Get frequencies
frq(vaslinks3$treat)

#######################################
# Q9. Create fileseq variable numbering all values from 1 to 9406
vaslinks4 <- vaslinks3

vaslinks4$fileseq <- seq(1,nrow(vaslinks4))

#######################################
# Q10. Create a morbseq variable at the end of each record

morbseq.fn <- function(x){return(c(1:length(x)))}
vaslinks4 <- ddply(vaslinks4, "rootlpno", transform,
                   morbseq = morbseq.fn(rootlpno))
# NOTE: need to ensure that data is sorted by date within
#       groupings of rootlpno first (this was done above in Q6)

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

#######################################
# Q15. Complete table in book.

####
# LEFT Column: Men who underwent vasectomy.

# Subset to records of men having first time vasectomies
vl4.vasseq1 <- vaslinks4[vaslinks4$vasseq == 1,]

# Then calculate statistics for these records:

# Mean age and standard deviation of age
mean(vl4.vasseq1$age,na.rm=TRUE)
sd(vl4.vasseq1$age,na.rm=TRUE)
# Dead frequency
frq(vl4.vasseq1$dead)
# Cancer frequencies
can1 <- as.numeric(!is.na(vl4.vasseq1$candate1))
can2 <- as.numeric(!is.na(vl4.vasseq1$candate2))
frq(data.frame(can1,can2)) # 
# Prior vasovasostomy frequency
frq(vl4.vasseq1$revseq>0)

# After first time vasectomy: vasovasostomy frequency (this is tricky)
# Do an aggregation to calculate those patients who have ever had a
# vasovasostomy and create vasovaso.ever variable.
Vasovasostomies <- aggregate(vaslinks4$treat,list("rootlpno"=vaslinks4$rootlpno),
                             function(x){as.numeric(sum(x==2)>0)})
row.names(Vasovasostomies) <- as.character(Vasovasostomies$rootlpno)
vaslinks4$vasovaso.ever <- Vasovasostomies[as.character(vaslinks4$rootlpno),"x"]
vl4.vasseq1 <- vaslinks4[vaslinks4$vasseq == 1,]

table("vasovaso.ever"=vl4.vasseq1$vasovaso.ever,
      "revseq"=vl4.vasseq1$revseq,exclude=NULL)
# We are looking for the number with vasovaso.ever==1 and revseq==0 for these
# records for men with first-time inpatient vasectomies.
# Calculate frequency and percentage for these patients where the following is TRUE
After.FirstVasect <- ifelse(vl4.vasseq1$vasovaso.ever==1 & vl4.vasseq1$revseq==0, 
                            TRUE, FALSE)
frq(After.FirstVasect)

# Clear workspace of some objects no longer needed:
rm(After.FirstVasect,Vasovasostomies)

####
# RIGHT Column: Men who underwent vasovasostomy.

# Subset to men who underwent vasovasostomy records
vl4.revseq1 <- vaslinks4[vaslinks4$revseq == 1,]

# Then calculate statistics for these records:

# Mean and standard deviation of age
mean(vl4.revseq1$age,na.rm=TRUE)
sd(vl4.revseq1$age,na.rm=TRUE)
# Death frequency
frq(vl4.revseq1$dead)
# Cancer frequencies
can1 <- as.numeric(!is.na(vl4.revseq1$candate1))
can2 <- as.numeric(!is.na(vl4.revseq1$candate2))
frq(data.frame(can1,can2))
# Number of paternity registrations (%)
patern <- as.numeric(!is.na(vl4.revseq1$bthdate))
frq(patern)








