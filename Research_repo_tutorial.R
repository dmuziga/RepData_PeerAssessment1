#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
#For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



setwd("C:/Users/dannyng.DIGICORE/Google Drive/Datascience/coursera/Reproducible research/week 2/Project 1/RepData_PeerAssessment1")


## Reading the activity data
activity <- read.csv("activity.csv")

## the data with missing value
dim (activity)

# AGregate the activity by calculating the mean of each step by interval

activity2 <- aggregate(steps~interval, data = activity, FUN = mean , na.rm = TRUE)


## Now replace the missing value (NA) with the mean




        for( x in 1: nrow(activity)){
            #browser()
            print (x)
            #browser()
            print(is.na(activity$steps[x]))
            #browser()  
                if(is.na(activity$steps[x]) == TRUE){
                           #browser()
                          #get the number of the interval from the missing value                
                           NAval<-activity[x,3]
                          
                          # once we have the Interval value of the missing value , we can determine mean value from the activity2 table 
                           #browser()
                           print (NAval)
                           meanval <- subset(activity2, interval == NAval, select = steps)
                           print (meanval)
                           #browser()
                          # replace the NA of the activity by the mean of the interval
                          print (x)
                          #browser()
                          
                          print(meanval)
                          #browser()
                          print(activity[x,1])
                          #browser()
                          activity[x,1] = meanval
                           #browser()
                          print( activity[x,1])
                          #browser()
                }
        

        }
        

        
## new data dimension without the missing value

ActivityNew <- activity 

dim (ActivityNew)

## to verify if there is no more Missing Value

sum(is.na(ActivityNew))

