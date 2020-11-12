### Google Analytics Spam Traffic Detector


### ---------------------------- Run required packages --------------------------------------------------------------

library(tidyverse)
library(googleAnalyticsR)


### ----------------------------  Connect to Google Analytics -------------------------------------------------------
#    Establish a connection to the Google API. 
#    This command will attempt to open up your web browser, if this doesn’t happen automatically, open a web browser. 
#    Make sure you are already logged into the Google account you use for Analytics.
#    The browser will show an access message, click the account that you use for Google Analytics, then click allow.


ga_auth()


### ---------------------------- Find the veiw ID for the Analytics view of interest ---------------------------------

accountlist <- ga_account_list()


### ---------------------------- Add the View ID below ---------------------------------------------------------------

ga_id <- YOUR VIEW ID GOES HERE!



### ---------------------------- Call the API to collect the data ----------------------------------------------------
#    The below script will call the API to collect the data needed for the Spam Traffic Dector. 
#    Be sure to set the date range to the range of your choice. 
#    The script contains an anti_sample clause and will keep attempting to pull the data if it samples.
#    The data will be split into batches of 10 to aid in anti sampling, increase this as needed if you are working with
#    a very large dataset. 




CollectedData <- google_analytics(
                                    ga_id,
                                    
                                    date_range = c("2020-11-02", "2020-11-02"),
                                    
                                    metrics = c(
                                      "sessions",
                                      "bounceRate"
                                                 ),
                                    
                                    dimensions = c(
                                                    "date",
                                                    "hour",
                                                    "userType",
                                                    "dayOfWeekName",
                                                    "city",
                                                    "campaign",
                                                    "browser",
                                                    "deviceCategory",
                                                    "hostname"
                                                    ),
                                    
                                    anti_sample = TRUE,
                                    
                                    anti_sample_batch = 10
                                    
                                  )


### ---------------------------- Transform the data to find Spam Traffic ---------------------------------------------
#    The first step filters the collected data for new users, typically spam traffic will generate a new IP each time
#    as a result will appear in Google Analytics as a new user. 


CleanData <- CollectedData %>% filter(userType == "New Visitor")


#    Now we sort the filtered data by Highest number of sessions which is ranked by a unique key combined of date, 
#    city, browser, campaign, device and hostname which will be used later to determine whether or not the traffic
#    is Spam. If you look at the raw data at this point you may see that bounce rate for the highest ranked is always
#    99%+. 


CleanData <-
  CleanData %>% arrange(desc(sessions)) %>% group_by(date) %>% mutate(rank = row_number(),uniquekey = paste(date, city, browser, campaign, deviceCategory, hostname))


#    From the CleanData which has now been ranked, we need to select the top 5 ranking Spam traffic strings that have
#    more than 300 sessions, you can alter this to better fit your data if required.

Spam <- 
  CleanData %>% filter(rank == 1 | rank == 2 | rank == 3 | rank == 4 | rank == 5, sessions > 300) %>% arrange(desc(date))



#   Next we need to aggregate the Spam sessions by unique key and calculate the start and end times of the spam attack.
#   This is joined on to the main subset of data containing the unconcatenated unique key values, this creates a
#   table containing all potential spam traffic. 


SpamSessions <- inner_join(
  Spam %>% select(uniquekey, hour, sessions, rank, date)
  %>% group_by(date, uniquekey)
  %>% summarise(
    sessions = sum(sessions),
    start = min(hour),
    finish = max(hour)
  )
  ,
  (
    Spam %>% distinct(uniquekey, .keep_all = TRUE) %>% select (
      date,
      uniquekey,
      dayOfWeekName,
      city,
      browser,
      deviceCategory,
      campaign,
      hostname
    )
  ),
  by = "uniquekey"
) 


#   This next step tidys the table to create the final output. 

Output <- (
  Output <-
    SpamSessions %>% select(
      date.x,
      dayOfWeekName,
      deviceCategory,
      browser,
      city,
      campaign,
      hostname,
      start,
      finish,
      sessions
    ) %>% mutate(date = date.x, device = deviceCategory, day = dayOfWeekName)
) %>% select(date,
             day,
             device,
             browser,
             city,
             campaign,
             hostname,
             start,
             finish,
             sessions)


#   Use the following command to save the output as a CSV 


write.csv(output, file = "./data/SpamSessionsOutput.csv")



