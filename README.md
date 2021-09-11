# Google-Analytics-Spam-Traffic-Detector

If you work with Google Analytics, there’s a chance that sometimes traffic can look a little suspicious. Whether its a rise in internal traffic while everyone is WFH, testing software creating false sessions or your website being hit by bots, investigating and catching this spam traffic can be time consuming especially when you have a weekly traffic report to send out by lunchtime.


The following script aims to detect high volumes of traffic coming from single sources, this was developed after discovering an issue with potential spam traffic polluting the Google Analytics account I work on in eye-watering volumes. To monitor the attacks quickly and easily, I made this script!


## Step by step guide to detecting spam traffic in Google Analytics using GoogleAnalyticsR.

Here’s a step by step guide to an R script that detects potential spam traffic in high volumes from singular sources and devices, so you can get on with your morning.

**Step 1 — Install packages.**

We will be using two packages,

- googleAnalyticsR — To connect to the Google Analytics API to collect the data needed.
- tidyverse — To transform the data.


```
library(tidyverse)
library(googleAnalyticsR)
```

**Shout out to Mark Edmondson for GoogleAnalyticsR! - https://github.com/MarkEdmondson1234/googleAnalyticsR**

**Step 2 — Connect to Google Analytics.**

Establish a connection to the Google API using the following command.


```
ga_auth()
```

This command will attempt to open up your web browser, if this doesn’t happen automatically, open up your browser. Make sure you are already logged into the Google account you use for Analytics. The browser will show an access message, click the account that you use for Google Analytics, then click allow.


**Step 3 — Select the Google Analytics view of interest.**


The following command will pull a list of all the accounts you have access to, you can then use this to find the view ID for the view of interest.
Paste or type the ID to store this for later use.


```
accountlist <- ga_account_list()
ga_id <- PASTE YOUR VIEW ID HERE
```

**Step 4 — Call the API to collect the data.**

The below script will call the API to collect the data needed for the Spam Traffic Detector. Be sure to set the date range to the range of your choice. The script contains an anti_sample clause and will keep attempting to pull the data if it samples. The data pull will be split into batches of 10 to aid in anti sampling, increase this as needed if you are working with a very large dataset.


We will be pulling Sessions and Bounce Rate by date, hour, user type, day of the week name, city, campaign, browser, device category and hostname.
You can change the dimensions to suit your business such as using source/medium instead of campaign or a custom dimension.


```
CollectedData <- google_analytics(
                       ga_id,
                       date_range = c(“2020–11–02”, “2020–11–02”),
                       metrics = c(
                                   “sessions”,
                                   “bounceRate”
                                   ),
                       dimensions = c(
                                       “date”,
                                       “hour”,
                                       “userType”,
                                       “dayOfWeekName”,
                                       “city”,
                                       “campaign”,
                                       “browser”,
                                       “deviceCategory”,
                                       “hostname”
                                        ),
                   anti_sample = TRUE,
                   anti_sample_batch = 10
                                     )
```

**Step 5 — Transform the data.**

First, we need to filter the collected data for new users, typically spam traffic generated by bots or testing software will will appear in Google Analytics as a new user.


```
CleanData <- CollectedData %>% filter(userType == “New Visitor”)
```

Now we sort the filtered data by the highest number of sessions which is then ranked by a unique key concatenated of the following dimensions; date, city, browser, campaign, device and hostname which will be used later to determine whether or not the traffic is spam. If you look at the raw data at this point you may see that bounce rate for the highest ranked unique keys is 99%+.

```
CleanData <-
            CleanData %>% arrange(desc(sessions)) %>% group_by(date)     
                      %>% mutate(rank = row_number(),uniquekey =  
                          paste(date, city, browser, campaign,   
                          deviceCategory, hostname))
```


From the CleanData which has now been ranked, we need to select the top 5 ranking spam traffic strings that have more than X sessions, you can alter this fit your traffic volumes. For me, sessions from a single source over 300 would be suspicious.


```
Spam <- 
    CleanData %>% filter(rank == 1 | rank == 2 | rank == 3 | rank ==
                  4 | rank == 5, sessions > 300) %>% 
                  arrange(desc(date))
```


Next we need to aggregate the spam sessions by unique key and calculate the start and end times of the ‘spam attack’, this will help with identifying the data in Google Analytics and removing the from your reports. This is joined on to the main subset of data containing the un-concatenated unique key values, this creates a table containing all potential spam traffic.



```
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
       Spam %>% distinct(uniquekey, .keep_all = TRUE) 
            %>% select (
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
        by = “uniquekey”
        )
```

This next step tidies the table to create the final output.

```
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
                                            ) 
                                   %>% mutate(
                                           date = date.x,
                                           device = deviceCategory,                     
                                           day = dayOfWeekName)
                                             )
                                   %>% select(
                                              date,
                                              day,
                                              device,
                                              browser,
                                              city,
                                              campaign,
                                              hostname,
                                              start,
                                              finish,
                                              sessions)
```

Example of final output:

<img width="605" alt="" src="https://miro.medium.com/max/1400/1*lRd4a--cCv1LD-g4ErWhlQ.jpeg">

Using the detector, I was able to uncover some internal testing traffic that needed to be excluded from the Google Analytics account.

Happy analyzing!
