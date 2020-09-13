## In this script we're going to scrape each individual roll call vote in US congress between 1990 and 2020.

scrape_votes <- function(rollcall, year) {
        # This function takes in a year (in string format) between 1990 and the current year, as well as a data frame
        # which contains a column named 'Roll_call_link', a link to a roll call vote.
        # The function returns a data frame with all the votes within a year in a particular rollcall.
        
        # First we make sure we have the right libraries
        require(XML)
        
        # We initiate empty lists to store our data, while we go through a for loop
        vote_details_list <- list()
        votes_list <- list()
        
        ## We don't want to run this scraping loop for all rollcalls, because it would take more than 25 hours.
        ## So we'll need to run this for one year at a time
        rollcall_subset <- rollcall[rollcall$Year == year,]
        
        # We initiate the for loop to go over each roll call vote
        for (i in 1:nrow(rollcall_subset)) {
                # We get the URL from the rollcall dataframe, and fetch the webpage online
                URL <- rollcall_subset$Roll_call_link[i]
                XML <- try(htmlTreeParse(URL,useInternal=TRUE))
                
                # Next we parse out the XML
                rootNode <- xmlRoot(XML)
                
                # Next we extract all the metadata about a vote
                majority <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/majority", xmlValue)
                congress <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/congress", xmlValue)
                session <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/session", xmlValue)
                chamber <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/chamber", xmlValue)
                rollcall_num <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/rollcall-num", xmlValue)
                legis_num <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/legis-num", xmlValue)
                vote_question <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/vote-question", xmlValue)
                vote_type <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/vote-type", xmlValue)
                vote_result <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/vote-result", xmlValue)
                action_date <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/action-date", xmlValue)
                action_time <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/action-time", xmlValue)
                vote_desc <- xpathSApply(rootNode,"//rollcall-vote/vote-metadata/vote-desc", xmlValue)
                
                # Control for possibility that there is no 'legislation number'
                if (is.null(legis_num[1][[1]])) {
                        legis_num = " "
                }
                
                # We combine the vote metadata into a list, and add it to our list of lists to combine the data
                rollcall_details <- c(majority, congress, session, rollcall_num, legis_num, vote_question, vote_type, 
                                      vote_result, action_date, action_time, vote_desc)
                vote_details_list[[i]] <- rollcall_details
                
                # Then we scrape each individual vote
                voter <- xpathSApply(rootNode,"//rollcall-vote/vote-data/recorded-vote/legislator", xmlValue)
                party <- xpathSApply(rootNode,"//rollcall-vote/vote-data/recorded-vote/legislator/@party")
                state <- xpathSApply(rootNode,"//rollcall-vote/vote-data/recorded-vote/legislator/@state")
                vote <- xpathSApply(rootNode,"//rollcall-vote/vote-data/recorded-vote/vote", xmlValue)
                
                # We now combine the 4 variables into a data frame, and add 2 constant variables to identify the vote
                # We do need to check and make sure that there's data in our data frame. There has been at least one
                # instance where no votes were made (rollcall 44 in 2016).
                rollcall_votes <- data.frame(cbind(voter, party, state, vote))
                if (nrow(rollcall_votes) > 0) {
                        rollcall_votes$action_date <- action_date
                        rollcall_votes$rollcall_num <- rollcall_num
                } else {
                        rollcall_votes <- data.frame(voter = NA, party = NA, state = NA, vote = NA,
                                                     action_date = action_date, rollcall_num = rollcall_num)
                }
                
                # We now assign our voting data frame to a list of data frames to combine the data
                votes_list[[i]] <- rollcall_votes
                
                # Print a message to indicate the rollcall is done
                print(paste(rollcall_num, "done.", sep = " "))
                
                # We build in a few seconds wait, to not overload the host server
                Sys.sleep(5)
        }
        vote_details <- do.call("rbind", vote_details_list)
        votes <- do.call("rbind", votes_list)
        
        return(list(vote_details, votes))
}


