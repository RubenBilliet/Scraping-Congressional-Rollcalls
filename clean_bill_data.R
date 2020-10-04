###########################
###                     ###
###   CLEAN BILL DATA   ###
###                     ###
###########################


####################
### BILL SUMMARY ###
####################

get_bill_summary <- function(scraped_bill_data, year) {
        # Assign column names
        names(scraped_bill_data) <- c("Index", "Issue", "Title", "Sponsor", "Committees", "Committee_reports",
                                      "Latest_action", "Rollcall_votes", "Summary_header", "Summary_text", "Text_shown_as",
                                      "Text", "Policy_area", "Legislative_subjects", "Cosponsors", "Committees_full",
                                      "Subcommittees_full")
        
        # To get the bill summary information, it's sufficient to just extract the relevant columns
        # from the source dataframe
        bill_summary <- scraped_bill_data[, c("Issue", "Summary_header", "Summary_text")]
        
        # We finally add a column for the year
        bill_summary$Year <- year

        # We return the final result        
        return(bill_summary)
}



#################
### BILL TEXT ###
#################

get_bill_text <- function(scraped_bill_data, year) {
        # Assign column names
        names(scraped_bill_data) <- c("Index", "Issue", "Title", "Sponsor", "Committees", "Committee_reports",
                                      "Latest_action", "Rollcall_votes", "Summary_header", "Summary_text", "Text_shown_as",
                                      "Text", "Policy_area", "Legislative_subjects", "Cosponsors", "Committees_full",
                                      "Subcommittees_full")
        
        # To get the bill text, it's sufficient to just extract the relevant columns from the source dataframe
        bill_text <- scraped_bill_data[, c("Issue", "Text_shown_as", "Text")]
        
        # We finally add a column for the year
        bill_text$Year <- year
        
        # We return the final result
        return(bill_text)
}



#######################
### BILL COMMITTEES ###
#######################

get_bill_committees <- function(scraped_bill_data, year, chamber = "House") {
        # Make sure we have the right libraries available
        require(tidyverse)
        
        # Assign column names
        names(scraped_bill_data) <- c("Index", "Issue", "Title", "Sponsor", "Committees", "Committee_reports",
                                      "Latest_action", "Rollcall_votes", "Summary_header", "Summary_text", "Text_shown_as",
                                      "Text", "Policy_area", "Legislative_subjects", "Cosponsors", "Committees_full",
                                      "Subcommittees_full")
        
        # The Committees variable will need to be managed into a separate data frame, since a bill can be assigned
        # to multiple committees
        bill_committees <- scraped_bill_data[, c("Issue", "Committees")]
        # Add the year column
        bill_committees$Year <- year
        
        
        # First we split the committees into senate and house committees
        # Since our data is not consistent in ordering house and senate committees, we'll need to first split them into 
        # two general columns
        bill_committees <- separate(bill_committees, Committees,
                                    c("Committees1", "Committees2"), sep = " \\| ", fill = 'right')
        # Now we can loop over the elements and assign them to the right column
        # Again, there's certainly a much more efficient way to do this, but that will be for another day
        bill_committees$House_committees <- NaN
        bill_committees$Senate_committees <- NaN
        for (i in 1:length(bill_committees$Committees1)) {
                if (grepl("House - ", bill_committees$Committees1[i])) {
                        branch <- gsub("House - ", "", bill_committees$Committees1[i])
                        bill_committees$House_committees[i] <- branch
                } else if (grepl("Senate - ", bill_committees$Committees1[i])) {
                        branch <- gsub("Senate - ", "", bill_committees$Committees1[i])
                        bill_committees$Senate_committees[i] <- branch
                }
                
                if (grepl("House - ", bill_committees$Committees2[i])) {
                        branch <- gsub("House - ", "", bill_committees$Committees2[i])
                        bill_committees$House_committees[i] <- branch
                } else if (grepl("Senate - ", bill_committees$Committees2[i])) {
                        branch <- gsub("Senate - ", "", bill_committees$Committees2[i])
                        bill_committees$Senate_committees[i] <- branch
                } 
        }
        # We can now drop the temporary holding variables
        bill_committees <- bill_committees[,-which(names(bill_committees) %in% c("Committees1","Committees2"))]
        
        # Next we create a separate data frame for senate and house committees, since they have varying number of committees
        # per bill
        bill_house_committees <- bill_committees[, c("Issue", "Year", "House_committees")]
        bill_senate_committees <- bill_committees[, c("Issue", "Year", "Senate_committees")]
        
        # We drop the temporary combined data frame
        rm("bill_committees")
        
        # Finally we create a separate row for every committee we can find
        # We also filter out any bills that not go to a committee in the branch
        bill_house_committees <- bill_house_committees %>%
                separate_rows(House_committees, sep = "; ") %>%
                filter(House_committees != "NaN")
        bill_senate_committees <- bill_senate_committees %>%
                separate_rows(Senate_committees, sep = "; ") %>%
                filter(Senate_committees != "NaN")
        
        if (chamber == "House") {
                return(bill_house_committees)
        } else if (chamber == "Senate") {
                return(bill_senate_committees)
        } else {
                return(NaN)
        }
}



#################################
### BILL LEGISLATIVE SUBJECTS ###
#################################

get_bill_legis_subjects <- function(scraped_bill_data, year) {
        # Make sure we have the right libraries
        require(tidyverse)
        
        # Assign column names
        names(scraped_bill_data) <- c("Index", "Issue", "Title", "Sponsor", "Committees", "Committee_reports",
                                      "Latest_action", "Rollcall_votes", "Summary_header", "Summary_text", "Text_shown_as",
                                      "Text", "Policy_area", "Legislative_subjects", "Cosponsors", "Committees_full",
                                      "Subcommittees_full")
        
        # For the Legislative_subjects variable we need to create a new data frame again
        bill_legis_subjects <- scraped_bill_data[, c("Issue", "Legislative_subjects")]
        # Add year column
        bill_legis_subjects$Year <- year
        
        # And then we separate the legislative subjects in separate rows
        bill_legis_subjects <- bill_legis_subjects %>%
                separate_rows(Legislative_subjects, sep = " \\\n ")
        
        # We return the final result
        return(bill_legis_subjects)
}



#######################
### BILL COSPONSORS ###
#######################

get_bill_cosponsors <- function(scraped_bill_data, year) {
        # Make sure we have the right libraries
        require(tidyverse)
        
        # Assign column names
        names(scraped_bill_data) <- c("Index", "Issue", "Title", "Sponsor", "Committees", "Committee_reports",
                                      "Latest_action", "Rollcall_votes", "Summary_header", "Summary_text", "Text_shown_as",
                                      "Text", "Policy_area", "Legislative_subjects", "Cosponsors", "Committees_full",
                                      "Subcommittees_full")
        
        # This will be a bit more challenging, since the separator between cosponsor and sponsor date, is the same
        # as between the various cosponsors.
        bill_cosponsors <- scraped_bill_data[, c("Issue", "Cosponsors")]
        # Add the year variable
        bill_cosponsors$Year <- year
        
        # We assume that the name of the cosponsor is each time followed by their party affiliation, state and district (if applicable),
        # all between square brackets and separated by just a dash without spaces. Sometimes the square brackets are followed by and asterisk.
        # After the square brackets, we have dash (between spaces) and then we see the date on which the bill was cosponsored.
        # After the date we have another dash (between spaces) to be followed by the next cosponsor name
        # If this sequence is consistent, we can assume that the dash between the cosponsor details and the date is always preceded by either
        # a ] or a ]*
        # This could allow us to replace the dash delimiter with something else
        bill_cosponsors$Cosponsors <- gsub("\\] -|\\]\\* -", "; ", bill_cosponsors$Cosponsors)
        
        # We initiate a dplyr pipeline to clean up our cosponsor data
        bill_cosponsors <- bill_cosponsors %>%
                # Next we can separate the cosponsors into separate rows on the dashes that remain between spaces
                separate_rows(Cosponsors, sep = " - ") %>%
                # We can then filter out any records for bills without cosponsors
                filter(Cosponsors != "") %>%
                # We also filter out a few weird values that are probably mistakes from the scraping
                mutate(Cosponsors = as.character(Cosponsors)) %>%
                filter(str_detect(Cosponsors,"Rep.|Sen.")) %>%
                mutate(Cosponsors = as.factor(Cosponsors)) %>%
                # And we can split the date of the cosponsor by splitting on the semi-colon
                separate(Cosponsors, c("Cosponsor", "Cosponsor_date"), sep = "; ") %>%
                # After that we can split up the cosponsor details
                separate(Cosponsor, c("Cosponsor_name", "Cosponsor_details"), sep = " \\[") %>%
                separate(Cosponsor_details, c("Cosponsor_party", "Cosponsor_state", "Cosponsor_district"), sep = "-", fill = 'right')
        
        # We now return the final result
        return(bill_cosponsors)
}



####################
### BILL DETAILS ###
####################

get_bill_details <- function(scraped_bill_data, year) {
        # Make sure we have the right libraries
        require(tidyverse)
        
        # Assign column names
        names(scraped_bill_data) <- c("Index", "Issue", "Title", "Sponsor", "Committees", "Committee_reports",
                                      "Latest_action", "Rollcall_votes", "Summary_header", "Summary_text", "Text_shown_as",
                                      "Text", "Policy_area", "Legislative_subjects", "Cosponsors", "Committees_full",
                                      "Subcommittees_full")
        
        # Drop the unnecessary variables
        bill_details <- scraped_bill_data %>% 
                select(-Index, -Committees, -Summary_header, -Summary_text, -Text_shown_as, -Text,
                       -Legislative_subjects, -Cosponsors, -Committees_full, -Subcommittees_full)
        
        # Adding a fixed year variable, will allow us later to merge all bill_data dataframes together.
        bill_details$Year <- year
        
        # Split the Sponsor column to extract the introduction date of the bill
        bill_details <- separate(bill_details, Sponsor, c("Sponsor", "Introduction_date"), sep = "Introduced ")
        
        # Further split the resulting Sponsor column to extract the name of the sponsor
        bill_details <- separate(bill_details, Sponsor, c("Sponsor_name", "Sponsor_details"), sep = " \\[", extra = 'drop')
        
        # We extract if the sponsor is a House representative or a senator
        # There's probably more elegant ways to do this than through a for loop and nested if-statements.
        # But this will have to do for now.
        bill_details$Sponsor_role <- NaN
        for (i in 1:length(bill_details$Sponsor_name)) {
                if (grepl("Rep. ", bill_details$Sponsor_name[i]) ){
                        bill_details$Sponsor_role[i] <- "House representative"
                        bill_details$Sponsor_name[i] <- gsub("Rep. ", "", bill_details$Sponsor_name[i])
                } else if (grepl("Sen. ", bill_details$Sponsor_name[i])) {
                        bill_details$Sponsor_role[i] <- "Senator"
                        bill_details$Sponsor_name[i] <- gsub("Sen. ", "", bill_details$Sponsor_name[i])
                }
        }
        
        # Further split the resulting Sponsor details to extract the sponsor party, state and district
        # We make sure to drop anything after the closed square parentheses
        bill_details <- bill_details %>%
                separate(Sponsor_details, c("Sponsor_party", "Sponsor_state", "Sponsor_district"),sep = "-", fill = 'right') %>%
                separate(Sponsor_state, c("Sponsor_state"), sep = "\\]", extra = 'drop') %>%
                separate(Sponsor_district, c("Sponsor_district"), sep = "\\]", extra = 'drop')
        
        # Keep only the first 10 characters of the Introduction_date values
        bill_details$Introduction_date <- substr(bill_details$Introduction_date, 0, 10)
        # We won't bother here to transform the variable to a date variable.
        # This cleaning happens so the dataset can be stored in a .csv-file or a SQL database
        # The variable can be formatted correctly during analysis.
        
        # For the committee reports, we take a slightly different approach
        # Since we are not interested in the IDs of the reports, we will limit ourselves to simply counting how many reports
        # we made per branch
        # This will avoid unnecessary complication of our final data model
        bill_details$Num_house_reports <- str_count(bill_details$Committee_reports, "H. Rept.")
        bill_details$Num_senate_reports <- str_count(bill_details$Committee_reports, "S. Rept.")
        bill_details$Committee_reports <- NULL
        
        # For the Latest_action variable, we start by dropping the "(All Actions)" substring that is attached to many values
        bill_details$Latest_action <- gsub("\\(All Actions\\)", "", bill_details$Latest_action)
        
        # Next we get the branch that completed the latest action
        bill_details <- separate(bill_details, Latest_action,
                                 c("Branch_latest_action", "Latest_action"),
                                 sep = " - ", extra = 'merge', fill = 'left')
        # Removing leading and trailing whitespaces
        bill_details$Latest_action <- trimws(bill_details$Latest_action, which = "both")
        
        # Extracting the first ten characters as the latest action date
        # We'll need to be carefull to check that the assumption that the date will lead after separating the branch holds
        # up for other years as well
        bill_details$Date_latest_action <- substr(bill_details$Latest_action, 0, 10)
        bill_details$Latest_action <- substr(bill_details$Latest_action, 12, nchar(bill_details$Latest_action))
        
        # We drop another substring from the Latest_action variable where it appears and trim whitespaces again
        bill_details$Latest_action <- gsub("\\(TXT \\| PDF\\)", "", bill_details$Latest_action)
        bill_details$Latest_action <- trimws(bill_details$Latest_action, which = "both")
        
        
        # For the rollcall votes we only want to know how many roll calls there have been for the bill
        bill_details$Rollcall_votes <- gsub("There has been |There have been ", "", bill_details$Rollcall_votes)
        bill_details$Rollcall_votes <- gsub(" roll call vote| roll call votes", "", bill_details$Rollcall_votes)
        
        return(bill_details)
}
