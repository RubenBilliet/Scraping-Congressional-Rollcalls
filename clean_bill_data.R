###############################
###                         ###
###   CLEAN ROLLCALL DATA   ###
###                         ###
###############################

# Load in the data set
bill_data_2019 <- read.csv("../../Data_files_backup/bill_data_2019.csv")

clean_bill_data <- function(scraped_bill_data, year) {
        # Make sure we have the right libraries
        require(tidyverse)
        
        # Assign column names
        names(scraped_bill_data) <- c("Index", "Rollcall", "Date", "Issue", "Title", "Sponsor", "Committees", "Committee_reports",
                                      "Latest_action", "Rollcall_votes", "Summary_header", "Summary_text", "Text_shown_as",
                                      "Text", "Policy_area", "Legislative_subjects", "Cosponsors", "Committees_full",
                                      "Subcommittees_full")
        
        # Drop the Index, Rollcall and Date variables
        scraped_bill_data <- scraped_bill_data %>% 
                select(-Index, -Rollcall, -Date)
        
        # We replace the 'Date' column with a 'Year' variable, which we fill as a constant value
        # The reason why we don't maintain the year variable, is because there can be multiple rollcall votes on the same
        # bill. The data of the bill will remain the same, while the date will be different.
        # Dropping the date allows us to filter out duplicate records later on.
        # Adding a fixed year variable, will allow us later to merge all bill_data dataframes together.
        scraped_bill_data$Year <- year
        
        # We also drop the Committees_full and Subcommittees_full columns
        # The Committees_full column contains the same information as the Committees column
        # The Subcommittees_full column is not useful enough since we cannot connect the subcommittees to the right committee
        scraped_bill_data <- scraped_bill_data %>%
                select(-Committees_full, -Subcommittees_full)
        
        # Remove duplicate records
        scraped_bill_data <- distinct(scraped_bill_data)
        
        # Filter out procedural records
        scraped_bill_data <- filter(scraped_bill_data, !grepl("ADJOURN|JOURNAL|QUORUM", Issue))
        scraped_bill_data <- filter(scraped_bill_data, Issue != "")
        
        # A few columns in our dataframe are making this dataframe unmanageably large
        # The best approach here is to remove these columns for the data and create a separate dataframe for the large columns
        # This goes against best practices that indicate that we should avoid one-to-one relationships in our data model
        # However, the exceptional circumstances warrant this approach.
        bill_summary <- scraped_bill_data[, c("Issue", "Year", "Summary_header", "Summary_text")]
        bill_text <- scraped_bill_data[, c("Issue", "Year", "Text_shown_as", "Text")]
        scraped_bill_data <- scraped_bill_data %>%
                select(-Summary_header, -Summary_text, -Text_shown_as, -Text)
        
        # Reset factor levels after filtering
        scraped_bill_data <- droplevels(scraped_bill_data)
        
        # Split the Sponsor column to extract the introduction date of the bill
        scraped_bill_data <- separate(scraped_bill_data, Sponsor, c("Sponsor", "Introduction_date"), sep = "Introduced ")
        
        # Further split the resulting Sponsor column to extract the name of the sponsor
        scraped_bill_data <- separate(scraped_bill_data, Sponsor, c("Sponsor_name", "Sponsor_details"), sep = " \\[", extra = 'drop')
        
        # We extract if the sponsor is a House representative or a senator
        # There's probably more elegant ways to do this than through a for loop and nested if-statements.
        # But this will have to do for now.
        scraped_bill_data$Sponsor_role <- NaN
        for (i in 1:length(scraped_bill_data$Sponsor_name)) {
                if (grepl("Rep. ", scraped_bill_data$Sponsor_name[i]) ){
                        scraped_bill_data$Sponsor_role[i] <- "House representative"
                        scraped_bill_data$Sponsor_name[i] <- gsub("Rep. ", "", scraped_bill_data$Sponsor_name[i])
                } else if (grepl("Sen. ", scraped_bill_data$Sponsor_name[i])) {
                        scraped_bill_data$Sponsor_role[i] <- "Senator"
                        scraped_bill_data$Sponsor_name[i] <- gsub("Sen. ", "", scraped_bill_data$Sponsor_name[i])
                }
        }
        
        # Further split the resulting Sponsor details to extract the sponsor party, state and district
        # We make sure to drop anything after the closed square parentheses
        scraped_bill_data <- scraped_bill_data %>%
                separate(Sponsor_details, c("Sponsor_party", "Sponsor_state", "Sponsor_district"),sep = "-", fill = 'right') %>%
                separate(Sponsor_state, c("Sponsor_state"), sep = "\\]", extra = 'drop') %>%
                separate(Sponsor_district, c("Sponsor_district"), sep = "\\]", extra = 'drop')
        
        # Keep only the first 10 characters of the Introduction_date values
        scraped_bill_data$Introduction_date <- substr(scraped_bill_data$Introduction_date, 0, 10)
        # We won't bother here to transform the variable to a date variable.
        # This cleaning happens so the dataset can be stored in a .csv-file or a SQL database
        # The variable can be formatted correctly during analysis.
        
        # The Committees variable will need to be managed into a separate data frame, since a bill can be assigned
        # to multiple committees
        bill_committees <- scraped_bill_data[, c("Issue", "Year", "Committees")]
        
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
        
        # We can now drop the Committees column from the original data frame
        scraped_bill_data$Committees <- NULL
        
        
        # For the committee reports, we take a slightly different approach
        # Since we are not interested in the IDs of the reports, we will limit ourselves to simply counting how many reports
        # we made per branch
        # This will avoid unnecessary complication of our final data model
        scraped_bill_data$Num_house_reports <- str_count(scraped_bill_data$Committee_reports, "H. Rept.")
        scraped_bill_data$Num_senate_reports <- str_count(scraped_bill_data$Committee_reports, "S. Rept.")
        scraped_bill_data$Committee_reports <- NULL
        
        
        # For the Latest_action variable, we start by dropping the "(All Actions)" substring that is attached to many values
        scraped_bill_data$Latest_action <- gsub("\\(All Actions\\)", "", scraped_bill_data$Latest_action)
        
        # Next we get the branch that completed the latest action
        scraped_bill_data <- separate(scraped_bill_data, Latest_action,
                                      c("Branch_latest_action", "Latest_action"),
                                      sep = " - ", extra = 'merge', fill = 'left')
        # Removing leading and trailing whitespaces
        scraped_bill_data$Latest_action <- trimws(scraped_bill_data$Latest_action, which = "both")
        
        # Extracting the first ten characters as the latest action date
        # We'll need to be carefull to check that the assumption that the date will lead after separating the branch holds
        # up for other years as well
        scraped_bill_data$Date_latest_action <- substr(scraped_bill_data$Latest_action, 0, 10)
        scraped_bill_data$Latest_action <- substr(scraped_bill_data$Latest_action, 12, nchar(scraped_bill_data$Latest_action))
        
        # We drop another substring from the Latest_action variable where it appears and trim whitespaces again
        scraped_bill_data$Latest_action <- gsub("\\(TXT \\| PDF\\)", "", scraped_bill_data$Latest_action)
        scraped_bill_data$Latest_action <- trimws(scraped_bill_data$Latest_action, which = "both")
        
        
        # For the rollcall votes we only want to know how many roll calls there have been for the bill
        scraped_bill_data$Rollcall_votes <- gsub("There has been |There have been ", "", scraped_bill_data$Rollcall_votes)
        scraped_bill_data$Rollcall_votes <- gsub(" roll call vote| roll call votes", "", scraped_bill_data$Rollcall_votes)
        
        
        # We are happy with the Policy_area variable as is
        
        
        # For the Legislative_subjects variable we need to create a new data frame again
        bill_legis_subjects <- scraped_bill_data[, c("Issue", "Year", "Legislative_subjects")]
        # And then we separate the legislative subjects in separate rows
        bill_legis_subjects <- bill_legis_subjects %>%
                separate_rows(Legislative_subjects, sep = " \\\n ")
        # We can now remove the legislative_subjects variable from the original dataframe
        scraped_bill_data$Legislative_subjects <- NULL
        
        
        # Next we look at the Cosponsors variable
        # This will be a bit more challenging, since the separator between cosponsor and sponsor date, is the same
        # as between the various cosponsors.
        # We will again need a separate data frame to track cosponsors
        bill_cosponsors <- scraped_bill_data[, c("Issue", "Year", "Cosponsors")]
        
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
        
        # We can now remove the Cosponsors column from our original data frame
        scraped_bill_data$Cosponsors <- NULL
        
        return(list(scraped_bill_data, bill_summary, bill_text, bill_house_committees, bill_senate_committees,
                    bill_legis_subjects, bill_cosponsors))
}

# Need to brake down the function into 7 separate functions to create the different parts of the data
test1 <- clean_bill_data(bill_data_2019, 2019)
