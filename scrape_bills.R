## In this script we'll scrape the details about each bill that was voted on by congress since 1990

# Next we'll define a function to extract the bill details
scrape_bills <- function(year, rollcall) {
        # This function takes in a year between 1990 and the current year (as a string) as well as a data frame
        # which contains a column called 'issue_link' and contains links to the bills on the congress.gov website.
        # The argument data frame should also contain a column named 'Roll', 'Date' and 'Issue'.
        # The function returns a data frame of 18 columns with a number of details regarding each bill that was
        # voted on in the year past in the arguments.
        # Note that there can be missing values (NaN) in the issue_link column in the argument data frame. In this
        # casee the resulting data frame will contain a row of NaNs.
        
        # We make sure we have the right libraries available
        require(XML)
        require(RCurl)
        require(httr)
        
        # We initiate an empty list to store all the scraped data
        bill_data <- list()

        ## We don't want to run this scraping loop for all rollcalls, because it would take more than 25 hours.
        ## So we'll need to run this for one year at a time
        rollcall_subset <- rollcall[rollcall$Year == year,]
        
        # We initiate the for loop to go over each roll call vote
        for (i in 1:nrow(rollcall_subset)) {
                # First we make sure we actually have a bill link.
                # If not, we skip the rollcall
                if (rollcall_subset$issue_link[i] == "NaN") {
                        # We put together a list of just the bill identifying information
                        # with all missing values
                        bill_details_list <- c(
                                rollcall_subset$Roll[i],
                                rollcall_subset$Date[i],
                                rollcall_subset$Issue[i],
                                "NaN","NaN","NaN","NaN","NaN","NaN","NaN","NaN",
                                "NaN","NaN","NaN","NaN","NaN","NaN","NaN"
                        )
                        
                        # We add the record with missing values to our overview list of lists
                        bill_data[[i]] <- bill_details_list
                        
                        # We print a message that the record has been skipped due to missing link
                        print(paste("Skipped ", i, "/", nrow(rollcall_subset), 
                                    " due to missing weblink. Record of NaNs added.", sep = ""))
                        next
                }
                
                
                # We get the URL from the rollcall dataframe, and fetch the webpage online
                bill_URL <- rollcall_subset$issue_link[i]
                XML <- htmlTreeParse(GET(bill_URL),useInternal=TRUE)
                rootNode <- xmlRoot(XML)
                
                # And then we extract the details about the bill
                # First the bill header
                bill_header_data <- xpathSApply(rootNode,"//h1/text()", xmlValue)[1]
                
                # Then the bill overview information
                bill_details_data <- xpathSApply(rootNode,"//td", xmlValue)
                
                # Here we need to check for some bills who don't have all detail information
                # We drop any 'Notes' that are added to header info
                # We assume that the 'Notes' section is always at the end of the detail elements
                bill_details_cats <- xpathSApply(rootNode, "//th", xmlValue)
                if (any(grepl("Notes", bill_details_cats, fixed = TRUE))) {
                        bill_details_data <- head(bill_details_data, -1)
                }
                
                # We drop any reference to 'Committee Meetings' by first finding the position
                # of the committee meetings element, and then dropping it
                com_meet_pos <- match("Committee Meetings:", bill_details_cats)
                if (!(is.na(com_meet_pos))) {
                        bill_details_data <- bill_details_data[-com_meet_pos]
                }
                
                # We check for each category we expect if it is present.
                # The expected categories are Sponsor, Committees, Committee Reports, Latest Action and Roll Call Votes
                # If any category is missing, we replaced it with NaN
                if (length(bill_details_data) < 5) {
                        if (!(any(grepl("Sponsor", bill_details_cats, fixed = TRUE)))) {
                                bill_details_data <- append(bill_details_data, "NaN", 0)
                        }
                        
                        if (!(any(grepl("Committees", bill_details_cats, fixed = TRUE)))) {
                                bill_details_data <- append(bill_details_data, "NaN", 1)
                        }
                        
                        if (!(any(grepl("Committee Reports", bill_details_cats, fixed = TRUE)))) {
                                bill_details_data <- append(bill_details_data, "NaN", 2)
                        }
                        
                        if (!(any(grepl("Latest Action", bill_details_cats, fixed = TRUE)))) {
                                bill_details_data <- append(bill_details_data, "NaN", 3)
                        }
                        
                        if (!(any(grepl("Roll Call Votes", bill_details_cats, fixed = TRUE)))) {
                                bill_details_data <- append(bill_details_data, "NaN", 4)
                        }
                }
                
                
                # Next the bill summary header and text
                # Note that this only extracts the summary that is on the bill web page.
                # We assume here that the summary displayed is always the latest version.
                bill_summary_header_parts <- xpathSApply(rootNode, "//div[@id = 'bill-summary']/h3", xmlValue)
                bill_summary_text_parts <- xpathSApply(rootNode, "//div[@id = 'bill-summary']/p", xmlValue)
                # We concatenate all the pieces of the bill summary text
                bill_summary_header <- paste(bill_summary_header_parts, collapse = '\n')
                bill_summary_text <- paste(bill_summary_text_parts, collapse = '\n ')
                
                # We change any missing pieces to NaN
                if (is.null(bill_summary_header)) {
                        bill_summary_header <- NaN
                }
                if (is.null(bill_summary_text)) {
                        bill_summary_text <- NaN
                }
                
                
                # Now that we have all the information from this webpage, we can dig further and find some additional pages with relevant information
                # We start with the webpage that contains the bill text
                text_URL <- paste(bill_URL, "/text?format=txt", sep ='')
                XML <- htmlTreeParse(GET(text_URL), useInternal=TRUE)
                rootNode <- xmlRoot(XML)
                
                # We extract the 'shown as' information and the full text
                # We again only extract the version on the webpage, which we assume is the latest version
                bill_text_shown_as <- xpathSApply(rootNode, "//div[@id = 'bill-summary']/h3", xmlValue)
                bill_text <- xpathSApply(rootNode, "//div[@id = 'bill-summary']/pre", xmlValue)
                
                # We change any missing pieces to NaN
                if (is.null(bill_text_shown_as)) {
                        bill_text_shown_as <- NaN
                }
                if (is.null(bill_text) | length(bill_text) == 0) {
                        bill_text <- NaN
                }
                
                
                # Next we get the webpage that contains the bill subjects
                subjects_URL <- paste(bill_URL, "/subjects", sep='')
                XML <- htmlTreeParse(GET(subjects_URL), useInternal=TRUE)
                rootNode <- xmlRoot(XML)
                
                # We extract the policy area and the legislative subjects
                bill_policy_area <- xpathSApply(rootNode, "//div[@class = 'column-aside']/ul/li", xmlValue)
                bill_legislative_subjects_parts <- xpathSApply(rootNode, "//div[@class = 'column-main']/div/div/ul/li", xmlValue)
                bill_legislative_subjects <- paste(bill_legislative_subjects_parts, collapse = ' \n ')
                
                # We replace any missing pieces by NaN
                if (is.null(bill_policy_area)) {
                        bill_policy_area <- NaN
                }
                if (is.null(bill_legislative_subjects)) {
                        bill_legislative_subjects <- NaN
                }
                
                
                # Next we get the webpage that contains the bill cosponsers
                cosponsors_URL <- paste(bill_URL, "/cosponsors", sep='')
                XML <- htmlTreeParse(GET(cosponsors_URL), useInternal=TRUE)
                rootNode <- xmlRoot(XML)
                
                # We extract the cosponsers and the date they cosponsered
                bill_cosponsors_parts <- xpathSApply(rootNode, "//table[@class = 'item_table']/tbody/tr/td", xmlValue)
                bill_cosponsors_parts <- trimws(bill_cosponsors_parts, which = "both")
                bill_cosponsors <- paste(bill_cosponsors_parts, collapse = ' - ')
                
                # We replace any missing pieces with NaN
                if (is.null(bill_cosponsors)) {
                        bill_cosponsors <- NaN
                }
                
                
                # Next we get the webpage that contains the bill committees and subcommittees
                committees_URL <- paste(bill_URL, "/committees", sep='')
                XML <- htmlTreeParse(GET(committees_URL), useInternal=TRUE)
                rootNode <- xmlRoot(XML)
                
                # Extract the information on committees and subcommittees
                bill_committees_parts <- xpathSApply(rootNode, "//tr[@class = 'committee']/th[@class = 'names']", xmlValue)
                bill_subcommittees_parts <- xpathSApply(rootNode, "//tr[@class = 'subcommittee']/th[@class = 'names']", xmlValue)
                bill_committees <- paste(bill_committees_parts, collapse = ' - ')
                bill_subcommittees <- paste(bill_subcommittees_parts, collapse = ' - ')
                
                # We replace any missing parts with NaN
                if (is.null(bill_committees)) {
                        bill_committees <- NaN
                }
                if (is.null(bill_subcommittees)) {
                        bill_subcommittees <- NaN
                }
                
                
                # Next we put all the information gathered so far into a list
                bill_details_list <- c(
                        rollcall_subset$Roll[i],
                        rollcall_subset$Date[i],
                        rollcall_subset$Issue[i],
                        bill_header_data,
                        bill_details_data,
                        bill_summary_header,
                        bill_summary_text,
                        bill_text_shown_as,
                        bill_text,
                        bill_policy_area,
                        bill_legislative_subjects,
                        bill_cosponsors,
                        bill_committees,
                        bill_subcommittees
                )
                
                # And then we add that list to an overall list of lists
                bill_data[[i]] <- bill_details_list
                
                # Now we give the user a message that the bill has been scraped
                print(paste("Completed ", i, "/", nrow(rollcall_subset), ".", sep = ""))
                
                # Finally, we pause execution for 5 seconds to not overload the website server
                # Note that this significantly slows down the process, but the website host will appreciate it
                Sys.sleep(5)
        }
                
        # Next we extract the header data on the bill basic data
        bill_details <- data.frame(do.call("rbind", bill_data))
        names(bill_details) <- c("Rollcall", "Date", "Issue", "Title", "Sponsor", "Committees",
                                 "Committee reports", "Latest action", "Roll call votes",
                                 "Bill summary header", "Bill summary text", "Bill text shown as",
                                 "Bill text", "Bill policy area", "Bill legislative subjects",
                                 "Bill cosponsors", "Bill committees full", "Bill subcommittees full")
        
        return(bill_details)
}





