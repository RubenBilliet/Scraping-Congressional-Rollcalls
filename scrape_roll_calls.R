## In this script we will scrape roll call votes from the U.S. congress

# We will approach this by creating a function that allows a user to pass the years for which
# they would like to scrape the roll call
# We assume here that the user passes a vector of numerical values to the function for which
# a roll call exists on congress.gov/roll-call-votes
scrape_congress_role_call <- function(years, wait_per_page = 5, wait_per_year = 20) {
        # We make sure the right libraries are available
        require(XML)
        require(dplyr)
        require(RCurl)
        
        # Could be useful to check if all years are valid numerical entries at this point
        
        # We need to initiate a loop to go over all the years passed in the argument
        # We'll need an overall container to hold the roll call data over the different years
        yearly_data <- list()
        for (i in 1:length(years)) {
                # First we compose the URL
                URL <- paste("http://clerk.house.gov/evs/", as.character(years[i]), "/index.asp", sep = "")
                
                # Now we export the overview page for the year
                htmlContent <- getURL(URL)
                XML <- try(htmlTreeParse(URL,useInternal=TRUE))
                
                # Next we parse out the XML
                rootNode <- xmlRoot(XML)
                
                # We extract all links to the pages with the different roll calls
                # Note that this only returns the variable part of the URL we'll need to get
                # all of the roll calls. We'll need to append the rest when we compose each URL.
                links_to_rolls <- xpathSApply(rootNode,"//body/font/a/@href")
                
                # Now we start another loop to go over all the pages of the roll call
                # We'll need a container for each matrix per iteration
                rolls_per_page <- list()
                for (j in 1:length(links_to_rolls)) {
                        # We compose the URL for the roll call sheet
                        URL <- paste("http://clerk.house.gov/evs/", 
                                     as.character(years[i]),
                                     "/",
                                     links_to_rolls[j],
                                     sep="")
                        
                        # We get the roll call sheet from the internet
                        htmlContent <- getURL(URL)
                        XML <- try(htmlTreeParse(URL,useInternal=TRUE))
                        
                        # We parse out the XML
                        rootNode <- xmlRoot(XML)
                        
                        # We export the header
                        header <- xpathSApply(rootNode,"//th", xmlValue)
                        
                        # We export the data
                        data <- xpathSApply(rootNode,"//td", xmlValue)
                        
                        # We assign the data to a matrix along the length of the header
                        data <- matrix(data, ncol = length(header), byrow=TRUE)
                        
                        # We also want to export the hyperlink to the roll call results
                        roll_call_results <- xpathSApply(rootNode,"//td/a/@href")
                        
                        # We append the roll call results to the data
                        data <- cbind(data, roll_call_results)
                        header <- c(header, "Roll_call_link")
                        
                        # We also want to export the hyperlink to the issue, where applicable
                        # The problem is that some roll calls don't include a link to an actual issue
                        # if the vote was on a more administrative issue
                        # We need to make sure that we don't include a link for these records
                        # Unfortunately we'll need to loop over all records within the subnode to do this
                        subnodes <- getNodeSet(rootNode, "//tr")
                        
                        issue_links = list()
                        for (k in 2:length(subnodes)) {
                                if (is.null(subnodes[[k]][[5]][[1]][[1]])) {
                                        issue_links[[k-1]] <- NaN
                                } else if (grepl("href", as(subnodes[[k]][[5]][[1]][[1]], "character"))) {
                                        issue_links[[k-1]] <- strsplit(as(subnodes[[k]][[5]][[1]][[1]], "character"), "\"")[[1]][2]
                                } else {
                                        issue_links[[k-1]] <- NaN
                                }
                        }
                        
                        # We append the issue links to the data
                        data <- cbind(data, unlist(issue_links, use.names = FALSE))
                        header <- c(header, "issue_link")
                        
                        # We put the data in the container
                        rolls_per_page[[j]] <- data
                        
                        # We introduce a 5-second pause as to not overload the server
                        Sys.sleep(wait_per_page)
                        
                        # We print a message to confirm that the page has been completed
                        print(paste("Page ", j, "/", length(links_to_rolls), 
                                    " of year ", years[i], " done.", sep = ""))
                }
                
                # We combine all pages into 1 matrix and assign it to the overall container
                yearly_data[[i]] <- do.call(rbind, rolls_per_page)
                
                # We add an additional column to the yearly data to indicate the year
                yearly_data[[i]] <- cbind(yearly_data[[i]], 
                                         rep(c(years[i]), times = nrow(yearly_data[[i]])))
                
                # We assign the column names based on the last header we exported
                colnames(yearly_data[[i]]) <- c(header, "Year")
                
                # We introduce an additional 20-second pause between years as to not overload the server
                Sys.sleep(wait_per_year)
        
        }
        # We combine all yearly data into one matrix
        full_data <- do.call(rbind, yearly_data)

        # We return the data as the output of the function
        return(full_data)
}