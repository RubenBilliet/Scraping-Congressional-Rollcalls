# Load in necessary functions and libraries
source("scrape_roll_calls.R")
source("scrape_bills.R")
source("scrape_votes.R")
source("clean_bill_data.R")
library(RMariaDB)

###### SCRAPE ROLL CALL ######

# Scrape congressional role calls
rollcall <- scrape_congress_role_call(years = c(1990:2020))


###### SCRAPE BILLS ######

# Next we scrape all bills
# We do this separately per year, in order to not get blocked by an error and have to start over from the start
# Additionally, this helps not to overload the host server
# We extract the data for 1990
bill_data_1990 <- scrape_bills("1990", rollcall)
write.csv(bill_data_1990, "../../Data_files_backup/bill_data_1990.csv")

# We extract the data for 1991
bill_data_1991 <- scrape_bills("1991", rollcall)
write.csv(bill_data_1991, "../../Data_files_backup/bill_data_1991.csv")

# We extract the data for 1992
bill_data_1992 <- scrape_bills("1992", rollcall)
write.csv(bill_data_1992, "../../Data_files_backup/bill_data_1992.csv")

# We extract the data for 1993
bill_data_1993 <- scrape_bills("1993", rollcall)
write.csv(bill_data_1993, "../../Data_files_backup/bill_data_1993.csv")

# We extract the data for 1994
bill_data_1994 <- scrape_bills("1994", rollcall)
write.csv(bill_data_1994, "../../Data_files_backup/bill_data_1994.csv")

# We extract the data for 1995
bill_data_1995 <- scrape_bills("1995", rollcall)
write.csv(bill_data_1995, "../../Data_files_backup/bill_data_1995.csv")

# We extract the data for 1996
bill_data_1996 <- scrape_bills("1996", rollcall)
write.csv(bill_data_1996, "../../Data_files_backup/bill_data_1996.csv")

# We extract the data for 1997
bill_data_1997 <- scrape_bills("1997", rollcall)
write.csv(bill_data_1997, "../../Data_files_backup/bill_data_1997.csv")

# We extract the data for 1998
bill_data_1998 <- scrape_bills("1998", rollcall)
write.csv(bill_data_1998, "../../Data_files_backup/bill_data_1998.csv")

# We extract the data for 1999
bill_data_1999 <- scrape_bills("1999", rollcall)
write.csv(bill_data_1999, "../../Data_files_backup/bill_data_1999.csv")

# We extract the data for 2000
bill_data_2000 <- scrape_bills("2000", rollcall)
write.csv(bill_data_2000, "../../Data_files_backup/bill_data_2000.csv")

# We extract the data for 2001
bill_data_2001 <- scrape_bills("2001", rollcall)
write.csv(bill_data_2001, "../../Data_files_backup/bill_data_2001.csv")

# We extract the data for 2002
bill_data_2002 <- scrape_bills("2002", rollcall)
write.csv(bill_data_2002, "../../Data_files_backup/bill_data_2002.csv")

# We extract the data for 2003
bill_data_2003 <- scrape_bills("2003", rollcall)
write.csv(bill_data_2003, "../../Data_files_backup/bill_data_2003.csv")

# We extract the data for 2004
bill_data_2004 <- scrape_bills("2004", rollcall)
write.csv(bill_data_2004, "../../Data_files_backup/bill_data_2004.csv")

# We extract the data for 2005
bill_data_2005 <- scrape_bills("2005", rollcall)
write.csv(bill_data_2005, "../../Data_files_backup/bill_data_2005.csv")

# We extract the data for 2006
bill_data_2006 <- scrape_bills("2006", rollcall)
write.csv(bill_data_2006, "../../Data_files_backup/bill_data_2006.csv")

# We extract the data for 2007
bill_data_2007 <- scrape_bills("2007", rollcall)
write.csv(bill_data_2007, "../../Data_files_backup/bill_data_2007.csv")

# We extract the data for 2008
bill_data_2008 <- scrape_bills("2008", rollcall)
write.csv(bill_data_2008, "../../Data_files_backup/bill_data_2008.csv")

# We extract the data for 2009
bill_data_2009 <- scrape_bills("2009", rollcall)
write.csv(bill_data_2009, "../../Data_files_backup/bill_data_2009.csv")

# We extract the data for 2010
bill_data_2010 <- scrape_bills("2010", rollcall)
write.csv(bill_data_2010, "../../Data_files_backup/bill_data_2010.csv")

# We extract the data for 2011
bill_data_2011 <- scrape_bills("2011", rollcall)
write.csv(bill_data_2011, "../../Data_files_backup/bill_data_2011.csv")

# We extract the data for 2012
bill_data_2012 <- scrape_bills("2012", rollcall)
write.csv(bill_data_2012, "../../Data_files_backup/bill_data_2012.csv")

# We extract the data for 2013
bill_data_2013 <- scrape_bills("2013", rollcall)
write.csv(bill_data_2013, "../../Data_files_backup/bill_data_2013.csv")

# We extract the data for 2014
bill_data_2014 <- scrape_bills("2014", rollcall)
write.csv(bill_data_2014, "../../Data_files_backup/bill_data_2014.csv")

# We extract the data for 2015
bill_data_2015 <- scrape_bills("2015", rollcall)
write.csv(bill_data_2015, "../../Data_files_backup/bill_data_2015.csv")

# We extract the data for 2016
bill_data_2016 <- scrape_bills("2016", rollcall)
write.csv(bill_data_2016, "../../Data_files_backup/bill_data_2016.csv")

# We extract the data for 2017
bill_data_2017 <- scrape_bills("2017", rollcall)
write.csv(bill_data_2017, "../../Data_files_backup/bill_data_2017.csv")

# We extract the data for 2018
bill_data_2018 <- scrape_bills("2018", rollcall)
write.csv(bill_data_2018, "../../Data_files_backup/bill_data_2018.csv")

# We extract the data for 2019
bill_data_2019 <- scrape_bills("2019", rollcall)
write.csv(bill_data_2019, "../../Data_files_backup/bill_data_2019.csv")

# Next we apply our different cleaning functions from the clean_bill_data.R script
# We're going to assume here that the scraped raw data does not exist in the environment yet
# and read it in from our csv back-up
years <- 1990:2019
for (i in years) {
        # Convert year to character
        year <- as.character(i)
        # Read in data
        data <- read.csv(paste("../../Data_files_backup/bill_data_", year, ".csv", sep = ""))
        
        # Apply the cleaning functions
        bill_house_committees <- get_bill_committees(data, year, "House")
        bill_senate_committees <- get_bill_committees(data, year, "Senate")
        bill_cosponsors <- get_bill_cosponsors(data, year)
        bill_details <- get_bill_details(data, year)
        bill_legis_subjects <- get_bill_legis_subjects(data, year)
        bill_summary <- get_bill_summary(data, year)
        bill_text <- get_bill_text(data, year)
        
        # Save the data as .csv files
        write.csv(bill_house_committees, paste("../../Data_files_backup/cleaned_bill_house_committees_", year, ".csv", sep = ""), row.names = FALSE)
        write.csv(bill_senate_committees, paste("../../Data_files_backup/cleaned_bill_senate_committees_", year, ".csv", sep = ""), row.names = FALSE)
        write.csv(bill_cosponsors, paste("../../Data_files_backup/cleaned_bill_cosponsors_", year, ".csv", sep = ""), row.names = FALSE)
        write.csv(bill_details, paste("../../Data_files_backup/cleaned_bill_details_", year, ".csv", sep = ""), row.names = FALSE)
        write.csv(bill_legis_subjects, paste("../../Data_files_backup/cleaned_bill_legis_subjects_", year, ".csv", sep = ""), row.names = FALSE)
        write.csv(bill_summary, paste("../../Data_files_backup/cleaned_bill_summary_", year, ".csv", sep = ""), row.names = FALSE)
        write.csv(bill_text, paste("../../Data_files_backup/cleaned_bill_text_", year, ".csv", sep = ""), row.names = FALSE)
}


###### SCRAPE VOTES ######

# We also scrape the votes.
# We again do this year by year, to prevent too much rework in case of errors and to prevent
# overload the host server

# We scrape the data for 2019 and turn it into 2 separate data frames
vote2019 <- scrape_votes(rollcall, "2019")
vote_details_2019 <- data.frame(vote2019[[1]])
vote_2019 <- data.frame(vote2019[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2019, "../../Data_files_backup/vote_details_2019.csv")
write.csv(vote_2019, "../../Data_files_backup/vote_2019.csv")

# We scrape the data for 2018 and turn it into 2 separate data frames
vote2018 <- scrape_votes(rollcall, "2018")
vote_details_2018 <- data.frame(vote2018[[1]])
vote_2018 <- data.frame(vote2018[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2018, "../../Data_files_backup/vote_details_2018.csv")
write.csv(vote_2018, "../../Data_files_backup/vote_2018.csv")

# We scrape the data for 2017 and turn it into 2 separate data frames
vote2017 <- scrape_votes(rollcall, "2017")
vote_details_2017 <- data.frame(vote2017[[1]])
vote_2017 <- data.frame(vote2017[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2017, "../../Data_files_backup/vote_details_2017.csv")
write.csv(vote_2017, "../../Data_files_backup/vote_2017.csv")

# We scrape the data for 2016 and turn it into 2 separate data frames
vote2016 <- scrape_votes(rollcall, "2016")
vote_details_2016 <- data.frame(vote2016[[1]])
vote_2016 <- data.frame(vote2016[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2016, "../../Data_files_backup/vote_details_2016.csv")
write.csv(vote_2016, "../../Data_files_backup/vote_2016.csv")

# We scrape the data for 2015 and turn it into 2 separate data frames
vote2015 <- scrape_votes(rollcall, "2015")
vote_details_2015 <- data.frame(vote2015[[1]])
vote_2015 <- data.frame(vote2015[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2015, "../../Data_files_backup/vote_details_2015.csv")
write.csv(vote_2015, "../../Data_files_backup/vote_2015.csv")

# We scrape the data for 2014 and turn it into 2 separate data frames
vote2014 <- scrape_votes(rollcall, "2014")
vote_details_2014 <- data.frame(vote2014[[1]])
vote_2014 <- data.frame(vote2014[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2014, "../../Data_files_backup/vote_details_2014.csv")
write.csv(vote_2014, "../../Data_files_backup/vote_2014.csv")

# We scrape the data for 2013 and turn it into 2 separate data frames
vote2013 <- scrape_votes(rollcall, "2013")
vote_details_2013 <- data.frame(vote2013[[1]])
vote_2013 <- data.frame(vote2013[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2013, "../../Data_files_backup/vote_details_2013.csv")
write.csv(vote_2013, "../../Data_files_backup/vote_2013.csv")

# We scrape the data for 2012 and turn it into 2 separate data frames
vote2012 <- scrape_votes(rollcall, "2012")
vote_details_2012 <- data.frame(vote2012[[1]])
vote_2012 <- data.frame(vote2012[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2012, "../../Data_files_backup/vote_details_2012.csv")
write.csv(vote_2012, "../../Data_files_backup/vote_2012.csv")

# We scrape the data for 2011 and turn it into 2 separate data frames
vote2011 <- scrape_votes(rollcall, "2011")
vote_details_2011 <- data.frame(vote2011[[1]])
vote_2011 <- data.frame(vote2011[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2011, "../../Data_files_backup/vote_details_2011.csv")
write.csv(vote_2011, "../../Data_files_backup/vote_2011.csv")

# We scrape the data for 2010 and turn it into 2 separate data frames
vote2010 <- scrape_votes(rollcall, "2010")
vote_details_2010 <- data.frame(vote2010[[1]])
vote_2010 <- data.frame(vote2010[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2010, "../../Data_files_backup/vote_details_2010.csv")
write.csv(vote_2010, "../../Data_files_backup/vote_2010.csv")

# We scrape the data for 2009 and turn it into 2 separate data frames
vote2009 <- scrape_votes(rollcall, "2009")
vote_details_2009 <- data.frame(vote2009[[1]])
vote_2009 <- data.frame(vote2009[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2009, "../../Data_files_backup/vote_details_2009.csv")
write.csv(vote_2009, "../../Data_files_backup/vote_2009.csv")

# We scrape the data for 2008 and turn it into 2 separate data frames
vote2008 <- scrape_votes(rollcall, "2008")
vote_details_2008 <- data.frame(vote2008[[1]])
vote_2008 <- data.frame(vote2008[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2008, "../../Data_files_backup/vote_details_2008.csv")
write.csv(vote_2008, "../../Data_files_backup/vote_2008.csv")

# We scrape the data for 2007 and turn it into 2 separate data frames
vote2007 <- scrape_votes(rollcall, "2007")
vote_details_2007 <- data.frame(vote2007[[1]])
vote_2007 <- data.frame(vote2007[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2007, "../../Data_files_backup/vote_details_2007.csv")
write.csv(vote_2007, "../../Data_files_backup/vote_2007.csv")

# We scrape the data for 2006 and turn it into 2 separate data frames
vote2006 <- scrape_votes(rollcall, "2006")
vote_details_2006 <- data.frame(vote2006[[1]])
vote_2006 <- data.frame(vote2006[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2006, "../../Data_files_backup/vote_details_2006.csv")
write.csv(vote_2006, "../../Data_files_backup/vote_2006.csv")

# We scrape the data for 2005 and turn it into 2 separate data frames
vote2005 <- scrape_votes(rollcall, "2005")
vote_details_2005 <- data.frame(vote2005[[1]])
vote_2005 <- data.frame(vote2005[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2005, "../../Data_files_backup/vote_details_2005.csv")
write.csv(vote_2005, "../../Data_files_backup/vote_2005.csv")

# We scrape the data for 2004 and turn it into 2 separate data frames
vote2004 <- scrape_votes(rollcall, "2004")
vote_details_2004 <- data.frame(vote2004[[1]])
vote_2004 <- data.frame(vote2004[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2004, "../../Data_files_backup/vote_details_2004.csv")
write.csv(vote_2004, "../../Data_files_backup/vote_2004.csv")

# We scrape the data for 2003 and turn it into 2 separate data frames
vote2003 <- scrape_votes(rollcall, "2003")
vote_details_2003 <- data.frame(vote2003[[1]])
vote_2003 <- data.frame(vote2003[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2003, "../../Data_files_backup/vote_details_2003.csv")
write.csv(vote_2003, "../../Data_files_backup/vote_2003.csv")

# We scrape the data for 2002 and turn it into 2 separate data frames
vote2002 <- scrape_votes(rollcall, "2002")
vote_details_2002 <- data.frame(vote2002[[1]])
vote_2002 <- data.frame(vote2002[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2002, "../../Data_files_backup/vote_details_2002.csv")
write.csv(vote_2002, "../../Data_files_backup/vote_2002.csv")

# We scrape the data for 2001 and turn it into 2 separate data frames
vote2001 <- scrape_votes(rollcall, "2001")
vote_details_2001 <- data.frame(vote2001[[1]])
vote_2001 <- data.frame(vote2001[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2001, "../../Data_files_backup/vote_details_2001.csv")
write.csv(vote_2001, "../../Data_files_backup/vote_2001.csv")

# We scrape the data for 2000 and turn it into 2 separate data frames
vote2000 <- scrape_votes(rollcall, "2000")
vote_details_2000 <- data.frame(vote2000[[1]])
vote_2000 <- data.frame(vote2000[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_2000, "../../Data_files_backup/vote_details_2000.csv")
write.csv(vote_2000, "../../Data_files_backup/vote_2000.csv")

# We scrape the data for 1999 and turn it into 2 separate data frames
vote1999 <- scrape_votes(rollcall, "1999")
vote_details_1999 <- data.frame(vote1999[[1]])
vote_1999 <- data.frame(vote1999[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1999, "../../Data_files_backup/vote_details_1999.csv")
write.csv(vote_1999, "../../Data_files_backup/vote_1999.csv")

# We scrape the data for 1998 and turn it into 2 separate data frames
vote1998 <- scrape_votes(rollcall, "1998")
vote_details_1998 <- data.frame(vote1998[[1]])
vote_1998 <- data.frame(vote1998[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1998, "../../Data_files_backup/vote_details_1998.csv")
write.csv(vote_1998, "../../Data_files_backup/vote_1998.csv")

# We scrape the data for 1997 and turn it into 2 separate data frames
vote1997 <- scrape_votes(rollcall, "1997")
vote_details_1997 <- data.frame(vote1997[[1]])
vote_1997 <- data.frame(vote1997[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1997, "../../Data_files_backup/vote_details_1997.csv")
write.csv(vote_1997, "../../Data_files_backup/vote_1997.csv")

# We scrape the data for 1996 and turn it into 2 separate data frames
vote1996 <- scrape_votes(rollcall, "1996")
vote_details_1996 <- data.frame(vote1996[[1]])
vote_1996<- data.frame(vote1996[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1996, "../../Data_files_backup/vote_details_1996.csv")
write.csv(vote_1996, "../../Data_files_backup/vote_1996.csv")

# We scrape the data for 1995 and turn it into 2 separate data frames
vote1995 <- scrape_votes(rollcall, "1995")
vote_details_1995 <- data.frame(vote1995[[1]])
vote_1995 <- data.frame(vote1995[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1995, "../../Data_files_backup/vote_details_1995.csv")
write.csv(vote_1995, "../../Data_files_backup/vote_1995.csv")

# We scrape the data for 1994 and turn it into 2 separate data frames
vote1994 <- scrape_votes(rollcall, "1994")
vote_details_1994 <- data.frame(vote1994[[1]])
vote_1994 <- data.frame(vote1994[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1994, "../../Data_files_backup/vote_details_1994.csv")
write.csv(vote_1994, "../../Data_files_backup/vote_1994.csv")

# We scrape the data for 1993 and turn it into 2 separate data frames
vote1993 <- scrape_votes(rollcall, "1993")
vote_details_1993 <- data.frame(vote1993[[1]])
vote_1993 <- data.frame(vote1993[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1993, "../../Data_files_backup/vote_details_1993.csv")
write.csv(vote_1993, "../../Data_files_backup/vote_1993.csv")

# We scrape the data for 1992 and turn it into 2 separate data frames
vote1992 <- scrape_votes(rollcall, "1992")
vote_details_1992 <- data.frame(vote1992[[1]])
vote_1992 <- data.frame(vote1992[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1992, "../../Data_files_backup/vote_details_1992.csv")
write.csv(vote_1992, "../../Data_files_backup/vote_1992.csv")

# We scrape the data for 1991 and turn it into 2 separate data frames
vote1991 <- scrape_votes(rollcall, "1991")
vote_details_1991 <- data.frame(vote1991[[1]])
vote_1991 <- data.frame(vote1991[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1991, "../../Data_files_backup/vote_details_1991.csv")
write.csv(vote_1991, "../../Data_files_backup/vote_1991.csv")

# We scrape the data for 1990 and turn it into 2 separate data frames
vote1990 <- scrape_votes(rollcall, "1990")
vote_details_1990 <- data.frame(vote1990[[1]])
vote_1990 <- data.frame(vote1990[[2]])
# As a precaution, we also store the data in a .csv file
write.csv(vote_details_1990, "../../Data_files_backup/vote_details_1990.csv")
write.csv(vote_1990, "../../Data_files_backup/vote_1990.csv")

# We combine all data frames together into 2 main data frames
vote_details <- rbind(vote_details_1990, vote_details_1991, vote_details_1992, vote_details_1993,
                      vote_details_1994, vote_details_1995, vote_details_1996, vote_details_1997,
                      vote_details_1998, vote_details_1999, vote_details_2000, vote_details_2001,
                      vote_details_2002, vote_details_2003, vote_details_2004, vote_details_2005,
                      vote_details_2006, vote_details_2007, vote_details_2008, vote_details_2009,
                      vote_details_2010, vote_details_2011, vote_details_2012, vote_details_2013,
                      vote_details_2014, vote_details_2015, vote_details_2016, vote_details_2017,
                      vote_details_2018, vote_details_2019)

votes <- rbind(vote_1990, vote_1991, vote_1992, vote_1993, vote_1994, vote_1995, vote_1996, vote_1997,
               vote_1998, vote_1999, vote_2000, vote_2001, vote_2002, vote_2003, vote_2004, vote_2005,
               vote_2006, vote_2007, vote_2008, vote_2009, vote_2010, vote_2011, vote_2012, vote_2013,
               vote_2014, vote_2015, vote_2016, vote_2017, vote_2018, vote_2019)

# Open up database connection
db_user <- readline(prompt = 'User: ')
db_password <- readline(prompt = 'Password: ')
db_name <- readline(prompt = 'Database: ')
db_host <- readline(prompt = 'Host: ')
prompted_port <- readline(prompt = 'Port: ')
db_port <- as.integer(prompted_port)

con <- dbConnect(MariaDB(),
                 user = db_user, 
                 password = db_password,
                 dbname = db_name,
                 host = db_host,
                 port = db_port)

dbWriteTable(con, 'Congressional_Role_Call', as.data.frame(rollcall))
dbWriteTable(con, 'Vote_details', as.data.frame(vote_details))
dbWriteTable(con, 'Votes', as.data.frame(votes))
# Still having issues writing the bill details table, cause some fields are too long
# dbWriteTable(con, 'Bill_details', as.data.frame(bill_data_1990), append = TRUE)

dbDisconnect(con)