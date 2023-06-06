# Packages needed

library(tabulizer)
library(tidyverse)
library(lubridate)
library(jsonlite)


url <- "https://ppac.gov.in/retail-selling-price-rsp-of-petrol-diesel-and-domestic-lpg/rsp-of-petrol-and-diesel-in-metro-cities-since-16-6-2017"
read_url <- readLines(url, encoding = "UTF-8", warn=FALSE)
ind <- grep('fz18 fw400 my-4 mb-auto',read_url)

# Source file
pdf <- gsub(x=read_url[ind], pattern=".*(https.*pdf).*", replace="\\1")

# Read csv files

petrol_file = "./data/Petrol.csv"
diesel_file = "./data/Diesel.csv"

read_csv <- function(csv_file) {

    # Process repeated as above

    oil <- read.csv(csv_file, sep = ",",
                    fileEncoding="utf-8")
    oil$Date <- parse_date_time(oil$Date, orders = c('dmy', 'ydm','ymd'))

    return (oil)

}

modify_df <- function(fuel_today) {
    PD <- fuel_today %>%
        mutate_all(type.convert) %>%
        mutate_if(is.factor, as.character) %>% # Converted to character

        mutate(across(where(is.character), str_trim))%>%

        mutate(map_df(fuel_today, ~ gsub('\\s+', '', .x))) %>% # Remove unwanted characters

        `colnames<-`(c("Date","Delhi","Mumbai","Chennai","Kolkata")) %>%
        mutate_at(vars("Delhi","Mumbai","Chennai","Kolkata"), as.numeric)

    PD$Date <- parse_date_time(PD$Date, orders = c('dmy', 'ydm','ymd'))

    return (PD)

}

fuel_update <- function(item, pdf) {
    # Extract first few rows of table
    petrol_diesel <- extract_tables(pdf,
                                    output = "matrix",
                                    pages = c(1,1),
                                    area = list(
                                        c(164.246,72.158,244.977,292.080),
                                        c(164.246,310.175,244.977,537.058)),
                                    guess = FALSE,
    )
    if (item == 'petrol') {
        # Petrol price
        fuel_td <- as.data.frame(petrol_diesel[[1]]) # As data.frame
        Pt <- modify_df(fuel_td)
        petrol <- read_csv(petrol_file)
        petrole <- rbind(petrol,Pt) %>%
            distinct()

        petrole <-petrole[rev(order(as.Date(petrole$Date, format = "%d-%B-%y"))),]

        return (petrole)
    }
    else if (item == 'diesel') {
        fuel_td <- as.data.frame(petrol_diesel[[2]])
        dt <- modify_df(fuel_td)
        diesel <- read_csv(diesel_file)
        diesele <- rbind(diesel, dt) %>% # Rbind to append scrapped data
            distinct()

        diesele <-diesele[rev(order(as.Date(diesele$Date, format = "%d-%B-%y"))),]

        return (diesele)
    }
    else {
        print("Invalid Item. Please use 'petrol' or 'diesel'.")
    }

}

write_output <- function(item, format, param) {

    if (item == 'petrol' || item == 'diesel') {
        if (format == 'csv') {
            fname <- paste("./data/", str_to_title(item),".csv", sep="")
            write.csv(param,fname, quote=F,row.names=FALSE)
        }

        else if (format == 'json') {
            fname <- paste("./data/", str_to_title(item),".json", sep="")
            djson <- write_json(param, path=fname)
        }


        else {
            print("Invalid format. Use 'csv' or 'json' for output.")
        }
    }
    else {
        stop("Invalid item. Please use 'petrol' or 'diesel'.")
    }
}

pet <- fuel_update('petrol', pdf)
des <- fuel_update('diesel', pdf)
write_output('petrol', 'csv', pet)
write_output('petrol', 'json', pet)
write_output('diesel', 'csv', des)
write_output('diesel', 'json', des)
