## Name of script: VITEKpdf_to_excel.R
##
## Purpose:
## Starting with the PDF files from the bioMerieux Vitek 2 Compact,
## we iterate through each PDF file, extract the necessary information
## for each isolate, and create a Master CSV file containing the
## data. This is specifically for the AST-GP75 VITEK card
##
## Authors: Andrea Domen and James Molyneux
##

## Load necessary R libraries:
## tidyverse for general data manipulations
library(tidyverse)
## Lubridate for converting dates into, well... dates.
library(lubridate)
## tabulizer for ripping tables from PDFs
library(tabulizer)

##FILEPATH WHERE ALL PDF'S ARE LOCATED
all.pdfs <- ""

#other info needed is the file output name (line 231)
#and the file output path (line 232)


#########################################################
## Create a function which formats the data extracted
## from the PDF files
## The idea is to scrape each PDF table and then send it
## through this function to scrub it into a nice "clean"
## data frame to work with and manipulate later on.

format_cells <- function(x) {
  # Initial goal: Coerce matrix from PDF to data.frame
  # Extract the column names
  column_names <- x[1, 1:3]
  # Take the two sets of columns and combine them into
  # one column of variables in the data frame.
  df <- as.data.frame(rbind(x[-1, 1:3], x[-1, 4:6]))
  # Add the names of the columns to the data.frame
  names(df) <- column_names
  
  # When the data from the PDF is initially scraped, 
  # some of the names are too long and end up on two
  # lines. R then treats these two lines as two separate
  # entries, but this is wrong. We want to combine the
  # multi-line entries into a single row
  
  # Find the rows which contain the words highlighted below.
  # We want the rows so that we can manipulate them later.
  syn_rows <- which(df$Antimicrobial == "(synergy)")
  resist_rows <- which(df$Antimicrobial == "Resistance")
  sulfa_rows <- which(df$Antimicrobial == "Sulfamethoxazole")
  
  # We found out we don't actually need the "(synergy)" rows,
  # so we comment this line out and move on.
  # df$Antimicrobial[syn_rows - 1] <- paste(df$Antimicrobial[syn_rows - 1], "(synergy)")
  
  # Take the rows containing the word "resistance" and add them
  # the name of the entry from the previous row. Same for the
  # Sulfamethoxazole
  df$Antimicrobial[resist_rows - 1] <- paste(df$Antimicrobial[resist_rows - 1], "Resistance")
  df$Antimicrobial[sulfa_rows - 1] <- paste0(df$Antimicrobial[sulfa_rows - 1], "Sulfamethoxazole")
  
  # Remove the rows in the "reverse" order of where they appear 
  # in the data. This way, we don't end up altering the number
  # of rows before removing them.
  df <- df[-sulfa_rows, ]
  df <- df[-resist_rows, ]
  df <- df[-syn_rows, ]
  
  # Remove blank rows since they make the data ugly
  blank_rows <- which(df$Antimicrobial == "")
  df <- df[-blank_rows, ]
  
  # Replace blank values with NA
  df[df == ""] <- NA
  
  # Drop interpretation column
  df <- df[, c("Antimicrobial", "MIC")]
  
  # Give us the clean data back.
  return(df)
}


#########################################################
## This is where the bulk of the work happens. We want 
## to find where the PDF files are 
## located and then extract all of the names of the files
## so we can (1) scrape the data from the pdf and (2) 
## pull some of the info from the PDF file name.

## Point to the base directory containing the PDF file
setwd(all.pdfs)
base_dir <- getwd()

## Extract the file names
pdf_files <- list.files(base_dir)


## Find the unique isolate numbers & dates based on the
## naming convention of the file names.
isolate_numbers <- gsub("^[0-9]+_WRLP_([0-9]+)_[a-z]+.pdf", "\\1", pdf_files)
comp_date <- gsub("^([0-9]+)_.*", "\\1", pdf_files)

## Once we have the final names, we paste the dates/isolate
## names together to find the unique combinations. This is 
## helpful for instances where some isolates were "re-run"
## on different dates.
uni_iso_comp_date <- unique(paste(comp_date, isolate_numbers, sep = "_"))
uni_iso_numbers <- unlist(str_split(uni_iso_comp_date, pattern = "_"))[seq(2, length(uni_iso_comp_date) * 2, by = 2)]
uni_comp_date <- unlist(str_split(uni_iso_comp_date, pattern = "_"))[seq(1, length(uni_iso_comp_date) * 2, by = 2)]


#########################################################
## Now, we want to loop through each isolate and 
## format the data 

## Use i = 1 so I could look at intermediate outputs
## while prototyping
 i = 1

for (i in 1:length(uni_iso_comp_date)) {

  ## locate the file name for the first unique isolate
  ## and date combination.
  ## We end up using "regular expressions" to find the files.
  ## Here, "^[0-9]+_WRLP_" means:
  ## "Starting from the beginning of the character string, 
  ## look for repeated numbers [i.e. the date in the file name],
  ## then look for _WRLP_ and finally the isolate number.
  isolate_files <- pdf_files[grepl(paste0("^", uni_comp_date[i], "_WRLP_",
                                          uni_iso_numbers[i]), pdf_files)]
  
  ## There should be two "files" for each isolate, one for 
  ## each "strain". We want to find the names for isolate i
  ## and strain staph/end.
  staph_file <- isolate_files[grepl("_staph.pdf$", isolate_files)]
  ent_file <- isolate_files[grepl("_ent.pdf$", isolate_files)]
  
  
  ## Use lubridate to convert the date numbers into a legit 
  ## date formatted variable
  comp_date <- ymd(uni_comp_date[i])
  
  ## Define the "card type", which we take to always be the
  ## same value.
  card_type <- "AST-GP75"
  
  #########################################################  
  ## Our next goal is to scrape the PDF tables using tabulizer
  ## We use extract_tables(...)[[4]] to get the "4th" table 
  ## in the pdf doc. And each doc is found using some file path
  ## trickery inside paste0
  staph_pdf <- extract_tables(paste0(base_dir, "/", staph_file), pages = 1, method = "stream")[[4]]
  ent_pdf <- extract_tables(paste0(base_dir, "/", ent_file), pages = 1, method = "stream")[[4]]

  
  ## There's 3 variables of interest which we want to drop
  ## from one of the PDF's tables (Since the values are 
  ## blank) and want to get just the values of the variables
  ## of interest in the second PDF tables.
  ## We define those variable names here:
  vars_of_interest <- c("Ampicillin", "Gentamicin High Level", "Streptomycin High Level")
  
  ## Use the format_cells() function to format the scraped
  ## data
  staph_df <- format_cells(staph_pdf)
  ent_df <- format_cells(ent_pdf)
  
  ## Join the two scraped data frames together, making sure
  ## to drop or only include the variables of interest from
  ## the respective tables
  actual_df <- full_join(filter(staph_df, 
                                !Antimicrobial %in% vars_of_interest),
                         filter(ent_df, 
                                Antimicrobial %in% vars_of_interest))
  
  ## Create a temporary data frame which we can "add onto" as
  ## we iterate through the isolates. Create this table by...
  
  ## Taking our combined data frames post cleaning and merging
  ## ...
  tmp_df <- actual_df %>% 
    ## Make the data "wide" to match the formatting of the 
    ## original "master" data we use
    pivot_wider(names_from = Antimicrobial, values_from = MIC) %>% 
    ## Include info about the isolate, date, and card type
    mutate(Isolate = paste0("WRLP", uni_iso_numbers[i]),
           Date = comp_date,
           Card = card_type) %>% 
    ## Here, we altered one variable name to match the 
    ## original master data. Could drop this line below.
    rename(Cefoxitin = `Cefoxitin Screen`) %>% 
    dplyr::select(Isolate, Date, Card, everything())
  
  ## For the first iteration only, we setup our first cleaned
  ## pdf and call it the "master" data. We then move to the 
  ## next interation to avoid the "joining" process shown
  ## below.
  if (i == 1) {
    master_df <- tmp_df
    next 
  }
  

  
  ## Create our master_df by joining the new data (tmp_df)
  ## onto the original master_df
  ## Note: Here, we'll need to make more alterations once
  ## we move away from starting with an existing master data
  ## set and create our own master data set "organically"
  master_df <- full_join(master_df, tmp_df) %>% 
    
    ## Naturally, when we looked at the CSV, the date variable
    ## was a "date time" class. We convert the date into 
    ## just a "date" variable here.
    mutate(Date = as.Date(Date))
}

View(master_df)


#########################################################
## Once we've iterated and created our updated master
## data file, we output that data file using the line
## of code below.

output_file_name <- ".csv"
output_location <- ""

output_file_path <- paste0(output_location,
                           output_file_name)
## Save the data.
write_csv(master_df, file = output_file_path)


