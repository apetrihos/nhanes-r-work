## ACTIVATING LIBRARIES
## I STILL SPECIFY PACKAGES FOR FUNCTION CALLS FOR READABILITY
library(nhanesA)
library(purrr)
library(progress)
library(pbapply)
library(magrittr)
library(glue)
library(car)
library(dplyr)

## WILL OVER EXPLAIN MY REASONING AND PROCESSES IN ORDER TO CLEAN THIS DATASET
## SOME EXAMPLES MIGHT BE INNEFICIENT OR INCONGRUENT
## I AM TRYING TO USE A VARIETY OF PACKAGES AND METHODS TO DEMONSTRATE MASTERY

## Sourcing Functions
require('niftyTools')

## IF YOU DON'T HAVE MY COMPANION PACKAGE DOWNLOAD HERE
#devtools::install_github('https://github.com/apetrihos/niftyTools')

## COLLATING ALL DATA FILES AND MERGING ####

## INDEX OF DATA FILES 
dataIndex <- nhanesA::nhanesTables('Q', '2009')

## EXTRACTING THE DATA
allDataList <- pbapply::pblapply(dataIndex$Data.File.Name, nhanes)

allDataDF <- merge_dataframe_list(allDataList, matching_key = 'SEQN')

dataNamesIndex <- sapply(allDataList, names) %>% 
  purrr::set_names(dataIndex$Data.File.Name)

## CHECKING FOR DUPLICATED COLS IN THE STACK
duplicates <- grep("\\.x$|\\.y$", names(allDataDF), value = T)
duplicates

# LOOKS LIKE THERE ARE DUPLICATED COLUMN NAMES
# LETS FIND WHERE THEY ARE

## WE WILL USE THE LIST SO WE KNOW WHERE THE DUPLICATES CAME FROM
duplicatedNames <- duplicates %>% 
  gsub("\\.x|\\.y", "", .) %>% 
  unique()

lapply(dataNamesIndex, function(datList) {
  
  ## WE CAN USE GREP
  grep(glue::glue_collapse(glue::glue("^{duplicatedNames}"), sep = '|'),
       datList)
  
  # ## OR JUST %in%
  # duplicatedNames %in% datList %>%
  #   table
  
})

# LOOKS LIKE THE LAST 2 DATAFRAMES CONTAIN DUPLICATE COLUMNS
# AFTER CHECKING THE INDEX ONE IS FOR ADULTS AND ONE IS FOR CHILDREN
# WE WILL REMOVE THE CHILDREN

## USING DPLYR
dataIndex %<>%
  dplyr::filter(Data.File.Name != 'CBQPFC_E')

## IF YOU WANTED TO USE BASE R
# dataIndex <- dataIndex[dataIndex$Data.File.Name != 'CBQPFC_E', ]

## WE CAN EITHER REMOVE THE LAST ELEMENT OF THE PREVIOUSLY CREATED LIST
## OR RERUN THE STACKING WITH THE UPDATED INDEX. 
## WE WILL REMOVE THE FINAL ELEMENT TO SAVE PROCESSING POWER

allDataList <- allDataList[1:length(allDataList)-1]
dataNamesIndex <- dataNamesIndex[1:length(allDataList)-1]
## WE NEED TO RERUN THE REDUCE CALL THOUGH TO HAVE THE CORRECT NAMES
## WITHOUT THE .x or .y

allDataDF <- merge_dataframe_list(allDataList, matching_key = 'SEQN')

# LETS CHECK FOR DUPLICATE NAMES ONE MORE TIME

grep("\\.x$|\\.y$", names(allDataDF), value = T)

## WE ARE CLEAN!

## ADDING IN META-DATA ####
## USING A PURRR IMPLEMENTATION WONT WORK BECAUSE WHEN WE REBIND
## THE RESULTING COLUMNS WE WILL LOSE THE ATTRIBUTES
## DOING A SIMPLE FOR LOOP IS BEST

# badValues <- c('Refused', 'Missing', "Don't know") ## Set the values to ignore
badValues <- NA
pb <- progress::progress_bar$new(total = length(names(allDataDF))-1)

for(varName in setdiff(names(allDataDF), 'SEQN')) {
  
  originalTableName <- sapply(dataNamesIndex, function(nms) {
    any(grepl(glue::glue("^{varName}$"), nms))
  }) %>% 
    which(., T) %>% 
    names()
  
  if(length(originalTableName) > 1 | length(originalTableName) %in% 0) {
    
    cat('Could not find original table for ', varName, '. Ending...\n\n')
    break
    
  }
  
  metaData <- tryCatch(
    {
      nhanesA::nhanesCodebook(nh_table = originalTableName,
                              colname = varName)
    },
    error = function(e) { 
      cat('Could not find retrieve meta from API for ', varName, '. Skipping...\n\n')
      NULL
    }
  )
  
  if(is.null(metaData)) {
    
    cat('Could not find meta info for ', varName, 'Skipping...\n\n')
    next
    
  }
  
  ## ADDING META INFO TO THE DATASET
  allDataDF[[varName]] <- set_attr(allDataDF[[varName]], 
                                   which = 'questionText', 
                                   value = metaData$`English Text:`)
  
  allDataDF[[varName]] <- set_attr(allDataDF[[varName]], 
                                   which = 'generalInfo', 
                                   value = metaData$`SAS Label:`)
  
  ## LETS RECODE ALL THE VARIABLES THAT NEED IT
  valuesTable <- metaData[[varName]] %>% 
    dplyr::filter(!`Code or Value` %in% '.') %>% 
    dplyr::filter(Count > 0 & !`Value Description` %in% badValues)
  
  if(any(grepl('to', valuesTable$`Code or Value`))) { ## IF THERE IS A RANGE
    
    valuesTable$`Code or Value` %<>%
      gsub(" to ", ":", .)
  }
  
  pb$tick()
  
  ## LET'S DROP MISSING VALUES
  recodeString <- paste0(valuesTable$`Code or Value`, '=',
                         1:length(valuesTable$`Code or Value`), collapse= ';')
  
  allDataDF[[varName]] <- tryCatch(
    {
      car::recode(allDataDF[[varName]], recodeString)
    },
    error = function(e) { 
      cat('Could not recode variable ', varName, '. Skipping...\n\n')
      allDataDF[[varName]]
    }
  )
  
  levels(allDataDF[[varName]]) <- valuesTable$`Value Description`
  
}

## A CLEANED VARIABLE WITH THE RELEVANT QTEXT AND ANSWER OPTIONS ADDED
allDataDF$ALQ110
