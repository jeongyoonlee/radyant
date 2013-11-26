require(shiny)

output <- list()

setwd('/Users/vnijs/radyant/inst/marketing')
source('global.R', local = TRUE)
source('server.R', local = TRUE)
source('radyant.R', local = TRUE)
source('ui.R', local = TRUE)

	# source data & analysis tools
flist_analysis <- sourceDirectory('tools/analysis', recursive = TRUE)
flist_data <- sourceDirectory('tools/data', recursive = TRUE)

output$tester <- reactive({
	regression()
})

output$tester()


# test <- matrix(rnorm(400),ncol = 4)
# test <- cbind(test,test) + matrix(rnorm(800, 0, .3), ncol = 8)
# cor(test)
# test <- data.frame(test)
# colnames(test) <- paste0('X',1:8)
# head(test)
# save(test, file = '~/Desktop/test.rda')


# might the below work for testing?
# just load some parts of the app and make sure the work as expected 
# with specific data and input parameters?
library(shiny)
runApp(list(
  ui = pageWithSidebar(
    # ...
  ),
  server = function(input, output, session) {
    # ...
  }
))


# maybe combined with stopApp()
# https://groups.google.com/forum/#!msg/shiny-discuss/QegGiX9qBcg/d00NvOIKWRYJ

# the url parse thing also seems to offer the opportunity to (re)set a bunch of parameters in the input 
# list


example(mean)

require(dplyr)
dir.create('~/Desktop/dplyr')
setwd('~/Desktop/dplyr')
# Download all zip files
library(lubridate)

source <- c(
  "ftp://ftp.fec.gov/FEC/2012/indiv12.zip",
  "ftp://ftp.fec.gov/FEC/2014/cn14.zip",
  "ftp://ftp.fec.gov/FEC/2014/cm14.zip"
)
target <- basename(source)

missing <- !file.exists(target)
Map(download.file, source[missing], target[missing])

# Unzip, read csv and save as rds
cache <- function(zip_path, rds_path) {
  filename <- unzip(zip_path, list = TRUE)[[1]]  
  unzipped <- unzip(zip_path, filename, exdir = tempdir())
  on.exit(unlink(unzipped))

  df <- read[[zip_path]](unzipped)
  class(df) <- c("tbl_cpp", "tbl", class(df))
  saveRDS(df, rds_path)
}

# List of functions for parsing each individual file
read <- list(
  "indiv12.zip" = function(path) {
    df <- read.delim(path, stringsAsFactors = FALSE, sep = "|", 
      header = FALSE)
    names(df) <- tolower(c("CMTE_ID", "AMNDT_IND", "RPT_TP", "TRANSACTION_PGI", 
      "IMAGE_NUM", "TRANSACTION_TP", "ENTITY_TP", "NAME", "CITY", "STATE", 
      "ZIP_CODE", "EMPLOYER", "OCCUPATION", "TRANSACTION_DT", "TRANSACTION_AMT", 
      "OTHER_ID", "TRAN_ID", "FILE_NUM", "MEMO_CD", "MEMO_TEXT", "SUB_ID"))
    df$transaction_dt <- as.Date(mdy(df$transaction_dt))
    df
  },
  "cn14.zip" = function(path) {
    # http://www.fec.gov/finance/disclosure/metadata/DataDictionaryCandidateMaster.shtml
    df <- read.delim(path, stringsAsFactors = FALSE, sep = "|", header = FALSE,
      quote = "")
    names(df) <- tolower(c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION", 
      "CAND_ELECTION_YR", "CAND_OFFICE_ST", "CAND_OFFICE", 
      "CAND_OFFICE_DISTRICT", "CAND_ICI", "CAND_STATUS", "CAND_PCC",
      "CAND_ST1", "CAND_ST2", "CAND_CITY", "CAND_ST", "CAND_ZIP"))
    df
  }, 
  "cm14.zip" = function(path) {
    # http://www.fec.gov/finance/disclosure/metadata/DataDictionaryCommitteeMaster.shtml
    df <- read.delim(path, stringsAsFactors = FALSE, sep = "|", header = FALSE,
      quote = "")
    names(df) <- tolower(c("CMTE_ID", "CMTE_NM", "TRES_NM", "CMTE_ST1", 
      "CMTE_ST2", "CMTE_CITY", "CMTE_ST", "CMTE_ZIP", "CMTE_DSGN", "CMTE_TP",
      "CMTE_PTY_AFFILIATION", "CMTE_FILING_FREQ", "ORG_TP", "CONNECTED_ORG_NM",
      "CAND_ID"))
    df
  }
)

cached <- gsub("zip", "rds", target)
missing <- !file.exists(cached)

Map(cache, target[missing], cached[missing])

ls()

