wiki4 <- read.csv("wiki4HE.csv"
                  , sep = ";"
                  , na.strings = "?")

turk <- read.csv("turkiye.csv")

yps <- read.csv("responses.csv")

library(xlsx)
read_jspf <- function(sheet, cols) {
  read.xlsx2("JSPF.xls"
             , sheetName = sheet
             , stringsAsFactors = FALSE
             , colClasses = c("numeric"
                              , "character"
                              , rep("numeric", cols)))
}

js <- read_jspf("JS", 13)
pf <- read_jspf("PF", 10)

oxis <- read.csv("oxis.csv")
names(oxis)[1] <- "qa01a"

temp <- tempfile()
download.file("http://personality-testing.info/_rawdata/BIG5.zip", temp, mode="wb")
perstest <- read.table(unz(temp, "BIG5/data.csv"), header = TRUE, sep="\t")
unlink(temp); rm(temp)


