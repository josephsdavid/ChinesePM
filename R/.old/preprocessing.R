library(xml2)
library(methods)
library(lubridate)
test <- read_xml("../data/train.xml")
str(test)
xml_children(test)
records <- xml_find_all(test,".//Record")
steps <- records[grepl(records, pattern = "HKQuantityTypeIdentifierStepCount" )]
print(steps[1])
library(magrittr)
steps <- records %>% xml_attr("value")
dates <- records %>% xml_attr("creationDate")

library(pipeR)
getrecords <- function(xs,val) {
	val2 <- paste0("HKQuantityTypeIdentifier", val)
	xs[grepl[xs, pattern = val2]] 
}



xml_attr(step, "Step")

