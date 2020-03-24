# Write code here to download the data you are using for your report.
# DO NOT push the data to your Github repository.

# For example, to download the simple Iris data from the UCI Machine Learning
# Repository
temp <- tempfile()
download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip',
              temp)
uci.bank <- read.csv(unzip(zipfile = temp,
                           files = 'bank-additional/bank-additional-full.csv', junkpaths = TRUE),
                     header = TRUE, sep = ';',
                     stringsAsFactors = FALSE)

library("dplyr")
library("forcats")


# Save into Data directory which is not pushed to Github
saveRDS(uci.bank, "Data/uci_bank.rds")
