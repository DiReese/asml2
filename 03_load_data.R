# Write code here to load the data you downloaded in download_data.R

uci.bank <- readRDS("Data/uci_bank.rds")

# You might choose to do any resampling here to ensure it is consistent across
# models

#set.seed(7482) # set seed for reproducibility

#library("rsample")
#uci.bank.rsample <- rsample::vfold_cv(uci.bank, v = 3, strata = y)
