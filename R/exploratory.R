## This code is an intial exploration of the data

## set working directory to source file location

library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

#### Read in Data ####
train <- read.csv("../datasets/train_2016.csv", header = TRUE)  # training data
submit <- read.csv("../datasets/sample_submission.csv", header = TRUE)  # sample submission
# props <- read.csv("../datasets/properties_2016.csv", header = TRUE)  # data: big file

# #### Create Smaller Data Set: n = 200,000 samples (Already Done) ####
# training set
train.ids <- unique(train$parcelid)
train.prop <- props[props$parcelid %in% train.ids,]

# smaller test set
samp <- sample(1:nrow(props), 200000)
samp.dat <- props[samp,] 
samp.test.prop <- samp.dat %>% filter(!(parcelid %in% train.ids))

saveRDS(train.prop, '../datasets/train_properties')
write.csv(train.prop, '../datasets/train_properties.csv', row.names = F)
saveRDS(samp.test.prop, '../datasets/sample_test_properties')
write.csv(samp.test.prop, '../datasets/sample_test_properties.cvs', row.names = F)

#### Play Around with Sample Data ####
dat <- readRDS('../datasets/train_properties')

str(dat)

# parcelid is unique
length(unique(dat$parcelid)) == length(dat$parcelid)

# percent of NAs per feature -> lots of NAs
percent.na <- function(x) {
  return(sum(is.na(x)) / length(x))
}
percent.na.by.col <- apply(dat, 2, percent.na)

# remove columns with NA > 30% for now
na.cols <- colnames(dat)[percent.na.by.col > .3]
dat_e <- dat %>% select(-one_of(na.cols))
dim(dat_e)

# convert some columns to factors
dat_e <- dat_e %>% 
  mutate_at(vars(fips,
                 propertycountylandusecode,
                 propertylandusetypeid,
                 regionidcity,
                 regionidcounty,
                 regionidzip),
            funs(as.factor))

# reshape training set to wide format
str(train)
length(unique(train$parcelid))
train.wide <- train %>% spread(transactiondate, logerror)

# training set by month
date.to.month <- function(x) {
  return(as.factor(substr(as.character(x), 1, 7)))
}
train.month <- train %>%
  mutate_at(vars(transactiondate),
            funs(date.to.month)) %>%
  distinct(parcelid, transactiondate, .keep_all = TRUE)

# reshape training set to wide format by month
train.month.wide <- train.month %>% spread(transactiondate, logerror)
# write.csv(train.month.wide, "../datasets/train_2016_wide.csv", row.names = F)
# saveRDS(train.month.wide, "../datasets/train_2016_wide.rds")

# number of sales by id by month
sales.per.id <- apply(train.month.wide, 1, 
                      FUN = function(x) {length(x) - sum(is.na(x)) - 1})
table(sales.per.id)

# plot logerror over months -> are there trends in logerror by month
train.month$transactiondate <- as.factor(as.yearmon(train.month$transactiondate))
ggplot(train.month) +
  aes(x = transactiondate, y = logerror, color = transactiondate) +
  geom_boxplot() +
  xlab("Transaction Date")

# ids with multiple transactions -> is there relationship in logerror over time? Yes
duplicated.ids <- train.month.wide$parcelid[sales.per.id > 1]
duplicated.train.month <- train.month %>% filter(parcelid %in% duplicated.ids)
ggplot(duplicated.train.month) +
  aes(x = transactiondate, y = logerror, group = as.factor(parcelid)) +
  geom_line(alpha = 0.5) +
  xlab("Transaction Month") +
  ggtitle("Duplicated Sales")

#### Merge Train X and y ####
train.y.ave <- data.frame(parcelid = train.month.wide$parcelid,
                          y_ave = rowMeans(train.month.wide[,-1], na.rm = TRUE))

train <- merge(dat, train.y.ave, by = "parcelid")
saveRDS(train, '../datasets/train_merged.rds')
write.csv(train, '../datasets/train_merged.csv')
