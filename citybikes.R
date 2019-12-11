
setwd(paste0(Sys.getenv('CS_HOME'),'/Reviews/PLOSONE/PONE-D-19-27375/reproduction'))

library(feather)
library(dplyr)
library(survival)

customers = read_feather('data/customer_details.feather')
customers$age = 2019 - customers$birth_year

trips = rbind(
  read_feather('data/customer_trips_part1.feather'),
  read_feather('data/customer_trips_part2.feather'),
  read_feather('data/customer_trips_part3.feather')
)

# unique users in trip table = 3825818
length(unique(trips$id_client))

user_duration = trips %>% group_by(id_client) %>% summarize(
  ntrips = n(),
  firsttrip = min(date.in),
  lasttrip = max(date.in),
  duration = lasttrip - firsttrip,
  maxnbdays = max(nb.days),
  maxadaptedyear = max(adapted.year)
)

yearly_users = user_duration$id_client[user_duration$duration>=365]
length(yearly_users)
# 59549
# should be 120,827

loyal_users = user_duration$id_client[user_duration$duration>3*365]
length(loyal_users)
#20332
# should be 25,963

# try filtering trips on adapted year
trips %>% f


####
# Table 1

yearlycounts = trips %>% group_by(year) %>% summarize(
  trips = n(),
  users = length(unique(id_client))
)






####
# Table 2

rownames(customers)<-customers$id_client

customers$age_class = cut(customers$age,breaks = c(0,13,22,32,42,52,62,72,100))

table(customers[loyal_users,c('age_class',"gender")])






