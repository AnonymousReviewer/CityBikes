
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
yearly_users = user_duration$id_client[user_duration$maxnbdays>=365&user_duration$id_client!=(-1)]
# 59542
yearly_users = user_duration$id_client[user_duration$maxadaptedyear>1]
# 59542

loyal_users = user_duration$id_client[user_duration$maxnbdays>=3*365]
# 20332

ftrips =  trips %>% filter(id_client%in%yearly_users)

####
# Table 1

ftrips %>% group_by(year) %>% summarize(
  trips = n(),
  users = length(unique(id_client)),
  trips_per_user = trips / users
)

ftrips %>% group_by(id_client,year) %>% summarize(ntrips = n()) %>% group_by(year) %>% summarize(
  mediantrips = median(ntrips),
  sdtrips = sd(ntrips)
)




####
# Table 2

customers$age_class = cut(customers$age,breaks = c(0,13,22,32,42,52,62,72,100))

customers %>% filter(id_client%in%yearly_users) %>% group_by(age_class) %>% summarize(
  nusers = n(),
  nloyal = length(which(id_client%in%loyal_users)),
  percentage_men = 100*length(which(gender=="Man"))/nusers,
  percentage_loyal =100*nloyal / nusers,
  percentage_loyal_men = 100*length(which(gender=="Man"&id_client%in%loyal_users))/nloyal
)






