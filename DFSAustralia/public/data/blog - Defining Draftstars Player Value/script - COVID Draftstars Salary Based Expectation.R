library(plyr)
library(dplyr)
library(jsonlite)

competition <- "AFL" # used to filter the data by sport
comp_type <- "Micro" # used to filter on competition type
fee <- 0.5

setwd("~/Dropbox/Daily Fantasy Sports/AFL/2020 Code/Supporting Files (JSON & CSV)/Live Tournament JSON")

temp<-list.files(pattern="DS")

contests <- list()
print("Extracting Contest Details")
pb <- txtProgressBar(min = 0, max = length(temp), style = 3)
for (i in 1:length(temp)){
  data <- jsonlite::fromJSON(temp[i], simplifyDataFrame = TRUE)
  contests[[i]] <- data.frame(contest = data$data$name, id = data$data$id,
                              payout_type = data$data$payout_type$description,
                              competition = data$data$competition_type$short_name,
                              fee = data$data$total_fee/100,
                              pool = data$data$prize_pool_value/100,
                              date = as.Date(data$data$start_time))
  setTxtProgressBar(pb, i)}

contests <- do.call(rbind,contests)
contests <- unique(contests)

contests <- contests[contests$competition %in% competition,]
contests <- contests[grepl(comp_type,contests$contest, fixed = TRUE),]


scores<-list()
for(i in 1:nrow(contests)){
  print(paste0("Downloading ",i," of ",nrow(contests)," contests"))
url <- paste0("https://api2.draftstars.com.au/v1.1/challenges/",contests$id[i],"?embed=current_user_entry,games.players")
data<-jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
scores[[i]]<-do.call(rbind,data$data$games$players)
}

scores <- do.call(rbind,scores)
scores <- select(scores,c(salary, fantasy_score))
scores <- scores[scores$fantasy_score > 0,]

scores.fit <- lm(fantasy_score ~ salary, data=scores)
intercept <- scores.fit$coefficients[1]
slope <- scores.fit$coefficients[2]
Salary <- seq(5000,20000,by = 1000) 
SBE <- round(intercept + slope * Salary,1)
Salx <- round(SBE * 1000 / Salary,2)
value.table <- data.frame('Draftstars Salary' = Salary, 'Salary Based Expectation' = SBE, 'Multiple of Salary' = Salx)

setwd("~/Desktop")
write.csv(value.table, file = "COVID Draftstars Salary Based Expectation.csv", row.names = FALSE)
