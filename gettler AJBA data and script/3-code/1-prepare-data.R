
# Clear workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(kinship2)
library(geosphere)
library(reshape2)

# this is a "not in" function.
"%!in%" <- function(x, y) !("%in%"(x, y))

setwd("/Users/danielredhead/Bayaka-social-networks")

# Load primary sources
people <- read.csv("./1-data/indiv.csv", stringsAsFactors = F)
nodelist <- read.csv("./1-data/nodelist.csv", stringsAsFactors = F)
kinship <- read.csv("./1-data/kinship.csv", stringsAsFactors = F)
coordinates <- read.csv("./1-data/coordinates.csv", stringsAsFactors = F)

# Standardize characters and types
colnames(people) <- tolower(colnames(people))
colnames(nodelist) <- tolower(colnames(nodelist))
colnames(kinship) <- tolower(colnames(kinship))
colnames(coordinates) <- tolower(colnames(coordinates))
people <- data.frame(lapply(people, tolower), stringsAsFactors = F)
nodelist <- data.frame(lapply(nodelist, as.character), stringsAsFactors = F)
nodelist <- data.frame(lapply(nodelist, tolower), stringsAsFactors = F)
coordinates <- data.frame(lapply(coordinates, tolower), stringsAsFactors = F)

people <- people[order(people$individ),]

ids <- (unique(nodelist$ego))
people <- people[people$individ %in% ids,]
# Subset to ids that appear in the nodelist


net <- pivot_longer(nodelist, cols = !c("ego", "question"))

net <- net[!is.na(net$value) & net$value %in% people$individ & net$ego %in% people$individ,c(1,2,4)]

##############################################
############ Question 1 ######################
##############################################

net_1 <- net[net$question == 3,]

matrix_1 <- matrix(10, nrow = length(people$individ), ncol = length(people$individ), dimnames = list(people$individ, people$individ))

for(i in 1:nrow(matrix_1)){
  for(j in 1:ncol(matrix_1)){
      if(nrow(net_1[net_1$ego == rownames(matrix_1)[i] & net_1$value == colnames(matrix_1)[j],]) > 0){
        matrix_1[i,j] <- 1
      } else {
        matrix_1[i,j] <- 0
      }
  }
}

write.csv(matrix_1, file = "./2-analyses/q3_all_matrix.csv")


net_2 <- net[net$question == 4,]

matrix_2 <- matrix(10, nrow = length(people$individ), ncol = length(people$individ), dimnames = list(people$individ, people$individ))

for(i in 1:nrow(matrix_2)){
  for(j in 1:ncol(matrix_2)){
      if(nrow(net_2[net_2$ego == rownames(matrix_2)[i] & net_2$value == colnames(matrix_2)[j],]) > 0){
        matrix_2[i,j] <- 1
      } else {
        matrix_2[i,j] <- 0
      }
  }
}

write.csv(matrix_2, file = "./2-analyses/q4_all_matrix.csv")


####################################################
############ Question 5,6,7,8 ######################
####################################################

net_5 <- net[net$question == 5,]

matrix_5 <- matrix(10, nrow = length(people$individ), ncol = length(people$individ), dimnames = list(people$individ, people$individ))

for(i in 1:nrow(matrix_5)){
  for(j in 1:ncol(matrix_5)){
    if(nrow(net_5[net_5$ego == rownames(matrix_5)[i] & net_5$value == colnames(matrix_5)[j],]) > 0){
      matrix_5[i,j] <- 1
    } else {
      matrix_5[i,j] <- 0
    }
  }
}

matrix_5 <- matrix_5[rownames(matrix_5) %in% people$individ[people$sex == "f"], ]

net_6 <- net[net$question == 6,]

matrix_6 <- matrix(10, nrow = length(people$individ), ncol = length(people$individ), dimnames = list(people$individ, people$individ))

for(i in 1:nrow(matrix_6)){
  for(j in 1:ncol(matrix_6)){
    if(nrow(net_6[net_6$ego == rownames(matrix_6)[i] & net_6$value == colnames(matrix_6)[j],]) > 0){
      matrix_6[i,j] <- 1
    } else {
      matrix_6[i,j] <- 0
    }
  }
}


matrix_6 <- matrix_6[rownames(matrix_6) %in% people$individ[people$sex == "m"], ]

foraging_net <- rbind(matrix_5,matrix_6)

net_7 <- net[net$question == 7,]

matrix_7 <- matrix(10, nrow = length(people$individ), ncol = length(people$individ), dimnames = list(people$individ, people$individ))

for(i in 1:nrow(matrix_7)){
  for(j in 1:ncol(matrix_7)){
    if(nrow(net_7[net_7$ego == rownames(matrix_7)[i] & net_7$value == colnames(matrix_7)[j],]) > 0){
      matrix_7[i,j] <- 1
    } else {
      matrix_7[i,j] <- 0
    }
  }
}

matrix_7 <- matrix_7[rownames(matrix_7) %in% people$individ[people$sex == "f"], ]

net_8 <- net[net$question == 8,]

matrix_8 <- matrix(10, nrow = length(people$individ), ncol = length(people$individ), dimnames = list(people$individ, people$individ))

for(i in 1:nrow(matrix_8)){
  for(j in 1:ncol(matrix_8)){
    if(nrow(net_8[net_8$ego == rownames(matrix_8)[i] & net_8$value == colnames(matrix_8)[j],]) > 0){
      matrix_8[i,j] <- 1
    } else {
      matrix_8[i,j] <- 0
    }
  }
}

matrix_8 <- matrix_8[rownames(matrix_8) %in% people$individ[people$sex == "m"], ]

gossip_net <- rbind(matrix_7,matrix_8)


write.csv(matrix_5, file = "./2-analyses/q5_matrix.csv")
write.csv(matrix_6, file = "./2-analyses/q6_matrix.csv")
write.csv(matrix_7, file = "./2-analyses/q7_matrix.csv")
write.csv(matrix_8, file = "./2-analyses/q8_matrix.csv")
write.csv(foraging_net, file = "./2-analyses/foraging_matrix.csv")
write.csv(gossip_net, file = "./2-analyses/gossip_matrix.csv")


people <- people[people$individ %in% colnames(matrix_2),]
people_sort <- data.frame(individ = colnames(matrix_2), stringsAsFactors = F)[,,drop = F]
people_sorted <- left_join(people_sort, people, by = "individ")

write.csv(people_sorted, file = "./2-analyses/indiv_sorted.csv", row.names = F)

####################################################
################# Relatedness ######################
####################################################

k <- kinship(pedigree(id = kinship$ego, dadid = kinship$father, momid = kinship$mother, sex = kinship$sex))
k2 <- k[row.names(k) %in% people$individ, ]
k2 <- k2[, colnames(k) %in% people$individ ]
k2 <- k2[order(as.numeric(row.names(k2))),order(as.numeric(row.names(k2)))]
k2 <- k2*2
all(row.names(k2) == people_sort)
all(colnames(k2) == people_sort)

write.csv(k2, file = "./2-analyses/relatedness.csv", row.names = T)


####################################################
################### Proximity ######################
####################################################

coords <- data.frame(select(people_sorted, individ, sharingunitid))
coords$latitude <- coordinates$latitude[match(coords$sharingunitid, coordinates$sharingunitid)]
coords$longitude <- coordinates$longitude[match(coords$sharingunitid, coordinates$sharingunitid)]

distance <- distm (cbind (coords$longitude, coords$latitude))
rownames(distance) = coords$individ
colnames(distance) = coords$individ
all(row.names(distance) == people_sort)
all(colnames(distance) == people_sort)

write.csv(distance, file = "./2-analyses/distance.csv", row.names = T)

####################################################
###################### SEX #########################
####################################################

# Create a long dataframe with every combination of ID's
matrix <- data.frame('i' = people$individ, 'sex_i' = people$sex)
matrix1 <- data.frame('j' = people$individ, 'sex_j' = people$sex)

sex_df <- merge(matrix, matrix1)

# Remove entries where i and j are the same ID
sex_df <- sex_df[which(sex_df$i != sex_df$j),]

rm(matrix, matrix1)

sex_df$value <- ifelse((sex_df$sex_i ==  sex_df$sex_j), 1, 0)

cleaned_df <- sex_df[,-2]
cleaned_df <- cleaned_df[,-3]

same_sex <- acast(cleaned_df, i ~ j , value.var='value')
same_sex[is.na(same_sex)] <- 0
same_sex <- same_sex[match(people_sort$individ, rownames(same_sex)), match(people_sort$individ, colnames(same_sex))]

write.csv(same_sex, file = "./2-analyses/same_sex.csv", row.names = T)

sex_df$value <- ifelse((sex_df$sex_i == "f" & sex_df$sex_j == "f"), 1, 0)

cleaned_df <- sex_df[,-2]
cleaned_df <- cleaned_df[,-3]

same_sex <- acast(cleaned_df, i ~ j , value.var='value')
same_sex[is.na(same_sex)] <- 0
same_sex <- same_sex[match(people_sort$individ, rownames(same_sex)), match(people_sort$individ, colnames(same_sex))]

write.csv(same_sex, file = "./2-analyses/same_female.csv", row.names = T)

# Same male network

sex_df$value <- ifelse((sex_df$sex_i == "m" & sex_df$sex_j == "m"), 1, 0)

cleaned_df <- sex_df[,-2]
cleaned_df <- cleaned_df[,-3]

same_sex <- acast(cleaned_df, i ~ j , value.var='value')
same_sex[is.na(same_sex)] <- 0
same_sex <- same_sex[match(people_sort$individ, rownames(same_sex)), match(people_sort$individ, colnames(same_sex))]

write.csv(same_sex, file = "./2-analyses/same_male.csv", row.names = T)


# Diff sex network

sex_df$value <- ifelse((sex_df$sex_i == "m" & sex_df$sex_j == "f" | sex_df$sex_i == "f" & sex_df$sex_j == "m"), 1, 0)

cleaned_df <- sex_df[,-2]
cleaned_df <- cleaned_df[,-3]

same_sex <- acast(cleaned_df, i ~ j , value.var='value')
same_sex[is.na(same_sex)] <- 0
same_sex <- same_sex[match(people_sort$individ, rownames(same_sex)), match(people_sort$individ, colnames(same_sex))]

write.csv(same_sex, file = "./2-analyses/diff_sex.csv", row.names = T)


# END OF SCRIPT
