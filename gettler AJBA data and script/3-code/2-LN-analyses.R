# Clear workspace
rm(list = ls())

# Set working directory
#setwd("/Users/danielredhead/Bayaka-social-networks")
setwd("./Bayaka-social-networks")

#Load packages
library(STRAND)


# Load functions

normalize <- function(y) {
    x<-y[!is.na(y)]
    x<-(x - min(x)) / (max(x) - min(x))
    y[!is.na(y)]<-x
    return(y)
}

#######################################
#         LOAD & SPECIFY DATA         #
#######################################

# Load attributes
att <- read.csv("./2-analyses/indiv_sorted.csv", stringsAsFactors = FALSE, header = TRUE)
distance <- as.matrix(read.csv("./2-analyses/distance.csv", stringsAsFactors = FALSE, header = TRUE, row.names = 1))
relatedness <- as.matrix(read.csv("./2-analyses/relatedness.csv", stringsAsFactors = FALSE, header = TRUE, row.names = 1))
same_male <- as.matrix(read.csv("./2-analyses/same_male.csv", stringsAsFactors = FALSE, header = TRUE, row.names = 1))
same_female <- as.matrix(read.csv("./2-analyses/same_female.csv", stringsAsFactors = FALSE, header = TRUE, row.names = 1))
diff_sex <- as.matrix(read.csv("./2-analyses/diff_sex.csv", stringsAsFactors = FALSE, header = TRUE, row.names = 1))

# Load networks
sharingout <- as.matrix(read.table("./2-analyses/q3_all_matrix.csv", sep = ",", header = TRUE, row.names = 1))
sharingin <- as.matrix(read.table("./2-analyses/q4_all_matrix.csv", sep = ",", header = TRUE, row.names = 1))
foraging <- as.matrix(read.table("./2-analyses/foraging_matrix.csv", sep = ",", header = TRUE, row.names = 1))
gossip <- as.matrix(read.table("./2-analyses/gossip_matrix.csv", sep = ",", header = TRUE, row.names = 1))

# Recode gender and status
att$sex <- ifelse(att$sex == "f", "female", "male")
att$status <- ifelse(att$status_menscouncil == 1| att$status_womencouncil == 1|
                        att$status_healer == 1, 1, 0  )
att$age_2 <- att$age^2

# Create the STRAND data object
nets <- list( food_out=sharingout , food_in=sharingin )

ind <- data.frame( Age = center(att$age), Age_2 = center(att$age_2))

dyad <- list( Relatedness = relatedness  , Distance = normalize(distance),
             Foraging = foraging, Gossip = gossip, Same_Female = same_female,
             Different_Sex = diff_sex )

group_ids <- data.frame(Status = factor(att$status)) 
#Household = factor(att$sharingunitid)

model_dat <- make_strand_data( self_report=nets,
			block_covariates = group_ids, individual_covariates=ind, dyadic_covariates=dyad)

#B_priors <- make_priors(priors_to_change= c("B_ingroup" = 0.01, "B_outgroup" = 0.1), include_rownames=T)


fit <- fit_latent_network_model( data=model_dat,
      fpr_regression = ~ 1, 
      rtt_regression = ~ Age, 
      theta_regression = ~ 1, 
      block_regression = ~  Status,
      focal_regression = ~ Age, 
      target_regression = ~ Age, 
      dyad_regression = ~ Distance + Relatedness + Foraging + Gossip + Same_Female + Different_Sex +
              + Same_Female * Relatedness  + Different_Sex * Relatedness,
       mode="mcmc",   
       return_latent_network = TRUE,
    stan_mcmc_parameters = list(chains = 2, parallel_chains = 2, refresh = 100,
    iter_warmup = 3000, iter_sampling = 6000,
     max_treedepth = NULL, adapt_delta = .98)
      )


res <- summarize_strand_results(fit)

# Simulate out predictions and plot the results
# Maybe plot the contrasts? 


# Extract estimated network
est_net <- round(apply(res$samples$latent_network_sample,2:3,median), 3)
rownames(est_net) <- rownames(sharingout)
colnames(est_net) <- rownames(sharingout)

estnet2 <- round(est_net, )

sr <- array(NA, dim(res$samples$srm_model_samples$focal_target_random_effects))
for( q in 1:12000) {
    for( j in 1:112 ){
      # Check line below with Richard to make sure its all good
      sr[q,j, ] = (diag(res$samples$srm_model_samples$focal_target_sd[q, ]) %*% res$samples$srm_model_samples$focal_target_L[q, , ]) %*% res$samples$srm_model_samples$focal_target_random_effects[q,j, ];
  }
} 

mean_sr <- data.frame(apply(sr, 2:3, mean))
mean_sr$id <- att$individ
mean_sr$su_id <- att$sharingunitid
mean_sr$sex <- att$sex
mean_sr$head <- att$head
colnames(mean_sr)[1:2] <- c("sender", "receiver")
write.csv(est_net, "./2-analyses/outputs/probs-estimated-network.csv")
write.csv(estnet2, "./2-analyses/outputs/binary-estimated-network.csv")
write.csv(mean_sr, "./2-analyses/outputs/parents-extracted-sender-receiver.csv", row.names = FALSE)

# Calculate range of in and out degree
lnm_r_outdeg <- range(rowSums(estnet2))
lnm_r_indeg <- range(colSums(estnet2))

# Create summary table
LNM_descriptives <- as.data.frame(t(data.frame(
  n_su = nrow(estnet2),
  n_ties = sum(estnet2),
    density = round(edge_density(graph_from_adjacency_matrix(estnet2)), 3),   
    reciprocity = round(reciprocity(graph_from_adjacency_matrix(estnet2)), 3),                 # get the network density
    transitivity = round(transitivity(graph_from_adjacency_matrix(estnet2)), 3),                 # general tendency towards transitivity
    outdeg_centralization = round((centralization.degree(graph_from_adjacency_matrix(estnet2), mode = "out")$centralization), 3),
    indeg_centralization = round((centralization.degree(graph_from_adjacency_matrix(estnet2), mode = "in")$centralization), 3),
    range_outdegree = paste(lnm_r_outdeg[1], lnm_r_outdeg[2], sep = " - "),
    range_indegree = paste(lnm_r_indeg[1], lnm_r_indeg[2], sep = " - ")
    )))

write.csv(LNM_descriptives, "./2-analyses/lnm_descriptives_table.csv")


save.image("./2-analyses/outputs/analyses.RData")

# END OF SCRIPT
