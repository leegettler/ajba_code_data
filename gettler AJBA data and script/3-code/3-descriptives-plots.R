# Clear workspace
rm(list = ls())

# Set working directory
#setwd("/Users/danielredhead/Bayaka-social-networks")
#setwd("./Bayaka-social-networks")
setwd("/Users/danielredhead/Dropbox/Bayaka-social-networks")

#Load packages
library(tidyverse)
library(ggplot2)
library(igraph)
library(RColorBrewer)
library(rethinking)
#######################################
#         LOAD & SPECIFY DATA         #
#######################################

# Load attributes
att <- read.csv("./2-analyses/indiv_sorted.csv", stringsAsFactors = FALSE, header = TRUE)
distance <- as.matrix(read.csv("./2-analyses/distance.csv", stringsAsFactors = FALSE, header = TRUE, row.names = 1))
relatedness <- as.matrix(read.csv("./2-analyses/relatedness.csv", stringsAsFactors = FALSE, header = TRUE, row.names = 1))
indiv <- read.csv("./1-data/BY_Indiv.csv", stringsAsFactors = FALSE, header = TRUE)

# Load networks
sharing <- as.matrix(read.table("./2-analyses/outputs/probs-estimated-network.csv", sep = ",", row.names = 1 , header = TRUE))
bin_sharing <- as.matrix(read.table("./2-analyses/outputs/binary-estimated-network.csv", sep = ",", row.names = 1 , header = TRUE))
sharingout <- as.matrix(read.table("./2-analyses/q3_all_matrix.csv", sep = ",", header = TRUE, row.names = 1))
sharingin <- as.matrix(read.table("./2-analyses/q4_all_matrix.csv", sep = ",", header = TRUE, row.names = 1))
foraging <- as.matrix(read.table("./2-analyses/foraging_matrix.csv", sep = ",", header = TRUE, row.names = 1))
gossip <- as.matrix(read.table("./2-analyses/gossip_matrix.csv", sep = ",", header = TRUE, row.names = 1))


att$sex <- ifelse(att$sex == "f", 1,0)
att$status <- ifelse(att$status_menscouncil == 1| att$status_womencouncil == 1|
                        att$status_healer == 1, 1, 0  )

probs_net <- graph_from_adjacency_matrix(sharing, mode= "directed", weighted = TRUE )

#######################################
#        VARIABLE DESCRIPTIVES        #
#######################################

# In-text descriptives
# N of households
n_hh <- length(unique(att$sharingunitid))

# % of sample
denom <- indiv$IndivID[indiv$Neighbourhood == "Ngele" & indiv$Age >= 18]
denom <- denom[!is.na(denom)]
length(denom)

sample_perc <- round(nrow(sharing)/length(denom)*100, 0)

# Check the names the don't appear.
as.numeric(substring(denom, 3))[!as.numeric(substring(denom, 3)) %in% rownames(sharing)]

rgen = range(att$sex)
rage = range(att$age)
rstat = range(att$status)
rrelate = range(relatedness)
rdist = range(distance)

# attributes

var_desc <- data.frame(
    m_gender = round(mean(att$sex), 3),
    r_gender = paste(rgen[1], rgen[2], sep = " - "),
    m_age = round(mean(att$age), 3),
    sd_age = round(sd(att$age), 3),
    r_age = paste(rage[1], rage[2], sep = " - "),
    m_status = round(mean(att$status), 3),
    r_status = paste(rstat[1], rstat[2], sep = " - "),
    m_relate = round(mean(relatedness), 3),
    sd_relate = round(sd(relatedness), 3),
    r_relate = paste(rrelate[1], rrelate[2], sep = " - "),
    m_dist = round(mean(distance), 3),
    sd_dist = round(sd(distance), 3),
    r_dist = paste(rdist[1], rdist[2], sep = " - ")
        )

write.csv(var_desc, "./2-analyses/outputs/variable_descriptives.csv")


# networks

networks <- list(bin_sharing,
                 sharingout,
                 sharingin,
                 foraging,
                 gossip)

rou <- list()
rin <- list()
net_desc <- list()

for( i in 1:length(networks)) {
    rou[[i]] = range(rowSums(networks[[i]]))
    rin[[i]] = range(colSums(networks[[i]]))

    net_desc[[i]] <- t(data.frame(
        n_ties = sum(networks[[i]], na.rm = T),
        density = round(edge_density(graph_from_adjacency_matrix(networks[[i]])), 3),                 # get the network density
        reciprocity = round(reciprocity(graph_from_adjacency_matrix(networks[[i]])), 3),                  # general tendency towards reciprocity
        transitivity = round(transitivity(graph_from_adjacency_matrix(networks[[i]])), 3),                 # general tendency towards transitivity
        m_degree = round(mean(rowSums(networks[[i]])),3) ,             # indegree for plotting
        r_indegree = paste(rin[[i]][1], rin[[i]][2], sep = " - "),
        r_outdegree = paste(rou[[i]][1], rou[[i]][2], sep = " - ")
    )
  )

}

net_descriptives <- do.call(cbind, net_desc)

colnames(net_descriptives) <- c("est_net", "sharing_out", "sharing_in", "foraging", "gossip")

write.csv(net_descriptives, "./2-analyses/outputs/full_network_descriptives.csv")

#######################################
#               DIGRAPHS              #
#######################################

V(probs_net)$name <- att$individ

V(probs_net)$gender <- att$sex[match(V(probs_net)$name, att$individ)]
V(probs_net)$gender[V(probs_net)$gender == 0] <- 2
V(probs_net)$degree <- degree(probs_net)
V(probs_net)$colour[V(probs_net)$gender == 1] <- "darkseagreen3"
V(probs_net)$colour[V(probs_net)$gender == 2] <- "deepskyblue4"

png("./2-analyses/outputs/nework_digraph.png", width = 1000, height = 1000, res = 120)
plot(probs_net, edge.arrow.size=0.15, edge.arrow.width= 0.5+E(probs_net)$weight, weights = E(probs_net)$weight, vertex.color= V(probs_net)$colour, vertex.size=5,
      vertex.label = NA, edge.curved=0.4, edge.width = E(probs_net)$weight, layout = layout_with_fr)
dev.off()

#######################################
#               RESULTS              #
#######################################

load("./2-analyses/outputs/analyses.RData")

# Interaction plot
## Sex & relatedness results: Plot the prediction from the three parameters.

## Make counterfactual assumptions
## Hold everything but relatedness/male-male/female-female/female-male
rel <- res$samples$srm_model_samples$dyadic_coeffs[,2]
same_female <- res$samples$srm_model_samples$dyadic_coeffs[,5]
diff_sex <- res$samples$srm_model_samples$dyadic_coeffs[,6]
same_female_X_rel <- res$samples$srm_model_samples$dyadic_coeffs[,7]
diff_sex_X_rel <- res$samples$srm_model_samples$dyadic_coeffs[,8]

Q <- 12000
K <- 100

pred1 <- matrix(NA, Q, K)
pred2 <- matrix(NA, Q, K)
pred3 <- matrix(NA, Q, K)

relatedness <- seq(0,0.5,length.out = K)
## Simulate

for(q in 1:Q) {
    for(k in 1:100) {
        # Parameters that match same female (e.g., b = same_female, c = relatedness d = same_female * relatedness)
        pred1[q, k] <-  same_female[q] + (rel[q] + same_female_X_rel[q])*relatedness[k]

        # Predictions about male_male ties
        pred2[q, k] <- rel[q]*relatedness[k]

        # PRedictions about diff sex ties (e.g., e = diff_sex, c = relatedness, f = diff_sex * relatedness)
        pred3[q, k] <-  diff_sex[q] + (rel[q] + diff_sex_X_rel[q])*relatedness[k]

    }
}

# Just plot pred 1, pred 2, pred 3
## Do like ribbon ggplot

med <- apply(pred1, 2, median)
HL <- apply(pred1, 2, HPDI)

p1_df <- data.frame(
    relatedness = relatedness,
    median = med,
    lower = HL[1,],
    upper= HL[2,],
    type = "Female to female ties"
)

med <- apply(pred2, 2, median)
HL <- apply(pred2, 2, HPDI)

p2_df <- data.frame(
    relatedness = relatedness,
    median = med,
    lower = HL[1,],
    upper = HL[2,],
    type = "Male to male ties"
)

med <- apply(pred3, 2, median)
HL <- apply(pred3, 2, HPDI)

p3_df <- data.frame(
    relatedness = relatedness,
    median = med,
    lower = HL[1,],
    upper= HL[2,],
    type = "Ties to different sex"
)

df <- rbind(p1_df, p2_df, p3_df)

rel_plot <- ggplot(data=df, aes(x=relatedness, y=median, ymin=lower, ymax=upper, fill=type, linetype=type)) +
 geom_line() +
 geom_ribbon(alpha=0.5) +
 scale_fill_manual(values=c("darkseagreen3", "deepskyblue4", "darkorchid4"), name="fill") +
  scale_x_continuous(breaks=c(0.0,0.0625, 0.125, 0.25, 0.5), limits=c(0, 0.5)) +
 xlab("Relatedness") +
 ylab("Sharing Ties") +
 theme_bw() +
 theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.text=element_text(size=20)) +
 guides(linetype = "none", fill=guide_legend(title=""))

ggsave(rel_plot, filename = "./2-analyses/outputs/relate_homophily_fig.pdf", device =  "pdf", width = 10, height = 10)


# Results table

results_table <- rbind(
    as.data.frame(res$summary_list$`False positive rate`),
    as.data.frame(res$summary_list$`Recall of true ties`),
    as.data.frame(res$summary_list$`Theta: question-order effects`),
    as.data.frame(res$summary_list$`Focal efffects: Out-degree`),
    as.data.frame(res$summary_list$`Target effects: In-degree`),
    as.data.frame(res$summary_list$`Dyadic effects`),
    as.data.frame(res$summary_list$`Other estimates`)
    )

results_table$type <- NA
results_table[1:4,]$type <- "False Positive Rate"
results_table[5:10,]$type <- "Recall of True Ties"
results_table[11:12,]$type <- "Question Duplication"
results_table[13:14,]$type <- "Sender Effects"
results_table[15:16,]$type <- "Receiver Effects"
results_table[17:27,]$type <- "Dyadic Effects"
results_table[28:32,]$type <- "Block Effects"

# Format names
results_table[1,]$Variable <- "False positive rate intercept (outgoing)"
results_table[2,]$Variable <- "False positive rate intercept (incoming)"
results_table[3,]$Variable <- "False positive rate SD (outgoing)"
results_table[4,]$Variable <- "False positive rate SD (incoming)"

results_table[5,]$Variable <- "Recall of true ties intercept (outgoing)"
results_table[6,]$Variable <- "Recall of true ties intercept (incoming)"
results_table[7,]$Variable <- "Recall of true ties SD (outgoing)"
results_table[8,]$Variable <- "Recall of true ties SD (incoming)"
results_table[9,]$Variable <- "Recall of true ties: Age effect (outgoing)"
results_table[10,]$Variable <- "Recall of true ties Age effects (incoming)"
results_table[11,]$Variable <- "Question duplication intercept"
results_table[12,]$Variable <- "Question duplication SD"

results_table[13,]$Variable <- "Sender effects SD"
results_table[14,]$Variable <- "Age sender effect"

results_table[15,]$Variable <- "Receiver effects SD"
results_table[16,]$Variable <- "Age receiver effect"

results_table[17,]$Variable <- "Dyadic effects SD"
results_table[18,]$Variable <- "Physical distance"
results_table[19,]$Variable <- "Genetic relatedness"
results_table[20,]$Variable <- "Foraging ties"
results_table[21,]$Variable <- "Gossip ties"
results_table[22,]$Variable <- "Same female"
results_table[23,]$Variable <- "Different sex"
results_table[24,]$Variable <- "Same female X Relatedness"
results_table[25,]$Variable <- "Different sex X Relatedness"
results_table[26,]$Variable <- "Generalized reciprocity"
results_table[27,]$Variable <- "Dyadic reciprocity"

results_table[28,]$Variable <- "Any to Any"
results_table[29,]$Variable <- "Low status to low status"
results_table[30,]$Variable <- "Low status to high status"
results_table[31,]$Variable <- "High status to low status"
results_table[32,]$Variable <- "High status to high status"

write.csv(results_table,"./2-analyses/outputs/results.csv")
colnames(results_table)[colnames(results_table)=="HPDI:0.05"] <- "lower"
colnames(results_table)[colnames(results_table)=="HPDI:0.95"] <- "upper"



colours <- c("goldenrod3", "mediumpurple4")

ind_fig <- ggplot(data=results_table[results_table$type %in% c("Sender Effects", "Receiver Effects"),],
 aes(x=Variable, y=as.numeric(Median),
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5),
        fill='darkorchid4', color = "darkorchid4") +
    geom_hline(yintercept= 0, lty=2) +
    coord_flip() +
    #scale_color_manual(values = c("#481567FF", "#287D8EFF", "#00cca5")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
   theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.text=element_text(size=20))

ind_fig

ggsave(ind_fig, filename = "./2-analyses/outputs/individual_figure.pdf", device =  "pdf", width = 14, height = 8)


dyad_fig <- ggplot(data=results_table[results_table$type == c("Dyadic Effects"),],
    aes(x=Variable, y=as.numeric(Median),
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5),
        fill='darkorchid4', color = "darkorchid4") +
    geom_hline(yintercept= 0, lty=2) +
    coord_flip() +
    #scale_color_manual(values = c("#481567FF", "#287D8EFF", "#00cca5")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
   theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.text=element_text(size=20))
dyad_fig

ggsave(dyad_fig, filename = "./2-analyses/outputs/dyadic_figure.pdf", device =  "pdf", width = 14, height = 8)


results_table[28,]$type <- "Intercept"

results_table$block_type <- NA
results_table[29:32,]$block_type <- "status"



status_samples <- res$samples$srm_model_samples$block_parameters[[2]]

for(i in 1:12000){
ss <- res$samples$srm_model_samples$block_parameters[[2]][i,,]
ss <- ss-ss[1,1]
status_samples[i, , ] <- ss
}

#res$samples$srm_model_samples$block_parameters[[2]] <- sex_samples

status_contrast <- select(results_table[30:32,],Variable, Median, lower, upper)

status_contrast$Median[status_contrast$Variable == "Low status to high status"] <- median(status_samples[, 1,2])
status_contrast$Median[status_contrast$Variable == "High status to low status"] <- median(status_samples[, 2,1])
status_contrast$Median[status_contrast$Variable == "High status to high status"] <- median(status_samples[, 2,2])

status_contrast$lower[status_contrast$Variable == "Low status to high status"] <- HPDI(status_samples[, 1,2])[1]
status_contrast$upper[status_contrast$Variable == "Low status to high status"] <- HPDI(status_samples[, 1,2])[2]
status_contrast$lower[status_contrast$Variable == "High status to low status"] <- HPDI(status_samples[, 2,1])[1]
status_contrast$upper[status_contrast$Variable == "High status to low status"] <- HPDI(status_samples[, 2,1])[2]
status_contrast$lower[status_contrast$Variable == "High status to high status"] <- HPDI(status_samples[, 2,2])[1]
status_contrast$upper[status_contrast$Variable == "High status to high status"] <- HPDI(status_samples[, 2,2])[2]

#appear <- c("Female to female",
#                    "Female to male",
#                    "Male to female",
#                    "Male to male",
#                    "Low status to low status",
#                    "Low status to high status",
#                    "High status to low status",
#                    "High status to high status")


status_block_fig <- ggplot(data=status_contrast,
    aes(x=Variable, y=as.numeric(Median),
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5),
        fill='deepskyblue4', color = "deepskyblue4") +
    geom_hline(yintercept= 0, lty=2) +
    coord_flip() +
    #scale_color_manual(values = "darkseagreen3") +
    xlab("") + ylab("Contrast (95% CI)") +
    theme_bw() +
   theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.text=element_text(size=20))
status_block_fig

ggsave(status_block_fig, filename = "./2-analyses/outputs/status_block_figure.pdf", device =  "pdf", width = 14, height = 8)
