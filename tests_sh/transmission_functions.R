
# function to extract the transmissions and proportions 


cases_deets_function <- function(g_df) {
cases_deets <- SE_Tanz %>%
  dplyr::mutate(row_n = row_number()) %>%
  dplyr::select(Species, Time_diff, row_n) %>%
  mutate(row_n = as.character(row_n)) %>%
  mutate(Species = forcats::fct_recode(Species, "dom" = "Dog", "dom" = "Cat"))
# last line added to trim down to the 3 types of transmission

try <- left_join(x = g_df, y = cases_deets, by = c("from" = "row_n")) %>%
  rename("species_from" = "Species", "time_diff_from" = "Time_diff") %>%
  left_join(y = cases_deets, by= c("to" = "row_n"))%>%
  rename("species_to" = "Species", "time_diff_to" = "Time_diff")

# now want to arrange dependent on the time as 'from' and 'to' are at present quite arbitrary. 

try[,"first_species"] <- ifelse(try[,"time_diff_from"] < try[,"time_diff_to"],
                                try[,"species_from"], try[,"species_to"] ) 
try[,"second_species"] <- ifelse(try[,"time_diff_from"] > try[,"time_diff_to"],
                                 try[,"species_from"], try[,"species_to"] ) 


try[,"trans"] <- paste(try[,"first_species"], try[,"second_species"], sep = "")

# props_df <- as.data.frame(matrix(ncol = 2, nrow = 9))
# colnames(props_df) <- c("trans_type", "proportion")
# props_df[,"trans_type"] <- c("dog-dog",  "dog-wild", "dog-cat","wild-dog", "wild-wild",
#                              "wild-cat", "cat-dog", "cat-wild", "cat-cat")

props_df <- as.data.frame(matrix(ncol = 2, nrow = 3))
colnames(props_df) <- c("trans_type", "proportion")
props_df[,"trans_type"] <- c("dom-dom",  "mixed", "wild-wild")

props_df[which(props_df$trans_type == "dom-dom"), "proportion" ] <- 
  nrow(try[which(try$trans == "11"),])/nrow(try)
props_df[which(props_df$trans_type == "mixed"), "proportion" ] <- 
  nrow(try[which(try$trans == "12" | try$trans == "21"),])  /nrow(try)
props_df[which(props_df$trans_type == "wild-wild"), "proportion" ] <- 
  nrow(try[which(try$trans == "22"),])/nrow(try)

# 
# props_df[which(props_df$trans_type == "dog-dog"), "proportion" ] <- 
#   nrow(try[which(try$trans == "11"),])/nrow(try)
# props_df[which(props_df$trans_type == "dog-wild"), "proportion" ] <- 
#   nrow(try[which(try$trans == "12"),])/nrow(try)
# props_df[which(props_df$trans_type == "dog-cat"), "proportion" ] <- 
#   nrow(try[which(try$trans == "14"),])/nrow(try)
# 
# props_df[which(props_df$trans_type == "wild-dog"), "proportion" ] <- 
#   nrow(try[which(try$trans == "21"),])/nrow(try)
# props_df[which(props_df$trans_type == "wild-wild"), "proportion" ] <- 
#   nrow(try[which(try$trans == "22"),])/nrow(try)
# props_df[which(props_df$trans_type == "wild-cat"), "proportion" ] <- 
#   nrow(try[which(try$trans == "24"),])/nrow(try)
# 
# props_df[which(props_df$trans_type == "cat-dog"), "proportion" ] <- 
#   nrow(try[which(try$trans == "41"),])/nrow(try)
# props_df[which(props_df$trans_type == "cat-wild"), "proportion" ] <- 
#   nrow(try[which(try$trans == "42"),])/nrow(try)
# props_df[which(props_df$trans_type == "cat-cat"), "proportion" ] <- 
#   nrow(try[which(try$trans == "44"),])/nrow(try)

sum(props_df$proportion)

outlist <- list()
outlist[[1]] <- try
outlist[[2]] <- props_df
names(outlist) <- c("transmissions", "props_df")

return(outlist)

}


