q_vect <- c(rep(q, n_runs))

all_trans_vect <- s1_vect <- s1s2_vect <- #s2s1_vect 
  s2_vect <- vector(length = n_runs)

out_vect <- vector(mode = "list", length = n_runs)

out_df <-as.data.frame(matrix(ncol = 5, nrow = 4))
row.names(out_df) <- c("all_trans", "s1_trans", "s1s2_trans", #"s2s1_trans", 
                       "s2_trans")
colnames(out_df) <- c("med", "range_min", "IQR_25", "IQR_75", "range_max")

tictoc::tic()
for (i in 1:n_runs) {
  
  out_vect[[i]] <-  get_quantiles_multi(d_type = "spatial", 
                                        s1_obs = s1_obs, s2_obs = s2_obs,
                                        s1_rr = s1_rr,
                                        s2_rr = s2_rr,
                                        params_s1s1 = params_s1s1_sp, params_s1s2 = params_s1s2_sp,
                                        #params_s2s1 = params_s2s1, 
                                        params_s2s2 = params_s2s2_sp,
                                        n = n, q = q_vect[i])
  
  all_trans_vect[i]<- out_vect[[i]][["threshold_sim"]]
  s1_vect[i] <- out_vect[[i]][["threshold_s1s1"]]
  s1s2_vect[i] <- out_vect[[i]][["threshold_s1s2"]]
  #s2s1_vect[i] <- out_vect[[i]][["threshold_s2s1"]]
  s2_vect[i] <- out_vect[[i]][["threshold_s2s2"]]
  
  
}

out_df["all_trans","med"] <- median(all_trans_vect)
out_df["all_trans","range_min"] <- range(all_trans_vect)[1]
out_df["all_trans", "IQR_25"] <- quantile(all_trans_vect, 0.25)
out_df["all_trans", "IQR_75"] <- quantile(all_trans_vect,0.75)
out_df["all_trans", "range_max"] <- range(all_trans_vect)[2]

out_df["s1_trans","med"] <- median(s1_vect)
out_df["s1_trans","range_min"] <- range(s1_vect)[1]
out_df["s1_trans", "IQR_25"] <- quantile(s1_vect, 0.25)
out_df["s1_trans", "IQR_75"] <- quantile(s1_vect,0.75)
out_df["s1_trans", "range_max"] <- range(s1_vect)[2]

out_df["s1s2_trans","med"] <- median(s1s2_vect)
out_df["s1s2_trans","range_min"] <- range(s1s2_vect)[1]
out_df["s1s2_trans", "IQR_25"] <- quantile(s1s2_vect, 0.25)
out_df["s1s2_trans", "IQR_75"] <- quantile(s1s2_vect,0.75)
out_df["s1s2_trans", "range_max"] <- range(s1s2_vect)[2]

# out_df["s2s1_trans","med"] <- median(s2s1_vect)
# out_df["s2s1_trans","range_min"] <- range(s2s1_vect)[1]
# out_df["s2s1_trans", "IQR_25"] <- quantile(s2s1_vect, 0.25)
# out_df["s2s1_trans", "IQR_75"] <- quantile(s2s1_vect,0.75)
# out_df["s2s1_trans", "range_max"] <- range(s2s1_vect)[2]

out_df["s2_trans","med"] <- median(s2_vect)
out_df["s2_trans","range_min"] <- range(s2_vect)[1]
out_df["s2_trans", "IQR_25"] <- quantile(s2_vect, 0.25)
out_df["s2_trans", "IQR_75"] <- quantile(s2_vect,0.75)
out_df["s2_trans", "range_max"] <- range(s2_vect)[2]

out_df[,"lower_%diff_from_med"] <- 100-((out_df[,"range_min"]/out_df[,"med"])*100)
out_df[,"lower_quant_from_med"] <- 100-((out_df[,"IQR_25"]/out_df[,"med"])*100)
out_df[,"upper_quant_from_med"] <- ((out_df[,"IQR_75"]/out_df[,"med"])*100)-100
out_df[,"upper_%diff_from_med"] <- ((out_df[,"range_max"]/out_df[,"med"])*100)-100

