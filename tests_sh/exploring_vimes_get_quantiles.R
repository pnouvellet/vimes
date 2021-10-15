x0 <-seq(120,150, by = 0.0001)
x1 <- seq(120,150, by = 0.01)

si_mean <- 27.8175438596491
si_sd <- 26.8565433014125

si_cv <- si_sd/si_mean

gam_parms <- epitrix::gamma_mucv2shapescale(mu = si_mean, cv = si_cv)

f0 <- pgamma(q = x0, shape = gam_parms$shape, scale = gam_parms$scale)
f1 <- pgamma(q = x1, shape = gam_parms$shape, scale = gam_parms$scale)

plot(x0,f0, xlim = c(123.4,124), ylim = c(0.9899, 0.9901), type = "l")
lines(x1, f1, type = "p")

res <- c()
for(i in 1:100) {

 d_type = "temporal"
# d_type = "spatial"
 n = 1000000
 rrpi = 1
# si_log_mean = LN_mean
# si_log_sd = LN_SD
# q = quants
# dist_ray = rayleigh_scale
 params = c(si_mean, si_sd)



    all_ind <- data.frame(Case_no = seq(1, n, 1),
                            distance = c(0,rgamma(n-1, shape = gam_parms$shape, scale = gam_parms$scale)), # adding location of (0,0) for first dog
                            detected = runif(n, 0, 1) <= rrpi)
      
    
    # work out the cumulative SI
    all_ind$cum = cumsum(all_ind$distance)
    
    #Extract the case numbers of those observed
    Observed_ind <- which(all_ind$detected)
    
    observed_dist <- diff(all_ind$cum[Observed_ind])
    
    Output <- data.frame(Missing_gens = diff(Observed_ind), # more like k
                         observed_dist = observed_dist) # observed distances between observed animals
    
    
    threshold_sim <- quantile(Output$observed_dist, 0.99)
    lines(threshold_sim, 0.99, type = "p", col = "red")
#threshold_sim
    res <- c(res, threshold_sim) 
}  

quantile(res, c(0.5, 0.75, 0.25))

f <-which(f1>0.99)[1]
tq <- mean(x1[(f-1):f])


tq

quantile((res - tq)/res, c(0.5, 0.75, 0.25))*100

(res - tq)/res, 
