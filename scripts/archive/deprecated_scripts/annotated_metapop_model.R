run_metapop <- function(n_patches, pop, beta, coupling, timesteps, births, mortalityrate) {
    Susc <- matrix(S.init, nrow = 1)
    Infect <- matrix(I.init, nrow = 1)
    
    for (i in 2:timesteps) {
        # browser()
        Susc.tmp <- numeric(0) #at each time step, create a placeholder for the current state of the population
        Infect.tmp <- numeric(0)
        for (j in 1:Npatches) {
            tmp <- ceiling(24 + 24 * (i / 24 - ceiling(i / 24)))
            beta.i <- runif(1, beta[tmp, j] * .75, beta[tmp, j] * 1.25) #generate a random beta based on the beta value and centered between 0.75a and 1.25*b of the uniform distribution
            # browser()
            I.ext <- sum(Infect[i - 1, -j]) #' Number of infecteds in the other patches. 
            migr <- ifelse(I.ext > 0,  min(10, rpois(1, coupling[j, -j] * I.ext)), 0)  #' If infecteds exist in the other patches, then generate some random number of migrations to the current patch
            migr <- ifelse(runif(1) > .9,  migr, 0)
            # browser()
            out.tmp <- SIRstep(Susc[i - 1, j], Infect[i - 1, j], beta.i, births[j], mortalityrate[j], migration = migr)
            Susc.tmp <- c(Susc.tmp, out.tmp$S)
            Infect.tmp <- c(Infect.tmp, out.tmp$I)
            cat(i, "-", j, ".\n")
        }
        Susc <- rbind(Susc, Susc.tmp)
        Infect <- rbind(Infect, Infect.tmp)
        # browser()
        if (sum(Infect.tmp) == 0) {
            break
        }
    }
    return(list(S = Susc, I = Infect))
}
