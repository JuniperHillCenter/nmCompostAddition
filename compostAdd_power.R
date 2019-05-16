caPower <- function(ntreat, nrep, blocked, mainEffects, blockEffectSD, errorSD) {
    # browser()
    dat <- data.frame(rep = rep(1:nrep, each = ntreat), t = paste0('t', 0:(ntreat - 1)))
    
    if(blocked) {
        dat$b <- paste0('b', dat$rep)
        mm <- model.matrix(~ 0 + b + t, dat)
        p <- c(rnorm(nrep, 0, blockEffectSD), mainEffects[-1])
    } else {
        mm <- model.matrix(~ t)
        p <- mainEffects
    }
    
    dat$y <- mm %*% p + rnorm(nrow(dat), 0, errorSD)
    
    mod <- lme4::lmer(y ~ t + (1 | b), dat)
}

caPower(4, 6)