caPower <- function(ntreat, nrep, blocked, mainEffect, blockEffectSD, errorSD) {
    dat <- data.frame(rep = rep(1:nrep, each = ntreat), t = paste0('t', 0:(ntreat - 1)))
    
    mainEffects <- 0:(ntreat - 1) * mainEffect
    
    
    dat$b <- if(blocked) {dat$rep} else {1:nrow(dat)}
    dat$b <- paste0('b', dat$b)
    # browser()
    
    mm <- model.matrix(~ 0 + b + t, dat)
    p <- c(rnorm(length(unique(dat$b)), 0, blockEffectSD), mainEffects[-1])
    
    dat$y <- mm %*% p + rnorm(nrow(dat), 0, errorSD)
    
    if(blocked) {
        mod1 <- logLik(lme4::lmer(y ~ t + (1 | b), dat))
        mod0 <- logLik(lme4::lmer(y ~ 1 | b, dat))
    } else {
        mod1 <- logLik(lm(y ~ t, dat))
        mod0 <- logLik(lm(y ~ 1, dat))
    }
    
    pval <- pchisq(-2 * (mod0 - mod1), df = attr(mod1, 'df') - attr(mod0, 'df'), 
                   lower.tail = FALSE)
    
    return(as.numeric(pval))
}

caPower(5, 6, blocked = FALSE, mainEffect = 0.5, blockEffectSD = 0.5, errorSD = 0.5)

