library(data.table)
library(reshape)
library(scales)
require(Hmisc)
library(lme4)
library(blme)
library(arm)
library(foreign)
library(stringr)

# set number of digits to display
options(digits=3)

# capitalize first letter of every word in string
capitalize.all <- Vectorize(function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
})

# categorize states by "contestedness"
battleground <- c('CO','FL','IA','NH','OH','VA')
quasi.battleground <- c('MI','MN','NC','NV','NM','PA','WI')
solid.obama <- c('CA','CT','DC','DE','HI','IL','ME','MD','MA','NJ','NY','OR','RI','VT','WA')
solid.romney <- c('AL','AK','AZ','AR','GA','ID','IN','KS','KY','LA','MS','MO','MT','NE','ND','OK','SC','SD','TN','TX','UT','WV','WY')
contestedness <- function(state) {
	result <- rep(NA, length(state))
	result[state %in% battleground] <- 'battleground'
	result[state %in% quasi.battleground] <- 'quasi-battleground'
	result[state %in% solid.obama] <- 'solid obama'
	result[state %in% solid.romney] <- 'solid romney'
	result
}

# replace values
replace.values <- function(x, old, new) {
	indices <- match(x,old)
	ndx <- !is.na(indices)
	x[ndx] <- new[indices[ndx]]
	x
}

inv.logit <- function(x) {
	val <- exp(x) / (1 + exp(x))
	val
}

# generate predictions for an lmer model
# NOTE: only works for varying-intercept models
predict.lmer <- function(fitted.model, new.data) {
	# cast new.data to a data.frame
	new.data <- as.data.frame(new.data)

	# add the fixed effect
	fitted.values <- rep(as.numeric(fixef(fitted.model)), nrow(new.data))

	# loop through the random effects and add up the coeficients
	for ( category in names(ranef(fitted.model)) ) {
		# generate the predictor names, to deal with interactions
		effect.components <- strsplit(category,':', fixed=T)[[1]]
		variable.names <- as.character(new.data[,effect.components[1]])
		if ( length(effect.components) >= 2 ) {
			for ( ndx in 2:length(effect.components) ) {
				variable.names <- paste(variable.names, new.data[,effect.components[ndx]], sep=':')
			}
		}

		# pull out the appropriate coeficients from the fitted model
		coefs <- ranef(fitted.model)[[category]]
		ndx <- match(variable.names, rownames(coefs))
		betas <- coefs[ndx, '(Intercept)']
		betas[is.na(betas)] <- 0

		# add the betas to the estimate
		fitted.values <- fitted.values + betas
	}

	fitted.values
}

# generate predictions for an glmer model
# NOTE: only works for varying-intercept logit models
predict.glmer <- function(fitted.model, new.data) {
	fitted.values <- inv.logit( predict.lmer(fitted.model, new.data) )
	fitted.values
}

# estimate the posterior predictive distirbution for an lmer model
# NOTE: only works for varying-intercept models
sim.predict.lmer <- function(fitted.model, new.data, num.sims=100) {
    ## cast new.data to a data.frame
    new.data <- as.data.frame(new.data)
    ## generate posterior distribution for coeficients
    sims <- arm::sim(fitted.model, num.sims)
    ## add up fixed effects
    fitted.values <- matrix(fixef(sims)[,"(Intercept)"], nrow=nrow(new.data), ncol=num.sims, byrow=T)
    for(fe in colnames(fixef(sims))) {
        if(fe != "(Intercept)" ) {
            alphas <- as.matrix(new.data[, fe]) %*% t(fixef(sims)[, fe])
            alphas[is.na(alphas)] <- 0
            fitted.values <- fitted.values + alphas
        }
    }
    ## loop through the random effects (w/o date) and add up the coeficients
    for (category in names(ranef(sims)) ) {
        if(category!="finish_date") {
            ## generate the predictor names, to deal with interactions
            effect.components <- strsplit(category,':', fixed=T)[[1]]
            variable.names <- as.character(new.data[,effect.components[1]])
            if ( length(effect.components) >= 2 ) {
                for ( ndx in 2:length(effect.components) ) {
                    variable.names <- paste(variable.names, new.data[,effect.components[ndx]], sep=':')
                }
            }
            ##  if ( length(effect.components)==1) {                           ## pull out the appropriate coeficients from the fitted model
            coefs <- ranef(sims)[[category]][,,'(Intercept)']
            ndx <- match(variable.names, colnames(coefs))
            betas <- t(coefs[,ndx])
            betas[is.na(betas)] <- 0
            rownames(betas) <- NULL
            ## add the betas to the estimate
            fitted.values <- fitted.values + betas
        }
                                        #}
    }
    fitted.values
}
# estimate the posterior predictive distirbution for a glmer model
# NOTE: only works for varying-intercept logit models
sim.predict.glmer <- function(fitted.model, new.data, num.sims=100) {
    fitted.values <- inv.logit(sim.predict.lmer(fitted.model, new.data, num.sims))
    fitted.values
}

poststratified.ci.by.state <- function(major.party.model, obama.model, cells1, cells2, num.sims=100) {
    cells1 <- data.frame(cells1)
    major.party = sim.predict.glmer(major.party.model, cells1, num.sims)
    obama.given.major = sim.predict.glmer(obama.model, cells1, num.sims)
    other = 1 - major.party
    obama = major.party * obama.given.major
    romney = major.party * (1-obama.given.major)

    mrp.estimates <- data.frame()

    weights <- cells1$weight / sum(cells1$weight)
    estimates <- data.frame(
				obama = t(obama) %*% weights,
				romney = t(romney) %*% weights,
				other = t(other) %*% weights
        )
    estimates <- transform(estimates,
                           obama.two.party = obama/(obama + romney)
                           )

    mrp.estimates <- with(estimates, {
        samples <- data.frame(rbind(obama, romney, other, obama.two.party))
        colnames(samples) <- paste("sample", 1:num.sims, sep=".")
        rownames(samples) <- NULL
        rbind(mrp.estimates, cbind(
            data.frame(
                outcome = c('obama','romney','other','obama.two.party'),
                p = c( mean(obama), mean(romney), mean(other), mean(obama.two.party) ),
                std.dev = c( sd(obama), sd(romney), sd(other), sd(obama.two.party) ),
                lower = c( quantile(obama, 0.025), quantile(romney, 0.025), quantile(other, 0.025), quantile(obama.two.party, 0.025) ),
                upper = c( quantile(obama, 0.975), quantile(romney, 0.975), quantile(other, 0.975), quantile(obama.two.party, 0.975) ),
                state = "US",
                category = "national"
                ),
            samples
            ))
    })

    cells2 <- data.frame(cells2)

    major.party = sim.predict.glmer(major.party.model, cells2, num.sims)
    obama.given.major = sim.predict.glmer(obama.model, cells2, num.sims)
    other = 1 - major.party
    obama = major.party * obama.given.major
    romney = major.party * (1-obama.given.major)

    for(s in unique(cells2$state)) {
        ndx <- cells2$state==s
        cells2.ndx <- subset(cells2, ndx)
        weights <- cells2.ndx$weight / sum(cells2.ndx$weight)
        estimates <- data.frame(
            obama = t(subset(obama, ndx)) %*% weights,
            romney = t(subset(romney, ndx)) %*% weights,
            other = t(subset(other, ndx)) %*% weights
            )
        estimates <- transform(estimates,
                               obama.two.party = obama/(obama + romney)
                               )
        mrp.estimates <- with(estimates, {
            samples <- data.frame(rbind(obama, romney, other, obama.two.party))
            rownames(samples) <- NULL
            colnames(samples) <- paste("sample", 1:num.sims, sep=".")
            rbind(mrp.estimates, cbind(
                data.frame(
                    outcome = c('obama','romney','other','obama.two.party'),
                    p = c( mean(obama), mean(romney), mean(other), mean(obama.two.party) ),
                    std.dev = c( sd(obama), sd(romney), sd(other), sd(obama.two.party) ),
                    lower = c( quantile(obama, 0.025), quantile(romney, 0.025), quantile(other, 0.025), quantile(obama.two.party, 0.025) ),
                    upper = c( quantile(obama, 0.975), quantile(romney, 0.975), quantile(other, 0.975), quantile(obama.two.party, 0.975) ),
                    state = s,
                    category = "state"
                    ),
                samples
                ))
        })
    }
    mrp.estimates
}
