####################################################################
## to replicate, please install lme4<1.0, blme<1.0 and arm<1.6-07 ##
####################################################################
## Load pre-defined convenience functions
source("./library.R") 

## simulated vote-intent table the day before election 2012-11-05 (4-day smoothed)
smoothed.intent.table <- data.table(read.csv("data/simulated_last_day_intent_table_smoothed.csv"))

## population data against which to poststratify
load('data/exit_2008.RData') # raw 2008 exit poll data
source("./actual_vote.R") # load actual vote share by state in 2000, 2004, 2008 and 2012

## prepare 2008 national exit polls for post-stratification
pop.data.2008.national <- data.table(pop.data.2008.national)
pop.data.2008.national$vote.2008 <- pop.data.2008.national$vote
pop.composition.national <- pop.data.2008.national[complete.cases(pop.data.2008.national), list(weight=sum(weight)),
                                                   by=c('vote.2008', 'sex','age','race','education','state', 'ideology','party.id')]
pop.composition.national <- pop.composition.national[,p:=weight/sum(weight)]
pop.composition.national$state.last.vote.share <- vote.share.2008[match(pop.composition.national$state, names(vote.share.2008))]
pop.composition.national$state.contestedness <- factor(contestedness(pop.composition.national$state))

pop.data.2008.state <- data.table(pop.data.2008.state)
pop.data.2008.state$vote.2008 <- pop.data.2008.state$vote
pop.composition.state <- pop.data.2008.state[complete.cases(pop.data.2008.state), list(weight=sum(weight)),
                                             by=c('vote.2008', 'sex', 'age','race','education','state','ideology','party.id')]
pop.composition.state <- pop.composition.state[order(pop.composition.state$state),]
pop.composition.state$p <- with(pop.composition.state, unlist(tapply(weight, state, function(x) x/sum(x))))
pop.composition.state$state.last.vote.share <- vote.share.2008[match(pop.composition.state$state, names(vote.share.2008))]
pop.composition.state$state.contestedness <- factor(contestedness(pop.composition.state$state))

################################################################
## Multi-level models part, we just use bglmer from blme package.

major.party.model <- blme::bglmer(
    formula = cbind(obama + romney, other) ~ (1|sex) + (1|age) + (1|race) + (1|education)
    + (1|state) + (1|ideology) + (1|party.id) + (1|vote.2008) + state.last.vote.share,
    data=smoothed.intent.table,
    family=binomial()
    )

obama.model <- blme::bglmer(
    formula = cbind(obama, romney) ~ (1|sex) + (1|age) + (1|race) + (1|education)
    + (1|state) + (1|ideology) + (1|party.id)  + (1|vote.2008) + state.last.vote.share,
    data=smoothed.intent.table[obama+romney > 0],
    family=binomial()
    )

## poststratify and create posterior samples
## the function poststratified.ci.by.state is defined in library.R
estimates <- poststratified.ci.by.state(major.party.model, obama.model, pop.composition.national, pop.composition.state, num.sim=100)

head(estimates)[, 1:20] ## there are 208 rows, 4 outcomes times 52 races (national + 51 electoral colleges)
