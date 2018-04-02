## actual voting share
actual.outcomes <- read.csv("data/History_Voting.csv")
state.abb.dc <- append(state.abb, "DC", after=8)
actual.outcomes <- actual.outcomes[match(state.abb.dc, actual.outcomes$ST), ]
row_names <- actual.outcomes$ST
actual.outcomes <- actual.outcomes[, grep("D\\.R\\.", colnames(actual.outcomes))]
rownames(actual.outcomes) <- row_names
vote.share.1996 <- actual.outcomes[,5]
names(vote.share.1996) <- row_names
vote.share.2000 <- actual.outcomes[,4]
names(vote.share.2000) <- row_names
vote.share.2004 <- actual.outcomes[,3]
names(vote.share.2004) <- row_names
vote.share.2008 <- actual.outcomes[,2]
names(vote.share.2008) <- row_names
vote.share.2012 <- actual.outcomes[,1]
names(vote.share.2012) <- row_names
national.vote.share <- c(51.1/(51.1 + 47.2), 52.9/(52.9 + 45.7), 48.3/(50.7+48.3), 48.4/(47.9+48.4), 49.2/(49.2 + 40.7))
names(national.vote.share) <- c("2012", "2008", "2004", "2000", "1996")
