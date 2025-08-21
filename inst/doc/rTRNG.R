## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE)
linking_ok <- rTRNG::check_rTRNG_linking()

## ----TRNG.Random--------------------------------------------------------------
library(rTRNG)
TRNGkind("yarn2")
TRNGseed(117)
TRNGjump(5) # advance by 5 the internal state
TRNGsplit(3, 2) # subsequence: one element every 3 starting from the 2nd

## ----runif_trng---------------------------------------------------------------
x <- runif_trng(10)
x

## ----TRNG.Engine--------------------------------------------------------------
rng <- yarn2$new()
rng$seed(117)
# alternative: rng <- yarn2$new(117) 
rng$jump(5) 
rng$split(3, 2) 

## ----runif_trng-engine--------------------------------------------------------
x <- runif_trng(10, engine = rng)
x

## ----ex-parallelGrain---------------------------------------------------------
TRNGseed(117)
RcppParallel::setThreadOptions(numThreads = 2)
x_parallel <- runif_trng(1e5, parallelGrain = 100)
TRNGseed(117)
x_serial <- runif_trng(1e5)
identical(x_serial, x_parallel)

## ----sourceCpp-R, echo=FALSE, eval=linking_ok---------------------------------
exampleCpp()

## ----check-linking, eval=FALSE------------------------------------------------
# rTRNG::check_rTRNG_linking()

