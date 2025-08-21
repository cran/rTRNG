## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)
options(digits = 5) # for kable
linking_ok <- rTRNG::check_rTRNG_linking()

## ----code, include=FALSE, cache=FALSE-----------------------------------------
source("utils/read_chunk_wrap.R", echo = FALSE, print.eval = FALSE)
read_chunk_wrap("code/mcMat.R")
read_chunk_wrap("code/mcMat.cpp")
read_chunk_wrap("code/mcMatParallel.cpp")
if (linking_ok) {
  Rcpp::sourceCpp("code/mcMat.cpp", verbose = FALSE, embeddedR = FALSE)
  Rcpp::sourceCpp("code/mcMatParallel.cpp", verbose = FALSE, embeddedR = FALSE)
}

## -----------------------------------------------------------------------------
library(rTRNG)

## ----mcMatR-------------------------------------------------------------------
mcMatR <- function(nrow, ncol) {
  r <- yarn2$new(12358)
  M <- matrix(rnorm_trng(nrow * ncol, engine = r),
              nrow = nrow, ncol = ncol, byrow = TRUE)
  M
}

## ----mcSubMatR----------------------------------------------------------------
mcSubMatR <- function(nrow, ncol,
                      startRow, endRow, subCols) {
  r <- yarn2$new(12358)
  r$jump((startRow - 1)*ncol)
  nSubCols <- endRow - startRow + 1
  S <- matrix(0.0, nrow, ncol)
  S[startRow:endRow, subCols] <-
    vapply(subCols,
           function(j) {
             rj = r$copy()
             rj$split(ncol, j)
             rnorm_trng(nSubCols, engine = rj)
           },
           FUN.VALUE = numeric(nSubCols))
  S
}

## ----subMatExampleR-----------------------------------------------------------
rows <- 9
cols <- 5
M <- mcMatR(rows, cols)
startRow <- 4
endRow <- 6
subCols <- c(2, 4:5)
S <- mcSubMatR(rows, cols,
               startRow, endRow, subCols)
identical(M[startRow:endRow, subCols],
          S[startRow:endRow, subCols])

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(cbind.data.frame(M = M, S = S), row.names = TRUE)

## ----subMatExampleRcpp, eval=linking_ok---------------------------------------
rows <- 9
cols <- 5
startRow <- 4
endRow <- 6
subCols <- c(2, 4:5)
M <- mcMatRcpp(rows, cols)
S <- mcSubMatRcpp(rows, cols, startRow, endRow, subCols)
identical(M[startRow:endRow, subCols],
          S[startRow:endRow, subCols])

## ----echo=FALSE, eval=linking_ok----------------------------------------------
knitr::kable(cbind.data.frame(M = M, S = S), row.names = TRUE)

## ----fullMatExampleRcppParallel, eval=linking_ok------------------------------
M <- mcMatRcpp(rows, cols)
Mp <- mcMatRcppParallel(rows, cols, seq_len(ncol(M)))
identical(M, Mp)

## ----subMatExampleRcppParallel, eval=linking_ok-------------------------------
Sp <- mcMatRcppParallel(rows, cols, subCols)
identical(M[, subCols],
          Sp[, subCols])

## ----echo=FALSE, eval=linking_ok----------------------------------------------
knitr::kable(cbind.data.frame(M = M, Sp = Sp), row.names = TRUE)

