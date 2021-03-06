% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LdFlags.R
\name{LdFlags}
\alias{LdFlags}
\title{Linker flags for rTRNG.}
\usage{
LdFlags()
}
\value{
Returns NULL invisibly. The function is not called for its return
  value rather for the side effect of outputting the flags.
}
\description{
Output the linker flags required to build against \pkg{rTRNG}.
}
\details{
\code{LdFlags} is typically called from Makevars as
\preformatted{PKG_LIBS += $(shell ${R_HOME}/bin/Rscript -e "rTRNG::LdFlags()")}
and from Makevars.win as
\preformatted{PKG_LIBS += $(shell "${R_HOME}/bin${R_ARCH_BIN}/Rscript.exe" -e "rTRNG::LdFlags()")}
}
