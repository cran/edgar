\name{negwords}
\alias{negwords}
\docType{data}
\title{ Negative Words for Textual Analysis of EDGAR Fillings}
\description{
  Dataset contains negative words for Textual Analysis of
  EDGAR fillings suggested by Loughran and McDonald 
  Financial Sentiment Dictionaries.
}
\format{
  The format is:
  $ WORDS: Factor w/ 2329 levels "abandon","abandoned",..: 1 2 ...
}
\source{
Loughran and McDonald Financial Sentiment Dictionaries
\url{http://www3.nd.edu/~mcdonald/Word_Lists.html}
}
\examples{
\dontrun{

neg.words<-read.csv(system.file("data/negwords.csv", package = "edgar"))
str(neg.words)
}
}
\keyword{datasets}
