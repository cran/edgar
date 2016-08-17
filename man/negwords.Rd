\name{negwords}
\alias{negwords}
\docType{data}
\title{ Dictionary of negative words}
\description{
  Dataset contains negative words for sentiment analysis of
  EDGAR filings suggested by Loughran and McDonald's
  financial sentiment dictionaries.
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
