\name{poswords}
\alias{poswords}
\docType{data}
\title{ Dictionary of positive words}
\description{
  Dataset contains positive words for sentiment analysis of
  EDGAR filings suggested by Loughran and McDonald's 
  financial sentiment dictionaries.
}
\format{
  The format is:
 $ WORDS: Factor w/ 2005 levels "abound","abounds",..: 1 2 ...
}
\source{
Loughran and McDonald Financial Sentiment Dictionaries
\url{http://www3.nd.edu/~mcdonald/Word_Lists.html}
}
\examples{
\dontrun{
pos.words<-read.csv(system.file("data/poswords.csv", package = "edgar"))
str(pos.words)
}
}
\keyword{datasets}
