\name{poswords}
\alias{poswords}
\docType{data}
\title{ Positive Words for Textual Analysis of EDGAR Filings}
\description{
  Dataset contains positive words for Textual Analysis of
  EDGAR Filings suggested by Loughran and McDonald 
  Financial Sentiment Dictionaries.
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
