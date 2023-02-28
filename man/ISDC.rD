\name{sample_EIS}
\alias{sample_EIS}
\title{Class for Impedance Spectrocopy sample object generation}
\usage{
objectName <- new('sample_EIS',
              code = "...",
              dataSource = "...",
              diameter = ...,
              thickness = ...)
}
\arguments{
\item{x}{A number.}
\item{y}{A number.}
}
\value{

}
\description{

}
\examples{
sample <- new('sample_EIS',
            code = "pol_01",
            dataSource = "C://project/data/sample01.txt",
            diameter = 5,
            thickness = 0.1)

view(sample$dielectric)
}
