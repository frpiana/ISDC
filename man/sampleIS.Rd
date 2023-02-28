\name{sampleIS}
\alias{sampleIS}
\title{Class for Impedance Spectrocopy sample object generation}
\usage{
objectName <- new('sampleIS',
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
The initialization requires a TXT file containing impedance data, the name and the geometry of the specimen.
}
\examples{
sample <- new("sampleIS",
            code = "pol_01",
            dataSource = "C://project/data/sample01.txt",
            diameter = 5,
            thickness = 0.1)

view(sample$dielectric)
}
