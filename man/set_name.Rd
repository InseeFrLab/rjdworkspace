% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manip_sa_item.R
\name{set_name}
\alias{set_name}
\title{Set the name of a SaItem}
\usage{
set_name(sa_item, name)
}
\arguments{
\item{sa_item}{a \code{"sa_item"} object.}

\item{name}{the new name.}
}
\value{
a new \code{"sa_item"} with the new name.
}
\description{
Function to set the name of a \code{"sa_item"}.
}
\examples{
sa_x13 <- jx13(ipi_c_eu[, "FR"])

wk <- new_workspace()
mp1 <- new_multiprocessing(wk, "sa1")
add_sa_item(wk, "sa1", sa_x13, "tramo seats")
sa1 <- get_object(mp1, 1)

new_sa <- set_name(sa1, "X13")
replace_sa_item(mp1, 1, new_sa)
# The first sa_item of mp1 is now "X13"
RJDemetra::get_name(get_object(mp1, 1))

}