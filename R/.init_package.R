
package <- "kwb.event"

kwb.orcid::get_kwb_orcids()

author <- list(
  name = "Hauke Sonnenberg",
  orcid = "0000-0001-9134-2871",
  url = "https://github.com/hsonne"
)

description <- list(
  name = package,
  title = "Generate Events from Time Series and work with Events",
  desc  = paste(
    "Functions to generate events from time series and work with events."
  )
)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.3.0",
  stage = "maturing"
)



pkg_dependencies <- c("kwb.utils", "kwb.datetime", "kwb.plot")

sapply(pkg_dependencies, usethis::use_package)

sapply(sprintf("github::kwb-r/%s", pkg_dependencies), desc::desc_add_remotes)

kwb.pkgbuild::use_autopkgdown()

desc::desc_normalize()
