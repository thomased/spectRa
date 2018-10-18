#' Search the spectral reflectance database
#'
#' Search the database
#'
#' @import dplyr
#' @importFrom pavo as.rspec
#'
#' @param ... Search conditions to match (note: must use relational operators, e.g.
#'  \code{==}, \code{<=}). Can match against any variables listed in the \code{\link{dictionary}},
#'  namely:
#'  \itemize{
#'   \item \code{phylum}:
#'   \item \code{class}:
#'   \item \code{order}:
#'   \item \code{family}:
#'   \item \code{genus}:
#'   \item \code{species}:
#'   \item \code{sub}:
#'   \item \code{patch}:
#'   \item \code{sex}:
#'   \item \code{country}:
#'   \item \code{latitude}:
#'   \item \code{longitude}:
#'   \item \code{study_name}:
#'   \item \code{contact_name}:
#'   \item \code{contact_email}:
#'   \item \code{doi}:
#'  }
#' @param aggregate aggregate resulting spectra? Defaults to FALSE.
#' @param by single categorical variable to aggregate by if aggregate = TRUE
#'
#' @return A data frame of reflectance spectra from binned in 1 nm intervals
#'  from 300-700 nm, with the first column containing wavelengths ('wl').
#'
#' @export
#'
#' @examples \dontrun{
#'
#' butterflies <- searchspec(order == 'Lepidoptera')
#'
#' female_butterflies <- searchspec(order == "Lepidoptera", sex == 'f')
#'
#' }
#'
#' @author Thomas White \email{thomas.white@@sydney.edu.au}

searchspec <- function(..., aggregate = FALSE, by = NULL){

  meta_int <- meta
  spectra_int <- spectra

  # Get selected id's
  id <- dplyr::filter(meta_int, ...)

  # Get spectra
  specs <- subset(spectra_int, select = id$spec_id)

  # Add wavelength column if need be
  if(names(specs)[1] != 'wl'){
    specs <- cbind(spectra_int$wl, specs)
    names(specs)[1] = 'wl'
  }

  specs <- as.rspec(specs)
  specs

}

