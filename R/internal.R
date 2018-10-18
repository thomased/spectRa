#' Internal function for writing to the database
#'
#' @import devtools
#'
#' @export
#'
#' @keywords internal
#'
#' @return A data frame of reflectance spectra
#'
#' @examples \dontrun{
#' # Dichromat
#' data(flowers)
#' }
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
writespec <- function(){

  meta <- read.csv('inst/extdata/meta.csv', stringsAsFactors = FALSE)
  spectra <- read.csv('inst/extdata/spectra.csv', stringsAsFactors = FALSE)
  dictionary <- read.csv('inst/extdata/dictionary.csv', stringsAsFactors = FALSE)

  devtools::use_data(meta, spectra, dictionary, internal = TRUE, overwrite = TRUE)

  }

#' Internal function for rebuilding the database
#'
#' @import devtools
#' @importFrom utils read.csv
#' @export
#' @keywords internal
rebuildspec <- function(){

  meta <- read.csv('raw/meta_data.csv', stringsAsFactors = FALSE)
  spectra <- read.csv('raw/spectra.csv', stringsAsFactors = FALSE)
  dictionary <- read.csv('raw/dictionary.csv', stringsAsFactors = FALSE)

  devtools::use_data(meta, spectra, dictionary, internal = TRUE, overwrite = TRUE)

}

#' Summary metadata
#'
#' @param specs a data.frame of reflectance spectra, the result of \code{\link{searchspec}}.
#'  If no spectra are provided, a species-level summary of the entire database is returned.
#'
#' @import dplyr
#'
#' @return summary metadata
#'
#' @examples \dontrun{
#' # Summarise entire current database
#' summaryspec()
#'
#' # Summarise a searched subset
#' butterflies <- searchspec(order == "Lepidoptera")
#' summaryspec(butterflies)
#' }
#'
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
summaryspec <- function(specs){

  meta_int <- meta

  # Summarise entire database if no arguments provided, otherwise just the provided subset
  if(missing(specs)){
    meta_summ <- dplyr::summarise(dplyr::group_by(meta_int, phylum, class, order, family, genus, species), n = n())
  }else{
    wl_ind <- which(names(specs) == 'wl')  # Should always be 1 anyway
    metadat <- dplyr::filter(meta_int, spec_id == names(specs[, -wl_ind]))
    meta_summ <- dplyr::summarise(dplyr::group_by(metadat, order, family, genus, species), n = n())
  }

  meta_summ
}
