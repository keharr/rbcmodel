#' Compute the mean of a vector if the vector is numerical or logical.
#' Otherwise, return the common value if all entries take the same value,
#' or NA in any other cases.
#'
#' @param x the vector to compute average from.
#' @param na.rm whether NA's are ignored for the purpose of computing average
#'   or for checking consistency between entries.
#' @returns The average or common value, if exists.
mean_if_num <- function(x, na.rm = TRUE) {

  # treat the NA cases once and for all for all branches
  if (na.rm) {
    x <- stats::na.omit(x)
  }

  if (length(x) == 0) {
    # no entries: just return NA
    return(NA)
  } else if (!any(is.na(x)) && all(x == x[[1]])) {
    # take the consistent value if all entries agree
    return(x[[1]])
  } else if (all(is.na(x))) {
    # special case when all entries are NA
    return(NA)
  } else if (is.numeric(x) || is.logical(x)) {
    # compute average if numeric
    return(mean(x))
  } else {
    # otherwise return NA
    return(NA)
  }
}

#' Compute the median of a vector if the vector is numerical or logical.
#' Otherwise, return the common value if all entries take the same value,
#' or NA in any other cases.
#'
#' @param x the vector to compute average from.
#' @param na.rm whether NA's are ignored for the purpose of computing average
#'   or for checking consistency between entries.
#' @returns The average or common value, if exists.
median_if_num <- function(x, na.rm = TRUE) {

  # treat the NA cases once and for all for all branches
  if (na.rm) {
    x <- stats::na.omit(x)
  }

  if (length(x) == 0) {
    # no entries: just return NA
    return(NA)
  } else if (!any(is.na(x)) && all(x == x[[1]])) {
    # take the consistent value if all entries agree
    return(x[[1]])
  } else if (all(is.na(x))) {
    # special case when all entries are NA
    return(NA)
  } else if (is.numeric(x) || is.logical(x)) {
    # compute average if numeric
    return(stats::median(x))
  } else {
    # otherwise return NA
    return(NA)
  }
}


#' Return the last non-NA value from a vector.
#'
#' @param x the vector to extract values from.
#' @returns The last non-NA value, or NA if none exists.
last_non_NA <- function(x) {

  # remove NA's
  y <- stats::na.omit(x)

  if (length(y) == 0){
    return(NA) # return NA if no entries
  } else {
    return(y[length(y)]) # return last entry
  }
}

#' Merge (combine) several entries from a data.frame.
#'
#' @param data The data.frame from which entries are pulled.
#' @param id_col The column from which the relevant entries are identified.
#' @param id_vals The values from id_col for which the corresponding entries
#'   will be pulled.
#' @param method The method to combine entries. Can be "average", "median", or "last".
#' @param new_id The new identifier (for id_col) for the merged entry.
#' @param const_cols The columns to check for consistency. Error is thrown
#'   if the values from the pulled entries are not identical for any such
#'   columns.
#' @param keep_merged Whether to keep the original entries that are pulled
#'   and merged.
#' @param keep_unmerged Whether to keep the original entries that have NOT
#'   been touched.
#' @export
#' @examples
#' df <- data.frame(
#'   id = 1:4,
#'   x = c(1, 2, NA, 3),
#'   y = c("this", "this", NA, "that")
#' )
#'
#' df2 <- merge_entries(df, "id", c(1,4), "average", new_id = "new")
#' df2
#'
#' df3 <- merge_entries(df, "id", c(4, 3, 1), "last", keep_merged = FALSE)
#' df3
#'
#' try(merge_entries(df, "id", c(4, 3, 1), "last", const_cols="y")) # expects error
merge_entries <- function(
  data, id_col, id_vals, method,
  new_id = NA, const_cols = NULL,
  keep_merged = TRUE, keep_unmerged = TRUE
) {

  # subset data by identifier
  subdata <- data[data[[id_col]] %in% id_vals, ]

  # check if values from const_cols are consistent
  chk_results <- logical(0)
  for (col in const_cols) {
    if (any(is.na(subdata[[col]]))) {

      # special case when at least one value is NA
      if (all(is.na(subdata[[col]]))) {
        chk_results <- c(chk_results, TRUE)
      } else {
        chk_results <- c(chk_results, FALSE)
      }

    } else {

      # case when no values are NA
      chk_val <- subdata[[1, col]]
      chk_results <- c(chk_results, all(subdata[[col]] == chk_val))
    }
  }

  if (!all(chk_results)) {
    stop("Inconsistent columns found. No merging performed.")
  }

  # perform merge to get new entry
  if (method == "average") { # take average
    new_row <- lapply(
      subdata, mean_if_num, na.rm = TRUE
    )
  } else if (method == "median") { # take median
    new_row <- lapply(
      subdata, median_if_num, na.rm = TRUE
    )
  } else if (method == "last") { # take last non-NA entry
    subsubdata <- subdata[match(id_vals,subdata[[id_col]]), ] # first, order as prescribed
    new_row <- lapply(
      subsubdata, last_non_NA
    )
  } else {
    stop("Unknown method. No merging performed.")
  }
  new_row[id_col] <- new_id

  new_row <- as.data.frame(new_row)

  # bind pre-merged entries to the new dataframe
  if (keep_merged){
    new_row <- rbind(subdata, new_row)
  }

  # bind untouched entries to the new dataframe
  if (keep_unmerged){
    data <- data[!(data[[id_col]] %in% id_vals), ]
    new_row <- rbind(data, new_row)
  }

  new_row # return result
}

#' Cite data collected in the package.
#'
#' @param identifier The identifier of the data. Takes a single identifier or a
#' list of identifiers.
#' @param type The type of citation desired. Can take "full", which will return
#' the full citation, or "short", which returns the pmid or doi, if available.
#' @returns For data from a single study, this returns the citation for that
#'   study. For data averaged or compiled by this study, returns a note to
#'   cite this package.
#' @examples
#' cite_Rbc("average_1B_all")
#' cite_Rbc(c("japonica_Sage_2002b","japonica_orr_2016_dH","average_1B_C3_warm_dH"))
#' cite_Rbc("breve_banda_2020",type="short")
cite_Rbc <- function(identifier, type="full") {
  if (!type %in% c("short","full")) {
    stop("type must be \"short\" or \"full\".")
  }

  citation <- vector(length=length(identifier))

  for (x in 1:length(identifier)) {
    i <- identifier[x]
    if (grepl("dH",i)) {
      if (grepl("average",i)) {
        ref <- temp_dep_averaged[temp_dep_averaged[["identifier"]]==i,][["short_ref"]]
      } else {
        ref <- temp_dep_abridged[temp_dep_abridged[["identifier"]]==i,][["short_ref"]]
      }
    } else if (grepl("average",i)) {
      ref <- Rubisco_abridged[Rubisco_abridged[["identifier"]]==i,][["short_ref"]]
    } else {
      ref <- Rubisco_25C[Rubisco_25C[["identifier"]]==i,][["short_ref"]]
    }

    if (identical(ref,character(0))) {
      stop(paste("No identifier matching",i,"was found."))
    }

    if (type=="short") {
      citation[x] <- refs_to_citation[refs_to_citation[["short_ref"]]==ref,][["pmid_or_doi"]]
    } else {
      citation[x] <- refs_to_citation[refs_to_citation[["short_ref"]]==ref,][["citation"]]
    }
  }
  print(citation)
}
