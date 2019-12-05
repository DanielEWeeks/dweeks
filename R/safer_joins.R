#' left_join augmented with checks
#'
#' This augments \code{left_join} from the \code{dplyr} package with important quality control checks.
#' When \code{by} is not NULL, then it checks for duplicated IDs (e.g., join variables).
#' When \code{by} is NULL, if there is only one column in common between \code{x} and \code{y}, then it checks for duplicated IDs using that single column as the join variable.
#' Also checks if the resulting join has more records than the input table \code{x}.
#'
#'
#'
#' @param x table to left join into.
#' @param y table to be joined into x.
#' @param by a character vector of variables to join by.
#' @param copy If x and y are not from the same data source, and copy is TRUE, then y will be copied into the same src as x.
#' @param suffix If there are non-joined duplicate variables in x and y, these suffixes will be added to the output to disambiguate them. Should be a character vector of length 2.
#'
#' @examples
#' x <- data.frame(ID=c(1,2,3),ID1=c(1,2,3),x=c(1,1,1))
#' y <- data.frame(ID=c(1,2,2),ID2=c(1,2,2),y=c(2,2,2))
#' x
#' y
#' left_join_check(x,y)
#' left_join_check(x,y,by="ID")
#' left_join_check(x,y,by = c("ID" = "ID"))
#' left_join_check(x,y,by = c("ID1" = "ID2"))
left_join_check <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    name.x <- deparse(substitute(x))
    name.y <- deparse(substitute(y))
    if (!is.null(by)) {
      if (is.null(names(by))) {
        # 'by' is an unamed vector
        ndupIDs.x = sum(duplicated(x[,by]))
        if (ndupIDs.x != 0) {
          warning(paste0("Duplicated IDs in ",name.x," in the ",by, " column. "))
        }
        ndupIDs.y = sum(duplicated(y[,by]))
        if (ndupIDs.y != 0) {
          warning(paste0("Duplicated IDs in ",name.y," in the ",by, " column. "))
        }
      } else {
        # 'by' is a named vector
        ndupIDs.x = sum(duplicated(x[,names(by)]))
        if (ndupIDs.x != 0) {
          warning(paste0("Duplicated IDs in ",name.x," in the ",names(by), " column. "))
        }
        ndupIDs.y = sum(duplicated(y[,by]))
        if (ndupIDs.y != 0) {
          warning(paste0("Duplicated IDs in ",name.y," in the ",by, " column. "))
        }
      }
    } else {
      by1 <- intersect(names(x),names(y))
      if (length(by1) == 1) {
        # If there is only one field in common, we can do these checks:
        ndupIDs.x = sum(duplicated(x[,by1]))
        if (ndupIDs.x != 0) {
          warning(paste0("Duplicated IDs in ",name.x," in the ",by1, " column. "))
        }
        ndupIDs.y = sum(duplicated(y[,by1]))
        if (ndupIDs.y != 0) {
          warning(paste0("Duplicated IDs in ",name.y," in the ",by1, " column. "))
        }

      }
    }
    d.x <- nrow(x)
    r <- left_join(x, y, by, copy, suffix)
    d.r <- nrow(r)
    if (d.x != d.r) {
      warning(paste0("nrow(",name.x,")=",d.x," != nrow(join)=",d.r,". "))
    }
    return(r)
}
