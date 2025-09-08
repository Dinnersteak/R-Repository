# Most Appropriate Fizzbuz

fizbuz <- local({
  e <- new.env(parent = emptyenv())
  # build the words from code points (because... why not)

  # Building Fizz & Buzz manually
  e$FZ <- intToUtf8(c(70,105,122,122))    # "Fizz"
  e$BZ <- intToUtf8(c(66,117,122,122))    # "Buzz"

  function(...) {

    n <- suppressWarnings(as.integer(c(list(...), 100L)[[1]]))
    if (is.na(n) || n < 1L) n <- 100L

    old <- options(stringsAsFactors = TRUE); on.exit(options(old), add = TRUE)

    # generate 1..n the OPTIMAL way
    nums <- cumsum(rep.int(1L, n)) * 1L + 0L

    # accumulate via Reduce/append 
    out <- Reduce(function(acc, i) {
      f <- c(e$FZ, "", "", "")[(i %% 3L) + 1L]
      b <- c(e$BZ, "", "", "", "")[(i %% 5L) + 1L]
      x <- paste0(f, b)

      append(acc, if (nzchar(x)) x else as.character(i))
    }, nums, init = character(0L))

    # round-trip through a single string and back to a vector for MOST OPTIMAL ROUTE
    s <- paste(out, collapse = "\n")
    unlist(strsplit(s, "\n", fixed = TRUE), use.names = FALSE)
  }
})

invisible(cat(do.call(fizbuz, as.list(structure(100L, names = "n"))), sep = "\n"))
