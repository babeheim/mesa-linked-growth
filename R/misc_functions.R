
mask_text <- function(at, y, labels, srt = 0, ...) {
  mask <- paste(rep("█", nchar(labels)), collapse = "")
  text(at, y, labels = mask, col = "white", srt = srt)
  text(at, y, labels = labels, srt = srt, ...)
}

dir_init <- function(path, verbose=FALSE, overwrite=TRUE){
  if (path == "/") stop("what are you doing! you cannot format root")
  if (substr(path, 1, 1) == "/") stop("path argument cannot start from root")
  contents <- dir(path, recursive=TRUE)
  if(dir.exists(path)){
    if(overwrite){
      if(verbose){
        if(length(contents)==0) print(paste("folder ", path, " created.", sep=""))
        if(length(contents)>0) print(paste("folder ", path, " wiped of ", length(contents), " files/folders.", sep=""))
      }
      if(dir.exists(path)) unlink(path, recursive=TRUE)
      dir.create(path)
    }
  } else {
    if(verbose){
      print(paste("folder ", path, " created.", sep=""))
    }
    dir.create(path)
  }
}

sample_safe <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

assign_sets <- function(n, n_sets) {
  if (n < n_sets) stop("there must be more individuals than bins!")
  i <- 1:n
  n_per_set <- rep(floor(n/n_sets), each = n_sets)
  n_xtra <- n %% n_sets
  if (n_xtra > 0) {
    for (i in 1:n_xtra) {
      add_index <- sample(1:n_sets, 1, replace = TRUE)
      n_per_set[add_index] <- n_per_set[add_index] + 1
    }
  }
  set <- rep(1:n_sets, times = n_per_set)
  set <- sample_safe(set)
  return(set)
}

geom_mean <- function(x) exp(mean(log(x)))

prep_latex_variables <- function(named_list) {
  out <- character()
  for (i in 1:length(named_list)) {
    out[i] <- paste0("\\newcommand{\\", names(named_list)[i], "}{", named_list[[i]], "}")
  }
  return(out)
}

insert_row <- function(d, after_row, label) {
  add_row <- matrix("", ncol = ncol(d), nrow = 1)
  add_row[,1] <- label
  rbind(
    d[1:after_row,],
    add_row,
    d[(after_row + 1):nrow(d),]
  )
}

texttab <- function(input.matrix, hlines=NA) {
  output <- character(nrow(input.matrix))
  for (i in 1:nrow(input.matrix)) {
    add.amps <- paste(input.matrix[i,], collapse = " & ")
    output[i] <- paste(add.amps, "\\\\", sep = " ")
  }
  if (all(!is.na(hlines))) {
    for (i in seq_along(hlines)) output <- append(output, "\\hline", hlines[i] + (i - 1))
  }
  return(output)
}

col_alpha <- function (acol, alpha = 0.2) {
  acol <- col2rgb(acol)
  acol.red <- acol["red",] / 255
  acol.green <- acol["green",] / 255
  acol.blue <- acol["blue",] / 255
  acol <- mapply(
    function(red, green, blue, alphas) rgb(red, green, blue, alphas),
      acol.red, acol.green, acol.blue, alpha
  )
  return(as.character(acol))
}
