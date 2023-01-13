#'
sql_read <- function(x) {
  if(file.exists(system.file("sql", x, package = "swo"))) {
    readLines(system.file("sql", x, package = "swo"))
  } else {
    stop("The sql file does not exist.")
  }
}

collapse_filters <- function(x) {
  sprintf("'%s'", paste(x, collapse = "','"))
}

sql_add <- function(x, sql_code, flag = "-- insert table") {

  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- x
  sql_code
}
sql_filter <- function(sql_precode = "=", x, sql_code, flag = "-- insert species") {

  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(x), ")"
  )
  sql_code
}

sql_run <- function(database, query) {
  query = paste(query, collapse = "\n")
  DBI::dbGetQuery(database, query, as.is=TRUE, believeNRows=FALSE)
}


core_samp <- function(data, samp, grp = c('year', 'species_code', 'stratum', 'hauljoin'), replace = FALSE) {
  data %>%
    .[,.SD[base::sample.int(.N, min(samp,.N), replace = replace)], by = grp]
}
