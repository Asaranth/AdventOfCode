dotenv::load_dot_env()

getInputData <- function(day) {
  cache_file <- file.path(getwd(), sprintf('2015/data/%02d.txt', day))

  if (file.exists(cache_file)) {
    return(readLines(cache_file, warn = FALSE))
  }

  url <- sprintf('https://adventofcode.com/2015/day/%d/input', day)
  session_cookie <- Sys.getenv('AOC_SESSION_COOKIE')

  if (session_cookie == '') {
    stop('AOC_SESSION_COOKIE not found in environment variables')
  }

  response <- httr::GET(url, httr::add_headers(Cookie = sprintf('session=%s', session_cookie)))

  if (httr::status_code(response) != 200) {
    stop('Failed to fetch data: ', httr::status_code(response))
  }

  data <- httr::content(response, 'text', encoding = 'UTF-8')

  data_lines <- unlist(strsplit(data, '\n'))
  data_lines <- data_lines[nzchar(data_lines) | c(TRUE, head(nzchar(data_lines), -1))]

  dir.create(file.path(getwd(), '2015/data'), showWarnings = FALSE, recursive = TRUE)
  writeLines(data_lines, cache_file)

  return(data_lines)
}