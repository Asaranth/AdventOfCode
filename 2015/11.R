data <- readLines('./2015/data/11.txt')[1]
banned_letters <- c('i', 'o', 'l')

skip_invalid_characters <- function(pw) {
  pw <- strsplit(pw, '')[[1]]
  for (i in seq_along(pw)) {
    if (pw[i] %in% banned_letters) {
      pw[i] <- intToUtf8(utf8ToInt(pw[i]) + 1)
      for (j in (i + 1):length(pw)) {
        pw[j] <- 'a'
      }
      break
    }
  }
  paste(pw, collapse = "")
}

increment_password <- function(password) {
  pw <- strsplit(password, '')[[1]]
  for (i in rev(seq_along(pw))) {
    if (pw[i] == 'z') {
      pw[i] <- 'a'
    } else {
      pw[i] <- intToUtf8(utf8ToInt(pw[i]) + 1)
      break
    }
  }
  paste(pw, collapse = '')
}

contains_increasing_straight <- function(pw) {
  for (i in 1:(nchar(pw) - 2)) {
    if (utf8ToInt(substr(pw, i, i)) + 1 == utf8ToInt(substr(pw, i + 1, i + 1)) && utf8ToInt(substr(pw, i, i)) + 2 == utf8ToInt(substr(pw, i + 2, i + 2))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

contains_banned_letters <- function(pw) {
  return(any(charToRaw(pw) %in% charToRaw(paste(banned_letters, collapse = ''))))
}

contains_two_pairs <- function(pw) {
  pairs <- gregexpr('(.)\\1', pw)[[1]]
  if (pairs[1] == -1) return(FALSE)

  num_pairs <- 0
  last_pos <- -2

  for (pos in pairs) {
    if (pos != last_pos + 1) num_pairs <- num_pairs + 1
    last_pos <- pos
  }

  return(num_pairs >= 2)
}

is_valid_password <- function(pw) {
  contains_increasing_straight(pw) && !contains_banned_letters(pw) && contains_two_pairs(pw)
}

solvePartOne <- function() {
  pw <- skip_invalid_characters(data[1])
  repeat {
    pw <- increment_password(pw)
    if (is_valid_password(pw)) {
      return(pw)
    }
  }
}

solvePartTwo <- function(pw) {
  repeat {
    pw <- increment_password(pw)
    if (is_valid_password(pw)) {
      return(pw)
    }
  }
}

partOneResult <- solvePartOne()
cat(stringr::str_interp('Part One: ${partOneResult}\n'))
cat(stringr::str_interp('Part Two: ${solvePartTwo(partOneResult)}\n'))