# cleanCode()
# Change default prompt and output line prefix in R
# Allows simply copy commands and output from R and paste them into another R session
# Idea original: https://stackoverflow.com/a/1451246/16424070

cleanCode <- function() {
  if (.Platform$OS.type == "unix" && .Platform$pkgType == "mac.binary") {
    to_edit <- readLines(pipe("pbpaste")) # Mac ONLY solution
  } else {
    to_edit <- readLines("clipboard") # Windows/Unix solution
  }
  opts <- options()
  cmdPrompts <- paste("^", opts$prompt, "|^", opts$continue, sep="")

  # can someone help me here? how to escape the + to \\+, as well as other special chars

  id_commands <- grep("^> |^\\+ ", to_edit) # which are command or continuation lines
  to_edit[id_commands] <- sub("^> |^\\+ ", "", to_edit[id_commands]) # remove prompts
  to_edit[-id_commands] <- paste("  # ", to_edit[-id_commands]) # comment output
  writeLines(to_edit)
}