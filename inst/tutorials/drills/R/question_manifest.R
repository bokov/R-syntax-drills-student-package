read_question_manifest <- function(path = "question_manifest.csv") {
  if (!file.exists(path)) {
    return(data.frame(
      item_label = character(),
      event = character(),
      topic = character(),
      points = numeric(),
      starter_question = logical(),
      release = integer(),
      stringsAsFactors = FALSE
    ))
  }

  read.csv(path, stringsAsFactors = FALSE, na.strings = "")
}

scored_manifest_labels <- function(manifest) {
  if (is.null(manifest) || !nrow(manifest)) return(character())
  unique(as.character(manifest$item_label[
    manifest$event == "exercise_result" & manifest$points > 0
  ]))
}

question_topic <- function(item_label, manifest, default = "unassigned") {
  if (is.null(item_label) || is.na(item_label) || !nzchar(item_label)) {
    return(default)
  }
  if (!nrow(manifest)) return(default)

  exact <- which(manifest$item_label == item_label)
  if (length(exact) == 1) return(manifest$topic[[exact]])

  parent <- which(vapply(
    manifest$item_label,
    function(label) {
      any(vapply(
        c("-", "_", "."),
        function(separator) startsWith(item_label, paste0(label, separator)),
        logical(1)
      ))
    },
    logical(1)
  ))
  if (length(parent) == 1) return(manifest$topic[[parent]])

  default
}
