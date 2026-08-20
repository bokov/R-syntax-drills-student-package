read_question_manifest <- function(path = "question_manifest.csv") {
  if (!file.exists(path)) {
    return(data.frame(
      item_label = character(),
      event = character(),
      topic = character(),
      points = numeric(),
      starter_question = logical(),
      question_hash = character(),
      stringsAsFactors = FALSE
    ))
  }

  read.csv(path, stringsAsFactors = FALSE, na.strings = "")
}

question_manifest_bank_version <- function(manifest) {
  drillr:::drillr_manifest_bank_version(manifest)
}

question_hash_for_item <- function(item_label, manifest, default = "") {
  if (is.null(item_label) || is.na(item_label) || !nzchar(item_label)) {
    return(default)
  }
  exact <- which(manifest$item_label == item_label)
  if (length(exact) == 1) return(as.character(manifest$question_hash[[exact]]))
  default
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
