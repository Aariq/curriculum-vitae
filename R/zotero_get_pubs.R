library(httr2)
library(stringr)
library(textutils)

zotero_get_pubs <- function(type = c("journalArticle", "bookSection", "computerProgram")) {
  type <- match.arg(type, several.ok = TRUE)
  if (length(type) > 1) {
    type <- glue::glue_collapse(type, sep = " || ")
  }
  req <-
    request("https://api.zotero.org") |>
    req_url_query(
      v = 3,
      include = "bib",
      sort = "date",
      style = "journal-of-ecology",
      itemType = type
    ) |>
    req_headers(`Zotero-API-Key` = Sys.getenv("ZOTERO_API_KEY")) |>
    req_url_path("users", "6720834", "publications", "items") |>
    req_retry()
  
  resp <- 
    req |> 
    req_perform()
  
  resp |> 
    resp_body_json() |> 
    purrr::map("bib") |> 
    unlist() |>
    #convert from HTML
    HTMLdecode() |> 
    # Convert html to markdown
    str_remove_all("<div[^>]*>") |> 
    str_remove_all("<\\/div>") |> 
    #replace itallics
    str_replace_all("<\\/?i>", "*") |> 
    #remove line breaks???
    str_remove_all("\\n") |> 
    # Bold my name 
    str_replace("(Scott, E\\. R\\.)", '**\\1**') |> 
    cat(
      sep = 
"

\\medskip

")
  
}

 
