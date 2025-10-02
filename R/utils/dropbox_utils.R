# Função auxiliar: pega um novo access token usando o refresh_token
refresh_dropbox_token <- function(refresh_token, client_id, client_secret) {
  url <- "https://api.dropboxapi.com/oauth2/token"
  
  res <- httr::POST(
    url,
    httr::authenticate(client_id, client_secret),
    body = list(
      grant_type = "refresh_token",
      refresh_token = refresh_token
    ),
    encode = "form"
  )
  
  if (httr::status_code(res) == 200) {
    return(httr::content(res)$access_token)
  } else {
    stop("Erro ao renovar token do Dropbox: ", httr::content(res, "text"))
  }
}

# Função principal: baixa e lê um arquivo .feather do Dropbox
read_feather_dropbox_httr <- function(dropbox_path, refresh_token, client_id, client_secret) {
  url <- "https://content.dropboxapi.com/2/files/download"
  
  # Sempre começa pedindo um novo access_token válido
  access_token <- refresh_dropbox_token(refresh_token, client_id, client_secret)
  
  res <- httr::POST(
    url,
    httr::add_headers(
      "Authorization" = paste("Bearer", access_token),
      "Dropbox-API-Arg" = paste0('{"path":"', dropbox_path, '"}')
    ),
    encode = "raw"
  )
  
  if (httr::status_code(res) == 200) {
    temp_file <- tempfile(fileext = ".feather")
    on.exit(unlink(temp_file)) # Garante que o arquivo temporário seja deletado
    writeBin(httr::content(res, "raw"), temp_file)
    df <- arrow::read_feather(temp_file)
    return(df)
  } else {
    stop("Erro ao baixar arquivo do Dropbox: ", httr::content(res, "text"))
  }
}