# Carrega as credenciais do Dropbox usadas para baixar os pontos de grade do MERRA-2.
# Ordem de busca:
#   1. Variaveis de ambiente DROPBOX_REFRESH_TOKEN, DROPBOX_CLIENT_ID e DROPBOX_CLIENT_SECRET
#      (definidas em um arquivo .Renviron na raiz do projeto; ver .Renviron.example);
#   2. Arquivo legado config_token.R, que define refresh_token, client_id e client_secret.
# Sem credenciais, o app sobe normalmente: apenas a geracao de series a partir do MERRA-2
# fica indisponivel (o modo de upload de series proprias continua funcionando).
load_dropbox_credentials <- function(legacy_file = "config_token.R") {
  creds <- list(
    refresh       = Sys.getenv("DROPBOX_REFRESH_TOKEN"),
    client_id     = Sys.getenv("DROPBOX_CLIENT_ID"),
    client_secret = Sys.getenv("DROPBOX_CLIENT_SECRET")
  )

  if (all(nzchar(unlist(creds)))) return(creds)

  if (file.exists(legacy_file)) {
    env <- new.env()
    sys.source(legacy_file, envir = env)
    return(list(
      refresh       = get0("refresh_token", envir = env, ifnotfound = ""),
      client_id     = get0("client_id", envir = env, ifnotfound = ""),
      client_secret = get0("client_secret", envir = env, ifnotfound = "")
    ))
  }

  warning(
    "Credenciais do Dropbox nao encontradas (.Renviron ou config_token.R). ",
    "A geracao de series a partir do MERRA-2 ficara indisponivel."
  )
  creds
}

# Função auxiliar: pega um novo access token usando o refresh_token
refresh_dropbox_token <- function(refresh_token, client_id, client_secret) {
  if (!nzchar(refresh_token) || !nzchar(client_id) || !nzchar(client_secret)) {
    stop("Credenciais do Dropbox ausentes: configure o arquivo .Renviron (ver README).")
  }
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