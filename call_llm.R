call_llm <- function(prompt, model = "mistral", max_tokens = 500) {
  
  # Vérifier que httr et jsonlite sont disponibles
  
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Le package 'httr' est requis. Installez-le avec : install.packages('httr')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Le package 'jsonlite' est requis. Installez-le avec : install.packages('jsonlite')")
  }
  
  library(httr)
  library(jsonlite)
  
  # URL de l'API Ollama (local)
  url <- "http://localhost:11434/api/generate"
  
  # Préparer la requête
  body_list <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    options = list(num_predict = max_tokens)
  )
  
  # Envoyer la requête POST
  
  response <- tryCatch({
    POST(
      url,
      body = toJSON(body_list, auto_unbox = TRUE),
      content_type_json(),
      timeout(60)
    )
  }, error = function(e) {
    stop("Impossible de se connecter à Ollama. Vérifiez qu'Ollama est lancé et accessible sur localhost:11434")
  })
  
  # Vérifier le statut de la réponse
  
  if (status_code(response) != 200) {
    error_msg <- content(response, as = "text", encoding = "UTF-8")
    stop(sprintf(
      "Erreur Ollama (code %d). Le modèle '%s' est-il installé ? Vérifiez avec : ollama list\nDétails : %s",
      status_code(response), model, error_msg
    ))
  }
  
  # Extraire la réponse
  
  content_response <- content(response, as = "parsed", encoding = "UTF-8")
  
  if (is.null(content_response$response)) {
    stop("Réponse vide du LLM. Vérifiez la configuration d'Ollama.")
  }
  
  return(content_response$response)
}