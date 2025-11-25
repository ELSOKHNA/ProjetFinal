# fonction prop_test_context

prop_test_context <- function(x, n, p0 = 0.5, conf_level = 0.95) {
  
  # 1. appel à prop.test()
  
  res <- prop.test(
    x = x,
    n = n,
    p = p0,
    conf.level = conf_level,
    correct = FALSE
  )
  
  # 2. calculer la proportion estimée
  prop_estimee <- x / n
  
  # 3. construire l'objet de sortie
  out <- list(
    x           = x,
    n           = n,
    p0          = p0,
    conf_level  = conf_level,
    prop_estimee = prop_estimee,
    p_value      = res$p.value,
    ic_inf       = res$conf.int[1],
    ic_sup       = res$conf.int[2]
  )
  
  # 4. lui donner une classe S3
  
  class(out) <- "context_table"
  
  # 5. retourner l'objet
  return(out)
}


library(ggplot2)

# Méthode plot.context_table

plot.context_table <- function(obj, ...) {
  # Vérifier la classe
  stopifnot(inherits(obj, "context_table"))
  
  # Construire un tableau 2x2 : Observé vs sous H0
  tab <- rbind(
    Observé = c(Succès = obj$x,
                Échec  = obj$n - obj$x),
    "Sous H0" = c(Succès = obj$n * obj$p0,
                  Échec  = obj$n * (1 - obj$p0))
  )
  
  # Tracer la mosaïque
  graphics::mosaicplot(
    tab,
    main = "Test de proportion",
    xlab = "",
    ylab = "Fréquence relative",
    color = TRUE,
    ...
  )
  
  invisible(obj)
}

obj <- prop_test_context(12, 20, p0 = 0.5)

plot(obj)   # appelle plot.context_table(obj)

# Méthode print de prop_test_context  :

print.context_table  <- function(y,...){
  # y est l'objet de classe "context_table"
  
  cat("Test de proportion : \n")
  cat("********************\n")
  cat("Le nombre d'essais est de:  ", y$n,"\n")
  cat("La probabilité du succès est: ",y$p0*100,"\n")
  cat("La proportion de succès est de: ",y$prop_estimee*100,"%",
      "avec une p_value de: ",y$p_value,"à", y$conf_level*100,"%\n" )
  }

## creer une fonction lm_contex() qui permettra 
## de faire des régressions linéaires

lm_context <- function(formula, data) {
  
  ## Ajustement du modele
  model <- lm(formula = formula, data = data)
  sm <- summary(model)
  
  ## Liste des objets que retournera la foonction
  out <- list(
    call = model$call,
    variables = terms(model), 
    residus_model = summary(residuals(model)),
    coefficients = coefficients(model),
    sigma = sm$sigma,
    r2 = sm$r.squared,
    adj_r2 = sm$adj.r.squared,
    statfisher = sm$fstatistic,
    df = model$df.residual
  )
  
  
  ## Specification de la classe de l'objet retourne
  class(out) <- "lm_context"
  
  ## Sortie
  return(out)
}

## Ajouter  une methode S3 en l occurence summary à notre fonction

summary.lm_context <- function(objet, ...){
  
  sommaire <- list(
    call = objet$call,
    variables = objet$variables, 
    residus_model = objet$residual_model,
    coefficients = objet$coefficients,
    sigma = objet$sigma,
    r2 = objet$r.squared,
    adj_r2 = objet$adj.r.squared,
    statfisher = objet$fstatistic,
    df = objet$df
  )
  
  
  ## definition de la classe de summary.lm_context
  
  class(sommaire) <- "summary.lm_context"
  
  return(sommaire)
}

data("iris")
data(mtcars)
test1 <- lm_context(mpg ~ hp + wt, data = mtcars)
summary.lm_context(m)
test2 <- lm_context(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species) 

