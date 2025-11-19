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
plot.context_table <- function(x, ...) {
  # x est l'objet de classe "context_table"
  
  df <- data.frame(
    label   = "proportion",
    estimate = x$prop_estimee,
    ic_inf   = x$ic_inf,
    ic_sup   = x$ic_sup
  )
  
  ggplot2::ggplot(df, ggplot2::aes(x = label, y = estimate)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ic_inf, ymax = ic_sup),
      width = 0.1
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      x = NULL,
      y = "Proportion",
      title = "Résultat du test de proportion"
    )
}

obj <- prop_test_context(12, 20, p0 = 0.5)

plot(obj)   # appelle plot.context_table(obj)