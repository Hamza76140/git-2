install.packages("shiny")

library(shiny)

# =====================================
# 1. CREATION DE DONNEES FICTIVES
# =====================================

set.seed(123)
n <- 100

donnees <- data.frame(
  ACN = runif(n, 20, 80),
  Temperature = runif(n, 25, 45)
)

# Réponse quantitative : temps de rétention
donnees$Retention <- 14 -
  0.12 * donnees$ACN -
  0.05 * (donnees$Temperature - 35) +
  rnorm(n, 0, 0.5)

# Réponse binaire : conforme (1) ou non (0)
probabilite <- plogis(
  -4.5 + 0.07 * donnees$ACN +
    0.02 * (donnees$Temperature - 35)
)

donnees$Conforme <- rbinom(n, 1, probabilite)

# =====================================
# 2. INTERFACE DE L'APPLICATION
# =====================================

ui <- fluidPage(
  
  titlePanel("Application GLM - Chimie analytique"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Paramètres du modèle"),
      
      selectInput(
        "modele",
        "Choisir le modèle :",
        choices = c(
          "Modèle linéaire général (LM)" = "LM",
          "GLM - Régression logistique" = "GLM"
        )
      ),
      
      checkboxInput(
        "temperature",
        "Inclure la température dans le modèle",
        value = TRUE
      ),
      
      sliderInput(
        "acn",
        "Pourcentage d'ACN (%) :",
        min = 20,
        max = 80,
        value = 50
      ),
      
      sliderInput(
        "temp",
        "Température (°C) :",
        min = 25,
        max = 45,
        value = 35
      ),
      
      hr(),
      
      h4("Prédiction"),
      verbatimTextOutput("prediction")
      
    ),
    
    mainPanel(
      
      h3("Résultats de l'analyse"),
      
      verbatimTextOutput("formule"),
      
      tabsetPanel(
        
        tabPanel(
          "Graphique",
          plotOutput("graphique"),
          p("La courbe est calculée à température constante : 35 °C.")
        ),
        
        tabPanel(
          "Coefficients et p-values",
          tableOutput("coefficients")
        ),
        
        tabPanel(
          "Interprétation",
          verbatimTextOutput("interpretation")
        ),
        
        tabPanel(
          "Données",
          tableOutput("tableau")
        )
        
      )
    )
  )
)

# =====================================
# 3. CALCULS STATISTIQUES
# =====================================

server <- function(input, output, session) {
  
  # Construction du modèle
  modele <- reactive({
    
    if (input$modele == "LM") {
      
      formule <- if (input$temperature) {
        Retention ~ ACN + Temperature
      } else {
        Retention ~ ACN
      }
      
      lm(formule, data = donnees)
      
    } else {
      
      formule <- if (input$temperature) {
        Conforme ~ ACN + Temperature
      } else {
        Conforme ~ ACN
      }
      
      glm(
        formule,
        data = donnees,
        family = binomial(link = "logit")
      )
    }
  })
  
  # Affichage de l'équation
  output$formule <- renderText({
    paste("Modèle :", deparse(formula(modele())))
  })
  
  # Tableau des coefficients
  output$coefficients <- renderTable({
    
    resultat <- summary(modele())$coefficients
    
    data.frame(
      Variable = rownames(resultat),
      Coefficient = round(resultat[, 1], 4),
      Erreur_type = round(resultat[, 2], 4),
      Statistique = round(resultat[, 3], 3),
      p_value = format.pval(resultat[, 4], digits = 3)
    )
    
  })
  
  # Prédiction à partir des sliders
  output$prediction <- renderText({
    
    nouvelles_donnees <- data.frame(
      ACN = input$acn,
      Temperature = input$temp
    )
    
    valeur <- predict(
      modele(),
      newdata = nouvelles_donnees,
      type = "response"
    )
    
    if (input$modele == "LM") {
      
      paste(
        "Temps de rétention prédit :",
        round(valeur, 2),
        "minutes"
      )
      
    } else {
      
      paste(
        "Probabilité de conformité :",
        round(valeur * 100, 1),
        "%"
      )
    }
  })
  
  # Graphique
  output$graphique <- renderPlot({
    
    acn_seq <- seq(20, 80, length.out = 200)
    
    nouvelles_donnees <- data.frame(
      ACN = acn_seq,
      Temperature = rep(35, 200)
    )
    
    valeurs_predites <- predict(
      modele(),
      newdata = nouvelles_donnees,
      type = "response"
    )
    
    if (input$modele == "LM") {
      
      plot(
        donnees$ACN,
        donnees$Retention,
        pch = 19,
        col = "steelblue",
        xlab = "ACN (%)",
        ylab = "Temps de rétention (min)",
        main = "Régression linéaire"
      )
      
      lines(
        acn_seq,
        valeurs_predites,
        col = "red",
        lwd = 3
      )
      
    } else {
      
      plot(
        donnees$ACN,
        jitter(donnees$Conforme, amount = 0.05),
        pch = 19,
        col = "steelblue",
        xlab = "ACN (%)",
        ylab = "Conformité / Probabilité",
        ylim = c(-0.1, 1.1),
        main = "Régression logistique"
      )
      
      lines(
        acn_seq,
        valeurs_predites,
        col = "red",
        lwd = 3
      )
    }
  })
  
  # Interprétation statistique
  output$interpretation <- renderText({
    
    coefficients <- summary(modele())$coefficients
    
    p_values <- coefficients[-1, 4]
    
    significatifs <- names(p_values)[p_values < 0.05]
    
    conclusion <- if (length(significatifs) > 0) {
      
      paste(
        "Facteur(s) significatif(s) à 5 % :",
        paste(significatifs, collapse = ", ")
      )
      
    } else {
      
      "Aucun facteur significatif au seuil de 5 %."
    }
    
    if (input$modele == "LM") {
      
      paste(
        "R² =", round(summary(modele())$r.squared, 3),
        "\n",
        conclusion
      )
      
    } else {
      
      paste(
        "AIC =", round(AIC(modele()), 2),
        "\n",
        conclusion,
        "\nLe GLM estime la probabilité de conformité."
      )
    }
  })
  
  # Tableau des données
  output$tableau <- renderTable({
    head(donnees, 15)
  })
  
}

# =====================================
# 4. LANCEMENT DE L'APPLICATION
# =====================================

shinyApp(ui = ui, server = server)

