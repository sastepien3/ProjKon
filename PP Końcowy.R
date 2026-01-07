# app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#  1. Dane 
set.seed(123)

dane <- expand.grid(
  Miesiac = month.abb,
  Produkt = c("A", "B", "C")
)

# Większe różnice w sprzedaży
dane$Sprzedaz <- sample(10:100, nrow(dane), replace = TRUE)

#  2. UI 
ui <- fluidPage(
  titlePanel("Analiza sprzedaży produktów w ciągu roku"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "produkty", "Wybierz produkty:",
        choices = c("A", "B", "C"),
        selected = c("A", "B", "C")
      ),
      sliderInput(
        "miesiace", "Wybierz zakres miesięcy:",
        min = 1, max = 12, value = c(1,12), step = 1,
        ticks = FALSE
      )
    ),
    
    mainPanel(
      plotOutput("wykres_liniowy"),
      plotOutput("wykres_suma"),
      plotOutput("wykres_procent"),
      verbatimTextOutput("wyliczenia")
    )
  )
)

# 3. Server 
server <- function(input, output) {
  
  # Filtrowanie danych wg produktów i zakresu miesięcy
  dane_filtrowane <- reactive({
    req(input$produkty)
    dane %>%
      filter(Produkt %in% input$produkty,
             match(Miesiac, month.abb) >= input$miesiace[1],
             match(Miesiac, month.abb) <= input$miesiace[2])
  })
  
  # Wykres liniowy dla wybranych produktów
  output$wykres_liniowy <- renderPlot({
    ggplot(dane_filtrowane(), aes(x = factor(Miesiac, levels=month.abb), 
                                  y = Sprzedaz, color = Produkt, group = Produkt)) +
      geom_line(size=1.2) +
      geom_point(size=3) +
      theme_minimal() +
      labs(title = "Sprzedaż wybranych produktów w czasie",
           x = "Miesiąc", y = "Sprzedaż") +
      scale_color_brewer(palette = "Pastel1")
  })
  
  # Wykres sumaryczny wszystkich produktów
  output$wykres_suma <- renderPlot({
    dane %>%
      group_by(Produkt) %>%
      summarise(SumaSprzedazy = sum(Sprzedaz), .groups = "drop") %>%
      ggplot(aes(x = Produkt, y = SumaSprzedazy, fill = Produkt)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Łączna sprzedaż produktów w roku", y = "Sprzedaż") +
      scale_fill_brewer(palette = "Pastel2")
  })
  
  # Wykres procentowy udziału w sprzedaży
  output$wykres_procent <- renderPlot({
    dane_proc <- dane %>%
      group_by(Produkt) %>%
      summarise(SumaSprzedazy = sum(Sprzedaz), .groups = "drop") %>%
      mutate(Procent = SumaSprzedazy / sum(SumaSprzedazy) * 100,
             Label = paste0(round(Procent, 1), "%"))
    
    ggplot(dane_proc, aes(x = "", y = Procent, fill = Produkt)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      labs(title = "Procentowy udział sprzedaży produktów") +
      scale_fill_brewer(palette = "Pastel1")
  })
  
  #  proste wyliczenia w tekście
  output$wyliczenia <- renderPrint({
    df <- dane_filtrowane() %>%
      group_by(Produkt) %>%
      summarise(Suma = sum(Sprzedaz), .groups = "drop")
    df
  })
  
}

#  4. Uruchom aplikację 
shinyApp(ui = ui, server = server)
