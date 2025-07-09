library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(leaflet)    
library(stringr)
library(sf)


data_emisi  <- read_excel("C:/Users/Aulia/Documents/kuliah/KOMSTAT/tugas akhir/data/Emisi Karbon indonesia (2015-2024).xlsx") 
data_kebkar <- read_excel("C:/Users/Aulia/Documents/kuliah/KOMSTAT/tugas akhir/data/Luas kebakaran hutan dan lahan indonesia (2015-2024).xlsx")
data_suhu   <- read_excel("C:/Users/Aulia/Documents/kuliah/KOMSTAT/tugas akhir/data/Suhu rata rata indonesia (2015-2024).xlsx")
View(data_emisi)
indo_sf <- st_read("C:/Users/Aulia/Documents/kuliah/KOMSTAT/tugas akhir/data/Administrasi_Provinsi.shp") |>
  st_transform(4326) |>                     
  mutate(Provinsi = str_trim(Provinsi))
library(ggplot2)
ggplot(indo_sf) + geom_sf(fill = "#69b3a2", color = "white") + theme_void()
data_long_emisi <- data_emisi  |>
  pivot_longer('2015':'2024', names_to = "tahun", values_to = "emisi_karbon") |>
  mutate(tahun = as.integer(tahun), Provinsi = factor(Provinsi))

data_long_kebkar <- data_kebkar |>
  pivot_longer('2015':'2024', names_to = "tahun", values_to = "luas_karhutla") |>
  mutate(tahun = as.integer(tahun), Provinsi = factor(Provinsi))
yr <- 2017

map_data <- data_long_kebkar %>%
  filter(tahun == yr) %>%
  group_by(Provinsi) %>%
  summarise(luas_karhutla = sum(luas_karhutla, na.rm = TRUE), .groups = "drop")

sf_year <- indo_sf %>% left_join(map_data, by = "Provinsi")
print(sf_year)
pal <- colorNumeric("Reds",
                    domain  = sf_year$luas_karhutla,
                    na.color = "#f0f0f0")

leaflet(sf_year) %>%
  addTiles() %>%
  addPolygons(
    fillColor   = ~pal(luas_karhutla),
    fillOpacity = 0.8,
    weight      = 1,
    
  )


unmatched <- sf_year$Provinsi[is.na(sf_year$luas_karhutla)]
if (length(unmatched) > 0) {
  cat("Provinsi tidak cocok:", paste(unmatched, collapse = ", "), "\n")
}


data_long_suhu <- data_suhu
View(data_long_suhu)
View(data_long_emisi)
View(data_long_kebkar)
setdiff(data_long_kebkar$Provinsi, indo_sf$Provinsi)
setdiff(indo_sf$Provinsi, data_long_kebkar$Provinsi)

gabung_12  <- inner_join(data_long_emisi,  data_long_kebkar, by = c("Provinsi", "tahun"))
gabung_all <- inner_join(gabung_12, data_long_suhu,       by = "tahun")

function(input, output, session) {
  
  ## 2.1  Bar chart – luas karhutla per provinsi (tahun terpilih)
  output$barChartKebakaran <- renderPlot({
    yr   <- input$dropdownKebakaranBar          
    plot_data <- data_long_kebkar |>
      filter(tahun == yr) |>
      arrange(desc(luas_karhutla))
    
    ggplot(plot_data, aes(x = reorder(Provinsi, -luas_karhutla),
                          y = luas_karhutla)) +
      geom_col(fill = "#e74c3c") +
      labs(title = paste("Luas Kebakaran Hutan –", yr),
           x = "Provinsi", y = "Luas (ha)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  ## 2.2  Line chart – tren kebakaran satu provinsi
  output$lineChartKebakaran <- renderPlot({
    prov <- input$dropdownKebakaranLine         
    plot_data <- if (prov=="Indonesia") {
      data_long_kebkar |>
        group_by(tahun) |>
        summarise(luas_karhutla = sum(luas_karhutla, na.rm = TRUE), .groups = "drop") |>
        mutate(label="Indonesia")
    } else {
      data_long_kebkar |>
        filter(Provinsi == prov) |>
        mutate(label=Provinsi)
    }
    ggplot(plot_data, aes(tahun, luas_karhutla)) +
      geom_line(color = "#2980b9", linewidth = 1.2) +
      geom_point() +
      labs(title = paste("Tren Luas Karhutla –", prov),
           x = "Tahun", y = "Luas (ha)") +
      theme_minimal()
  })
  
  ## 2.3  Bar chart emisi karbon per provinsi (tahun)
  output$barChartKarbon <- renderPlot({
    yr <- input$dropdownKarbonBar
    plot_data <- data_long_emisi |>
      filter(tahun == yr) |>
      arrange(desc(emisi_karbon))
    
    ggplot(plot_data, aes(reorder(Provinsi, -emisi_karbon), emisi_karbon)) +
      geom_col(fill = "#27ae60") +
      labs(title = paste("Emisi Karbon –", yr),
           x = "Provinsi", y = "Emisi (kt CO₂e)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ## 2.4  Line chart emisi karbon satu provinsi
  output$lineChartKarbon <- renderPlot({
    prov <- input$dropdownKarbonLine           
    plot_data <- if (prov=="Indonesia") {
        data_long_emisi |>
        group_by(tahun) |>
        summarise(emisi_karbon = sum(emisi_karbon, na.rm = TRUE), .groups = "drop") |>
        mutate(label="Indonesia")
      } else {
        data_long_emisi |>
        filter(Provinsi == prov) |>
          mutate(label=Provinsi)
      }
        
    
    ggplot(plot_data, aes(tahun, emisi_karbon)) +
      geom_line(color = "#16a085", linewidth = 1.2) +
      geom_point() +
      labs(title = paste("Tren Emisi Karbon –", prov),
           x = "Tahun", y = "Emisi (kt CO₂e)") +
      theme_minimal()
  })
  

  ## 2.5  Bar chart suhu rata2 nasional (tahun)
  output$barChartSuhu <- renderPlot({
    yr <- input$dropdownSuhuBar               
    plot_data <- data_long_suhu |>
      filter(tahun == yr)
    
    ggplot(plot_data, aes("", Suhu_Rata)) +
      geom_col(fill = "#f1c40f") +
      coord_flip() +
      labs(title = paste("Rata‑rata Suhu Nasional –", yr),
           x = NULL, y = "Suhu (°C)") +
      theme_minimal()
  })
  
  ## 2.6  Line chart suhu rata2 nasional 2015‑2024
  output$lineChartSuhu <- renderPlot({
    ggplot(data_long_suhu, aes(tahun, Suhu_Rata)) +
      geom_line(color = "#d35400", linewidth = 1.2) +
      geom_point() +
      labs(title = "Tren Rata‑rata Suhu Nasional",
           x = "Tahun", y = "Suhu (°C)") +
      theme_minimal()
  })
  

  ## 2.7  Ranking luas kebakaran
  output$rankKebakaran <- renderPlot({
    yr <- input$dropdownRankKebakaran          
    plot_data <- data_long_kebkar |>
      filter(tahun == yr) |>
      slice_max(order_by = luas_karhutla, n = 3)
    
    ggplot(plot_data, aes(x = reorder(Provinsi, luas_karhutla),
                          y = luas_karhutla)) +
      geom_col(fill = "#c0392b") +
      coord_flip() +
      labs(title = paste("Top‑10 Luas Karhutla –", yr),
           x = NULL, y = "Luas (ha)") +
      theme_minimal()
  })
  
  ## 2.8  Ranking emisi karbon
  output$rankKarbon <- renderPlot({
    yr <- input$dropdownRankKarbon              
    plot_data <- data_long_emisi |>
      filter(tahun == yr) |>
      slice_max(order_by = emisi_karbon, n = 3)
    
    ggplot(plot_data, aes(reorder(Provinsi, emisi_karbon), emisi_karbon)) +
      geom_col(fill = "#1abc9c") +
      coord_flip() +
      labs(title = paste("Top‑10 Emisi Karbon –", yr),
           x = NULL, y = "Emisi (kt CO₂e)") +
      theme_minimal()
  })
  
  ## 2.9  Ranking suhu
  output$rankSuhu <- renderPlot({
    yr <- input$dropdownRankSuhu               
    plot_data <- data_long_suhu |>
      filter(tahun == yr) |>
      slice_max(order_by = suhu, n = 3)
    
    ggplot(plot_data, aes(reorder(Provinsi, suhu), suhu)) +
      geom_col(fill = "#f39c12") +
      coord_flip() +
      labs(title = paste("Top‑10 Suhu Terpanas –", yr),
           x = NULL, y = "Suhu (°C)") +
      theme_minimal()
  })
  
  
  output$mapKebakaran <- renderLeaflet({
    yr <- input$dropdownKebakaranMap
    
    # data per‑provinsi
    map_data <- data_long_kebkar %>%
      filter(tahun == yr) %>%
      group_by(Provinsi) %>%
      summarise(luas_karhutla = sum(luas_karhutla, na.rm = TRUE),
                .groups = "drop")
    
    sf_year <- indo_sf %>% left_join(map_data, by = "Provinsi")
    

    validate(
      need(any(!is.na(sf_year$luas_karhutla)),
           paste("Tidak ada data kebakaran untuk tahun", yr))
    )
    
    pal <- colorNumeric("Reds",
                        domain  = sf_year$luas_karhutla,
                        na.color = "#f0f0f0")
    
    leaflet(sf_year) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = ~pal(luas_karhutla),
        fillOpacity = 0.8,
        weight      = 1,
        color       = "white",
        label       = ~paste0(
          Provinsi, "<br>",
          format(luas_karhutla, big.mark = ","), " ha")
      ) %>%
      addLegend("bottomright",
                pal     = pal,
                values  = ~luas_karhutla,
                title   = paste("Luas Karhutla", yr),
                labFormat = labelFormat(big.mark = ","))
  })
  
  
  
  
  ## 2.11  Peta emisi karbon
  # CHOROPLETH EMISI KARBON
  output$mapKarbon <- renderLeaflet({
    yr <- input$dropdownPetaEmisi
    
    map_data <- data_long_emisi %>%
      filter(tahun == yr) %>%
      group_by(Provinsi) %>%
      summarise(emisi_karbon = sum(emisi_karbon, na.rm = TRUE),
                .groups = "drop")
    
    sf_year <- indo_sf %>% left_join(map_data, by = "Provinsi")
    
    validate(
      need(any(!is.na(sf_year$emisi_karbon)),
           paste("Tidak ada data emisi untuk tahun", yr))
    )
    
    pal <- colorNumeric("Greens",
                        domain  = sf_year$emisi_karbon,
                        na.color = "#f0f0f0")
    
    leaflet(sf_year) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = ~pal(emisi_karbon),
        fillOpacity = 0.8,
        weight      = 1,
        color       = "white",
        label       = ~htmltools::HTML(
          sprintf("%s<br>%s&nbsp;kt&nbsp;CO\u2082e",
                  Provinsi,
                  format(emisi_karbon, big.mark = ",")))
      ) %>%
      addLegend("bottomright",
                pal     = pal,
                values  = ~emisi_karbon,
                title   = paste("Emisi Karbon", yr),
                labFormat = labelFormat(big.mark = ","))
  })
  
  
  
}
