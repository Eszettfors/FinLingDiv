library(tidyverse)
library(shiny)
library(leaflet)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(treemapify)
library(colorspace)
library(plotly)

# set theme
theme_set(theme_bw())

# this is a shiny app which allows visualization of Finland register data

# allow the selection of year, measure -> impact the map
# clicking a country shows the distribution of languages in that country
# selecting a list number of countries draws a line plot showing their diversity across time according to the selected measure. 
# select a language and gets its distribution


# read data ######
df_fin = read_csv("data/processed/full_time_series_speakers.csv") %>%
  rename("language_name" = language)

df_language = df_fin %>%
  distinct(ISO6393, language_name, family, macroarea, glottocode, classification, family_name)
df_municip = df_fin %>%
  distinct(municipality)
df_diversity = read_csv("data/processed/diversity_time_series.csv")
df_geo = read_sf("data/geodata", options = "ENCODING=latin1") %>%
  rename("municipality" = mncplty)

#sim_m = read_rds("data/final_dataset/lexical_similarity_matrix.rds")

df_geo = sf::st_transform(df_geo, crs = 4326)

# add most dominant language and its percent as a diversity variable that can be visualized on the map
# select the most dominant language of each country and year
df_dominant = df_fin %>%
  group_by(municipality, year) %>%
  mutate(percent = speakers / sum(speakers) * 100) %>%
  slice_max(order_by = percent, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename("dominant_language" = language_name,
         "dominant_language_percent" = percent) %>%
  select(municipality, year, dominant_language, dominant_language_percent)


df_diversity = df_dominant %>%
  right_join(df_diversity, join_by(municipality, year))


# set up language data
df_fin = df_fin %>%
  group_by(municipality, year) %>%
  mutate(percent = speakers / sum(speakers) * 100) %>%
  select(municipality, year, language_name, family_name, speakers, percent) %>%
  ungroup()


# add geodata
df_diversity = df_diversity %>%
  right_join(df_geo) %>%
  st_as_sf()

mst_spkn_langs = df_dominant %>%
  distinct(dominant_language) %>% pull(dominant_language)

# colors for languages
custom_palette = c(
  "Sami" = "#1f78b4",  # blue
  "Finnish"  = "#e31a1c",  # red
  "Swedish" = "#ffDA03" # yellow
)

rest_palette = qualitative_hcl(40, "Dark 3")

missing_langs = setdiff(mst_spkn_langs, names(custom_palette))
rest_palette = setNames(rest_palette[seq_along(missing_langs)], missing_langs)
full_palette = c(custom_palette, rest_palette)

###### functions

get_languages = function(data = df_fin, mncp, y){
  langs = data %>%
    filter(municipality == mncp) %>%
    filter(year == y) %>%
    mutate(speakers_perc = round(speakers / sum(speakers) * 100, 2)) %>%
    select(language_name, family_name, speakers, speakers_perc) %>%
    arrange(desc(speakers_perc)) %>%
    rename("Language" = "language_name",
           "Family" = "family_name",
           "Speakers (%)" = "speakers_perc",
           "Speakers" = "speakers")
  
  langs = langs %>%
    as_tibble()
  
  return(langs)
}

get_languages(mncp = "Närpes", y = 2022)

get_languages_global = function(data = df_fin, y = 2023) {
  # summarizes languages on a global level according to tweets

  langs = data %>%  
    filter(year == y) %>%
    group_by(language_name) %>%
    summarise(
      family = unique(family_name),
      speakers = sum(speakers, na.rm = TRUE)) %>%
    mutate(percent = round(100 * speakers / sum(speakers), 2)) %>%
    arrange(desc(speakers)) %>%
    rename("Language" = "language_name",
           "Family" = "family",
           "Speakers (%)" = "percent",
           "Speakers" = "speakers")

    
    langs = langs %>%
      as_tibble()
    
    return(langs)
}




get_mncp_comparison_mncp_ts = function(data = df_diversity, mncp = c("Vaasa", "Helsinki"), measure = "richness"){
  # this function takes two or more mncp, a diversity measure and plots a timeseries

  diversity_measure = sym(measure)
  
  # filter to countries of interest
  data = data %>%
    filter(municipality %in% mncp)
  print(data)
  #plot
  p = data %>%
    ggplot(aes(y = !!diversity_measure, fill = municipality, color = municipality, x = year)) +
    geom_point() + 
    geom_line() +
    labs(title = paste0("Diveristy time series, ", rlang::as_name(diversity_measure))) + 
    labs(fill = "Municipality",
         color = "Municipality")
  plot(p)
  return(p)
  
}
get_mncp_comparison_mncp_ts(mncp = c("Turku", "Närpes"))


get_treemap = function(data = df_fin, mncp = c("Helsinki"), y = "2023") {
  
  #filter and get percent
  data = data %>%
    filter(municipality %in% mncp) %>%
    filter(year == y) %>%
    mutate(percent = speakers / sum(speakers) * 100)
  
  p = ggplot(data, aes(
    area = percent,
    fill = family_name,
    label = paste0(language_name, "\n", round(percent, 2), "%")
  )) +
    geom_treemap(color = "black") +
    geom_treemap_text(
      colour = "white",
      place = "centre",
      grow = TRUE,
      reflow = TRUE
    ) + 
    labs(title = paste0("Language distribution in ", mncp, ", ", y)) +
    theme(legend.position = "none")
  
  plot(p)
  return(p)
  
}  
test = get_treemap()

get_top_10_langs_ts = function(data = df_fin, mncp = "Helsinki") {
  
  data_filtered = data %>%
    filter(municipality == mncp) %>%
    group_by(year) %>%
    mutate(percent = speakers / sum(speakers) * 100) %>%
    ungroup()
  
<<<<<<< Updated upstream
  
=======
>>>>>>> Stashed changes
  # make complete
  data_filtered = data_filtered %>%
    select(year, language_name, speakers, percent) %>%
    complete(year, language_name,
             fill = list(speakers = 0, percent = 0))
  
<<<<<<< Updated upstream
  
=======
>>>>>>> Stashed changes
  # Top 10 languages overall
  top_langs = data_filtered %>%
    group_by(language_name) %>%
    summarise(total = sum(speakers)) %>%
    slice_max(total, n = 10) %>%
    pull(language_name)
  
  data_plot = data_filtered %>%
    mutate(language_name = ifelse(!language_name %in% top_langs, "*other", language_name)) %>%
    group_by(language_name, year) %>%
    summarize(speakers = sum(speakers)) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(percent = speakers / sum(speakers) * 100)
  
  
  p = ggplot(data_plot, aes(x = year, y = percent, fill = language_name)) +
    geom_area(alpha = 0.8, size = 0.1, color = "white") +
    labs(
      title = paste("10 most spoken languages over time in", mncp),
      x = "Year",
      y = "Percent of Page Views"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank())
<<<<<<< Updated upstream

=======
>>>>>>> Stashed changes
  return(p)
}


test = get_top_10_langs_ts(df_fin, "Närpes")
plot(test)
# add percentage and most spoken language

# adjust naming of variables
df_diversity = df_diversity %>% 
  rename("Richness" = "richness",
         "Exponent Shannon" = "exp_shannon",
         "Inverse Simpson" = "inv_simpson",
         "Lexical Diversity (q=0)" = "lex_div_q_0",
         "Lexical Diversity (q=1)" = "lex_div_q_1",
         "Lexical Diversity (q=2)" = "lex_div_q_2",
         "Dominant Language" = "dominant_language")

# define year choices
year_choices = sort(unique(df_diversity$year))

# define language choices 
lang_choices = sort(unique(df_fin$language_name))


ui = fluidPage(
  titlePanel("The Finnish Linguistic Diversity Dashboard"),
  tabsetPanel(
    id = "main_tabs",
    
    # --- Tab 1: Diversity Dashboard ---
    tabPanel("Diversity",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "selected_measure",
                   label = "Select a measure to visualize:",
                   choices = c(
                     "Richness", "Exponent Shannon", "Inverse Simpson",
                     "Lexical Diversity (q=0)", "Lexical Diversity (q=1)",
                     "Lexical Diversity (q=2)", "Dominant Language"
                   ),
                   selected = "Dominant Language"
                 ),
                 selectInput(
                   inputId = "selected_year",
                   label = "Select a year:",
                   choices = sort(unique(df_diversity$year)),
                   selected = 2025,
                   multiple = FALSE
                 ),
                 radioButtons(
                   inputId = "map_mode",
                   label = "Map type",
                   choices = c(
                     "Diversity measure" = "diversity",
                     "Language share"    = "language"
                   ),
                   selected = "diversity",
                   inline = TRUE
                 ),
                 selectizeInput(
                   inputId = "selected_lang",
                   label = "Select language",
                   choices = sort(lang_choices),
                   selected = lang_choices[lang_choices == "Finnish"],
                   options = list(
                     placeholder = "Type to search a language",
                     maxOptions = 1000
                 )),
                 radioButtons(
                   inputId = "selected_vis",
                   label = "Select Visualization:",
                   choices = c("Time Series" = "ts", "Language Treemap" = "treemap"),
                   selected = "treemap",
                   inline = TRUE
                 ),
                
                 h4("Most spoken languages"),
                 radioButtons(
                   "table_view_mode", 
                   "Select Table View:", 
                   choices = c("Municipality" = "municipality", "Finland" = "global"),
                   selected = "global",
                   inline = TRUE
                 ),
                 DT::dataTableOutput("language_table")
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.map_mode == 'diversity'",
                   leafletOutput("map", height = "600px")
                 ),
                 
                 conditionalPanel(
                   condition = "input.map_mode == 'language'",
                   leafletOutput("lang_map", height = "600px")
                 ),
                 conditionalPanel(
                   condition = "input.selected_vis == 'ts'",
                   plotOutput("ts", height = "400px")
                 ),
                 conditionalPanel(
                   condition = "input.selected_vis == 'treemap'",
                   plotOutput("language_treemap", height = "450px")
                 )
                 
               )
             )
    ),
    tabPanel("About",
             h3("About this dashboard"),
             p("This dashboard was created to visualize the FinLingDiv dataset and showcase how the lingustic diversity of Finland has changed between 1990 and 2024. The data stems from the Finnish Population Information System (https://dvv.fi/en/population-information-system) where each persons address and native language is recorded, allowing us to infer the linguistic composition of 
               the respective municipalities of Finland and ultimately quantify changes in diversity."),
             p("The source of the data is Statistic Finland and is published under a Creative Commons Attribution 4.0 International license. The data was processed by Hannes Essfors who is the owner and maintainer of the dashboard and reserves all rights. Linguistic Diversity was calculated according to the Leinster-cobbold framework (2012). For more information, please see
               Essfors (2025). The dataset can be accessed through Zenodo: https://zenodo.org/records/18257720. The dataset should be cited as: Essfors, H. (2026). FinLingDiv (1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.18257720"),
             p("If you want to cite the dashboard directly, please use: Essfors, H (2026). The Finnish Linguistic Diversity Dashboard [Shiny web application], https://f39e09-hannes-essfors.shinyapps.io/FinLingDiv/."),
             p("1. Essfors, H. N. H. (2025). Global linguistic diversity – Adapting the Leinster-Cobbold framework from ecology for humanities research. In T. Arnold, M. Fantoli, & R. Ros (Eds.), Anthology of Computers and the Humanities (Vol. 3, pp. 653–669). Association for Computers and the Humanities. https://doi.org/10.63744/srhQaCwGo5mj")
             
             )))


# Server
server = function(input, output, session) {
  
  clicked_mncp = reactiveVal(NULL)
  
  # --- Diversity data ---
  color_data = reactive({
    req(input$selected_measure, input$selected_year)
    df_diversity %>%
      filter(year == input$selected_year) %>%
      mutate(selected_value = .data[[input$selected_measure]])
  })
  
  # for numeric color values
  measures_pal = reactive({
    req(input$selected_measure)
    
    if (input$selected_measure != "Dominant Language") {
      colorNumeric(
        palette = "YlGnBu",
        domain = color_data()$selected_value,
        na.color = "grey"
      )
    } else {
      NULL
    }
  })
  
  # for categoric language colors
  language_colors = colorFactor(
    palette = full_palette, 
    domain = mst_spkn_langs, 
    na.color = "grey"
  )
  
  # for the language map
  lang_data_filtered = reactive({
    req(input$selected_lang, input$selected_year)
    
    lang_df = df_fin %>%
      filter(language_name == input$selected_lang,
             year == input$selected_year)
    
    df_geo %>%
      left_join(lang_df, by = "municipality") %>%
      st_as_sf()
    
  })
  
  # color for the language map
  lang_pal = reactive({
    req(lang_data_filtered())
    
    colorNumeric(
      palette = "YlGnBu",
      domain = lang_data_filtered()$percent,
      na.color = "grey"
    )
  })

  
  # Diversity map
  output$map = renderLeaflet({
    leaflet(df_diversity) %>%
      addTiles() %>%
      addPolygons() %>%
      setView(lng = 26, lat = 65.2, zoom = 5)
  })
  
  
  # langauge map
  output$lang_map = renderLeaflet({
    leaflet(df_geo) %>%
      addTiles() %>%
      addPolygons() %>%
      setView(lng = 26, lat = 65.2, zoom = 5)
  })
  
  # observe for diversity map
  observe({
    req(input$map_mode == "diversity")
    data <- color_data()
    
    if (input$selected_measure == "Dominant Language") {
      
      leafletProxy("map", data = data) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          layerId = ~municipality,
          fillColor = ~language_colors(`Dominant Language`),
          fillOpacity = ~dominant_language_percent / 100,
          color = "#BDBDC3",
          weight = 1,
          highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
          label = ~paste0(
            municipality, ": ",
            `Dominant Language`, " (",
            round(dominant_language_percent, 2), "%)"
          )
        ) %>%
        addLegend(
          pal = language_colors,
          values = data$`Dominant Language`,
          opacity = 1,
          title = "Dominant Language",
          position = "bottomright"
        )
      
    } else {
      
      pal_func <- measures_pal()
      
      leafletProxy("map", data = data) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          layerId = ~municipality,
          fillColor = ~pal_func(selected_value),
          fillOpacity = 0.7,
          color = "#BDBDC3",
          weight = 1,
          highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
          label = ~paste0(municipality, ": ", round(selected_value, 2))
        ) %>%
        addLegend(
          pal = pal_func,
          values = data$selected_value,
          opacity = 0.7,
          title = input$selected_measure,
          position = "bottomright",
          labFormat = labelFormat(digits = 3)
        )
    }
  })
  
  # observe for language map
  observe({
    req(input$map_mode == "language")
    data <- lang_data_filtered()
    pal  <- lang_pal()
    
    leafletProxy("lang_map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        layerId = ~municipality,
        fillColor = ~pal(percent),
        fillOpacity = 0.7,
        color = "#BDBDC3",
        weight = 1,
        highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
        label = ~paste0(
          municipality, ": ",
          round(percent, 2), "% ",
          input$selected_lang
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~percent,
        opacity = 0.7,
        title = paste0(input$selected_lang, " (", input$selected_year, ")"),
        position = "bottomright",
        labFormat = labelFormat(suffix = "%")
      )
  })
  
  
  # time series
  output$ts = renderPlot({
    req(clicked_mncp(), input$selected_measure, input$selected_vis)
    if(input$selected_vis == "ts" && input$selected_measure != "Dominant Language"){
      get_mncp_comparison_mncp_ts(mncp = clicked_mncp(), measure = input$selected_measure)
    } else if(input$selected_vis == "ts" && input$selected_measure == "Dominant Language"){
      get_top_10_langs_ts(mncp = clicked_mncp())
    }
  })
  
  output$language_treemap = renderPlot({
    req(clicked_mncp(), input$selected_year)
    get_treemap(mncp = clicked_mncp(), y = input$selected_year)
    
  })
  

  
  # Use radioButtons input directly
  table_view_mode = reactive({ input$table_view_mode })
  
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)
    
    clicked_mncp(input$map_shape_click$id)
    
    updateRadioButtons(
      session,
      "table_view_mode",
      selected = "municipality"
    )
  })
  
  observeEvent(input$lang_map_shape_click, {
    req(input$lang_map_shape_click$id)
    
    clicked_mncp(input$lang_map_shape_click$id)
    
    updateRadioButtons(
      session,
      "table_view_mode",
      selected = "municipality"
    )
  })
  
  # Language table
  output$language_table = DT::renderDataTable({
    mode = table_view_mode()
    
    if (mode == "global") {
      lang_data = get_languages_global(y = input$selected_year) 
      
      } else if (mode == "municipality" && !is.null(clicked_mncp())) {
        lang_data = get_languages(mncp = clicked_mncp(), y = input$selected_year)
      }
    else{
      lang_data = data.frame()
    }
    
    DT::datatable(lang_data, options = list(pageLength = 10), rownames = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
