library(tidyverse)
library(datos)
library(highcharter)

paises_2007 <- datos::paises %>% 
  filter(anio == max(anio)) %>% 
  mutate(poblacion = round(poblacion/1e6))

data_to_hierarchical_series <- function(data, group_vars, size_var) {
  
  # data <- paises_2007
  # group_vars <- c("continente", "pais")
  # size_var <- "poblacion"
  
  group_syms <- rlang::syms(group_vars)
  size_sym <- rlang::sym(size_var)
  

  if (data %>%
      select(!!!group_syms) %>%
      mutate_all(as.character) %>%
      purrr::map(unique) %>%
      unlist() %>%
      anyDuplicated()) stop("Sunburst data uses same label at multiple levels.")
  
  data <- data %>%
    mutate_at(vars(all_of(group_vars)), as.character)
  
  name_cell <- function(..., depth) paste0(list(...), 1:depth, collapse = "")
  
  data_at_depth <- function(depth = 1) {
    
    data %>%
      group_by(!!!group_syms[1:depth]) %>%
      summarise(value = sum(!!size_sym)) %>%
      ungroup() %>%
      arrange(desc(value)) %>% 
      mutate(name = !!group_syms[[depth]],
             level = depth) %>%
      # mutate_at(group_vars, as.character()) %>%
      {
        if (depth == 1)
          mutate(., id = paste0(name, 1))
        else {
          mutate(
            .,
            parent = purrr::pmap_chr(list(!!!group_syms[1:depth - 1]),
                                     name_cell,
                                     depth = depth - 1),
            id = paste0(parent, name, depth)
          )
        }
      }
  }
  
  sunburst_df <- 1:length(group_vars) %>%
    purrr::map(data_at_depth) %>%
    bind_rows() %>%
    arrange(level)
  
  data_list <- sunburst_df %>%
    highcharter::list_parse() # %>% purrr::map( ~ .[!is.na(.)])
  
  data_list
  
}


reduced_countries <- paises_2007 %>% 
  filter(continente %in% c("África", "Américas", "Oceanía"))

government_tree <- data_to_hierarchical_series(
  government_last,
  group_vars = c('left_right','party'),
  size_var = "prob"
)

dataserie <- data_to_hierarchical_series(
  reduced_countries,
  group_vars = c("continente", "pais"),
  size_var = "poblacion"
)

highchart() %>%
  hc_add_series(
    data = government_tree,
    type = "sunburst",
    # type=  "treemap",
    allowDrillToNode = TRUE,
    levels = list(
      list(
        level = 1,
        borderWidth = 0,
        borderColor = "transparent",
        colorByPoint = TRUE,
        dataLabels = list(enabled = TRUE)
      ),
      list(
        level = 2,
        borderWidth = 0,
        borderColor = "transparent",
        colorVariation = list(key = "brightness", to = 0.50),
        dataLabels = list(enabled = TRUE)
      )
    )
  )






library(scales)

diamantes_cortes <- count(diamantes, corte)
diamantes_cortes <- mutate(diamantes_cortes, porcentaje = percent(n/sum(n)))

diamantes_cortes

# para un pie remover la parte de "innerSize"
hc06 <- hchart(
  diamantes_cortes, "pie", hcaes(name = corte, y = n),
  name = "Corte",
  innerSize = "80%",
  dataLabels = list(format = "{point.name}<br>({point.porcentaje})")
)

hc06



