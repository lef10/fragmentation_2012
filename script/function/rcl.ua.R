library(forcats)
library(dplyr)

rlc.urbancode <- function(var) {

        cl <- as.numeric(case_when((var == "11100" ~ "1"),
                                   (var == "11210" ~ "1"),
                                   (var == "11220" ~ "1"),
                                   (var == "11230" ~ "1"),
                                   (var == "11240" ~ "1"),
                                   (var == "11300" ~ "1"),
                                   (var == "12100" ~ "1"),
                                   (var == "12300" ~ "1"),
                                   (var == "12400" ~ "4"),
                                   (var == "12210" ~ "3"),
                                   (var == "12220" ~ "3"),
                                   (var == "12230" ~ "3"),
                                   (var == "13100" ~ "1"),
                                   (var == "13300" ~ "1"),
                                   (var == "13400" ~ "1"),
                                   (var == "14200" ~ "2"),
                                   (var == "14100" ~ "2"),
                                   (var == "21000" ~ "2"),
                                   (var == "22000" ~ "2"),
                                   (var == "23000" ~ "2"),
                                   (var == "24000" ~ "2"),
                                   (var == "25000" ~ "2"),
                                   (var == "31000" ~ "2"),
                                   (var == "32000" ~ "2"),
                                   (var == "33000" ~ "2"),
                                   (var == "40000" ~ "2"),
                                   (var == "50000" ~ "4"),
                                   TRUE ~ "NA"))

return(cl)
}


rlc.urbanclassif <- function(var) {
  
  cl <- as.factor(case_when((var == "11100" ~ "Urban fabric"),
                             (var == "11210" ~ "Urban fabric"),
                             (var == "11220" ~ "Urban fabric"),
                             (var == "11230" ~ "Urban fabric"),
                             (var == "11240" ~ "Urban fabric"),
                             (var == "11300" ~ "Isolated structures"),
                             (var == "12100" ~ "Industrial, commercial"),
                             (var == "12300" ~ "Port"),
                             (var == "12400" ~ "Airport"),
                             (var == "12210" ~ "Linear structure"),
                             (var == "12220" ~ "Linear structure"),
                             (var == "12230" ~ "Linear structure"),
                             (var == "13100" ~ "Mineral extraction"),
                             (var == "13300" ~ "Construction"),
                             (var == "13400" ~ "Land with current use"),
                             (var == "14200" ~ "Green urban"),
                             (var == "14100" ~ "Green urban"),
                             (var == "21000" ~ "Farming"),
                             (var == "22000" ~ "Farming"),
                             (var == "23000" ~ "Farming"),
                             (var == "24000" ~ "Farming"),
                             (var == "25000" ~ "Farming"),
                             (var == "31000" ~ "Forest"),
                             (var == "32000" ~ "Vegetation"),
                             (var == "33000" ~ "Open spaces"),
                             (var == "40000" ~ "Wetlands"),
                             (var == "50000" ~ "Water"),
                             TRUE ~ "NA"))
  
  return(cl)
}

# aggregator function
aggreg.union <- function(x, y, area, pop) {
  
  ag.union <- x %>%
    group_by(!!sym(y)) %>%
    summarise(area = sum(!!sym(area)), pop = sum(!!sym(pop)))
  
  return(ag.union)
}

# fonction pour calculer la geometrie
perimeter <- function(x) {
  p <- sf::st_length(sf::st_cast(x, "MULTILINESTRING"))
  
  return(p)
}


# calcul indices des formes
pa.shape <- function(x, c=2, all =FALSE ){
  a <- sf::st_area(x)
  p <- sf::st_length(sf::st_cast(x, "MULTILINESTRING"))
  pa <- p/a^(1/c)
  y <- if(all == TRUE){cbind(a, p, pa)}
  else{pa}
  return(y)
}

# calcul des indices de fragment
pa.frag.size <- function(x, all =FALSE ){
  a <- sf::st_area(x)
  pa <- sqrt(a)
  return(pa)
}

