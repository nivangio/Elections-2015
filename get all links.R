library(rvest)

url.base <- "http://www.resultados.gob.ar/nacionaltelegr/"

list <- html(paste0(url.base,"IPRO.htm"))

provincias.links <- list %>% html_nodes("a") %>% html_attr("href")
provincias.htmls <- sapply(provincias.links, function(x) html(paste0(url.base,x)))

municipios.links <- lapply(provincias.htmls, function(x) x %>% html_nodes("a") %>% html_attr("href"))
municipios.htmls <- lapply(municipios.links, function(y) lapply(y, function(x) html(paste0(url.base,x))))

secciones.links <- lapply(municipios.htmls, function(y) lapply(y, function(x) x %>% html_nodes("a") %>% html_attr("href")))
secciones.htmls <- lapply(secciones.links, function(z) lapply(z, function(y) 
                    lapply(y, function(x) html(paste0(url.base,x)))))

mesas.links <-  lapply(secciones.htmls, function(z) lapply(z, function(y) 
  lapply(y, function(x) x %>% html_nodes("a") %>% html_attr("href"))))

mesas.scrape <- unlist(mesas.links)

tables <- lapply(mesas.scrape, function(x) {
  
  raw <-  html(paste0(url.base,x))
  positives <- raw %>% html_node("#contentinfomesa")
  if(is.null(positives)) {positives <- NA} else {
    positives <- positives %>% html_node("#TVOTOS")  %>% html_table()  
  }
  negatives <- raw %>% html_node(".tablon") 
  if(is.null(negatives)) {negatives <- NA} else {
    negatives <- negatives %>% html_table()
  }
  
   return(list(positives,negatives))
})

