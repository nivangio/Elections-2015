library(rvest)

raw <-  html("http://www.resultados.gob.ar/nacionaltelegr/04/002/0015/040020015_3184.htm")

aaa <- raw %>% html_node("#contentinfomesa") %>% html_node("#divTvotos") %>% html_node("#TVOTOS")  %>% html_table()
bbb <- raw %>% html_node(".tablon") %>% html_table()
