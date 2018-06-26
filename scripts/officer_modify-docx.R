install.packages("officer")

library(officer)
library(magrittr)
library(here)

# initial document
my_doc <- read_docx() 
styles_info(my_doc)

# create an image from a plot
src <- tempfile(fileext = ".png")
png(filename = src, width = 5, height = 6, units = 'in', res = 300)
barplot(1:10, col = 1:10)
dev.off()
browseURL(src)

# add that image to the document along with some new text paragraphs and a table
my_doc <- my_doc %>% 
  body_add_img(src = src, width = 5, height = 6, style = "centered") %>% 
  body_add_par("Hello world!", style = "Normal") %>% 
  body_add_par("", style = "Normal") %>% # blank paragraph
  body_add_table(iris[1:10,], style = "table_template")

# write the Word file
docx_1 <- here("scripts/officer_example_01.docx")
print(my_doc, target = docx_1)
browseURL(docx_1)

# read in doc, add table, write out
docx_2 <- here("scripts/officer_example_02.docx")
read_docx(docx_1) %>%
  cursor_bookmark("tbl1") %>%
  body_add_table(iris[1:10,], style = "table_template") %>%
  print(target=docx_2)
browseURL(docx_2)

insert_table_caption <- function (x, text="", style = NULL, depth){
  x %>% 
    cursor_backward() %>%
    body_add_par(value = text, style="table title") %>% 
    slip_in_text(str = ". ", style = style, pos = "before") %>%
    slip_in_seqfield(
      #str = "SEQ table \\* Arabic \\s 1 \\* MERGEFORMAT", 
      str = " SEQ Table \\* ARABIC ", 
      style = style, pos = "before") %>%
    slip_in_text(str = "Table ", style = style, pos = "before")
}


# test replacement with caption
docx_3 <- here("scripts/officer_example_03.docx")
d3 <- read_docx(docx_2) %>%
  # move to table
  cursor_bookmark("tbl1") %>%
  cursor_forward() %>%
  # replace table
  body_add_table(iris[11:20,], style = "table_template", pos = "on") %>%
  insert_table_caption("Example caption of data iris, rows 11-20.") %>% 
  print(target=docx_3)
browseURL(docx_3)

# test replacement of table with caption
docx_4 <- here("scripts/officer_example_04.docx")
read_docx(docx_3) %>%
  # move to table
  cursor_bookmark("tbl1") %>%
  cursor_forward() %>%
  body_remove() %>% # remove caption
  # replace table
  body_add_table(iris[21:25,], style = "table_template", pos = "on") %>%
  insert_table_caption("Example caption of data iris, rows 21-25.") %>% 
  print(target=docx_4)
browseURL(docx_4)

x <- read_docx(docx_3) %>%
  cursor_bookmark("tbl1")

is_cursor_at_table_caption <- function(x){
  node <- x$doc_obj$get_at_cursor()
  if (xml2:::xml_name(node) == "p"){
    par_data <- officer:::par_as_tibble(x$doc_obj$get_at_cursor(), styles_info(x))
    if ("text" %in% names(par_data) && 
        stringr::str_sub(par_data$text, 1, 9) == "Table SEQ"){
      return(T)
    } else {
      return(F)
    }
  } else {
    return(F)
  }
}

