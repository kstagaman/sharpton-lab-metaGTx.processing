# generate_template_data.R

template.code <- readLines(con = "template.R")
save(template.code, file = "data/template_code.RData")
