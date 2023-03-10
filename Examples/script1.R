
srcFiles <- list.files("R/", full.names = T)
purrr::walk(srcFiles, ~source(.x))
