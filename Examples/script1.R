
srcFiles <- list.files("R/", full.names = T)
purrr::walk(srcFiles, ~source(.x))

fn <- list.files(path = "/home/ben/Documents/Thermo/TKO-Example", "*.raw")
fn


tt <- info$new(name = "test")
tt$add(data = readData(dataFrame = mtcars))
tt$add(data = readData(dataFrame = iris))
tt$data[[1]]
tt$data[[2]]
tt$save(path = "data", filename = "")

tt2 <- info$new(name = "test")
tt2$load(path = "data", filename = "")
tt2
tt2$name <- "test2"
tt2$load(path = "data", filename = "", useName = "test")
tt2$data[[1]]
tt2$data[[2]]

dbr <- BBPersonalR::db_open("data/test.sqlite")
tt <- createInfo(name =  "test", type = "infoDB")
tt$add(data =  readData(dataFrame = mtcars))
tt$name
tt$empty
tt$save(db = dbr, overwrite = T)
tt
tt$data[[1]]

tt2 <- createInfo(name =  "test2", type = "infoDB")
iris2 <- iris
colnames(iris2) <- str_replace(colnames(iris2), pattern = "\\.", replacement = "_")
tt2$add(data = readData(dataFrame = iris2))
tt2
tt2$data[[1]]
tt2$save(db = dbr, overwrite = T)
tt2
tt2$data[[1]]


tt2$load(db = dbr)
pool::dbListTables(dbr)
tt2$data[[1]]

tt2$load(db = dbr, useName = "test")
tt2$data[[1]]
tt2$name


db_close(dbr)


dbr <- BBPersonalR::db_open("data/test2.sqlite")
tt <- createInfo(name =  "test", type = "infoDBVariable")
tt$add(data =  readData(dataFrame = mtcars))
tt$add(data =  readData(dataFrame = mtcars))
tt$name
tt$empty
tt$save(db = dbr, overwrite = T)
tt
tt$data[[1]]

tt2 <- createInfo(name =  "test2", type = "infoDBVariable")
iris2 <- iris
colnames(iris2) <- str_replace(colnames(iris2), pattern = "\\.", replacement = "_")
tt2$add(data = readData(dataFrame = iris2))
tt2
tt2$data[[1]]
tt2$save(db = dbr, overwrite = T)
tt2
tt2$data[[1]]


tt2$load(db = dbr)
pool::dbListTables(dbr)
tt2$data[[1]]
tt2

tt2$load(db = dbr, useName = "test")
tt2$data[[1]]
tt2$data[[2]]
tt2$name
tt2

