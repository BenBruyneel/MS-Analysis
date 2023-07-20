
srcFiles <- list.files("R/", full.names = T)
purrr::walk(srcFiles, ~source(.x))

fn <- list.files(path = "/home/ben/Documents/Thermo/TKO-Example", "*.raw", full.names = T)
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



dbr <- BBPersonalR::db_open("data/test3.sqlite")
tt <- createInfo(name =  "test", type = "infoDatabase")
tt$add(data =  readData(dataFrame = mtcars))
tt$add(data =  readData(dataFrame = mtcars))
tt$name
tt$empty
tt$save(db = dbr, overwrite = T)
tt
tt$data[[1]]

tt2 <- createInfo(name =  "test2", type = "infoDatabase")
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


tt <- infoList$new(name = "test", names = c("mtcars", "iris","penguins"),
                   types = "infoDatabase")
tt
class(tt$item(1))
tt$item(1)$name
tt$item(1)$add(data = readData(dataFrame = mtcars))
tt$item("mtcars")$data[[1]]
tt
tt$contents

dbt <- db_open(fileName = "data/test4.sqlite")
tt$save(db = dbt)

pool::dbListTables(dbt)
tt$load(db = dbt)
tt

tt2 <- infoList$new(name = "test", names = c("mtcars", "iris","penguins"),
                    types = "infoDatabase")
tt2$load(db = dbt)
tt2
tt2$item(1)$data

msR <- MSInfo$new()
msR
msR$chromatograms
msR$spectra
msR$files

msR$files$add(data = fileInfoThermo(filename = fn))
msR
msR$item('files')

msR$chromatograms$add(data = readChromatogramThermo(filename = fn, type = "tic"))
msR$chromatograms$add(data = readChromatogramThermo(filename = fn, type = "bpc"))
msR
msR$item('chromatograms')
msR$chromatogram.plot()
msR$chromatogram.plot(id = 3)
msR$chromatogram.plot(id = 3, rtLimits = c(30,35))
ts <- (msR$files$item(1) %>% filter(StartTime > 33.3, StartTime < 33.8, MSOrder == "Ms"))$scan
msR$spectra$addMultiple(data = readMultipleSpectrumThermo(filename = fn, scan = ts), verbose = T)

msR$spectra$add(data = readSpectrumThermo(filename = fn, scan =1))
msR$spectra$info %>% tail()
msR$spectrum.plot(id = 35)

msR$chromatogram.plot(id = 3)
msR$chromatograms$addMultiple(data = readMultipleChromatogramsThermo(filename = fn, mz = c(371.1020, 574.3217, 501.7720, 597.3254, 403.2809, 352.7568, 478.2561)), verbose = T)
msR
msR$chromatogram.plot(id = 4)
msR$chromatogram.plot(id = 5)
msR$chromatogram.plot(id = 6)
msR$chromatogram.plot(id = 7)
msR$chromatogram.plot(id = 8)
msR$chromatogram.plot(id = 9)
msR$chromatogram.plot(id = 10)
msR$chromatogram.plot(id = 11)

msR$save(db = dbt)
pool::dbListTables(dbt)

msR2 <- MSInfo$new()
msR2$load(db = dbt)
msR2
msR2$spectra
msR2

msR2$spectra$info
