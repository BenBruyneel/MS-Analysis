
createInfo <- function(type = "infoDatabase", name = ""){
        switch(type,
               "rds" = info$new(name = name),
               "infoDB" = infoDB$new(name = name),
               "infoDBVariable" = infoDBVariable$new(name = name),
               "infoDatabase" = infoDatabase$new(name = name),
               NA
        )
}

