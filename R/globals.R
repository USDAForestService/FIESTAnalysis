utils::globalVariables(c(".SD", ".N", ".I",
                         "st_intersects", "sd", "CmbTable", "zone_ds", "count",
                         "RunningStats"))
utils::globalVariables(names(formals(FIESTA::savedata_options)[1:(length(formals(FIESTA::savedata_options))-1)]))