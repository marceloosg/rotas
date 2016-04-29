library(RJSONIO)

library(plyr)
routematrix=function (from, to, mode = c("driving", "walking", "bicycling", 
                                 "transit"), structure = c("legs", "route"), output = c("simple", 
                                                                                        "all"), alternatives = FALSE, messaging = FALSE, sensor = FALSE, 
              override_limit = FALSE) 
{
        if (is.numeric(from) && length(from) == 2) 
                from <- revgeocode(from)
        stopifnot(is.character(from))
        if (is.numeric(to) && length(to) == 2) 
                to <- revgeocode(to)
        stopifnot(is.character(to))
        mode <- match.arg(mode)
        structure <- match.arg(structure)
        output <- match.arg(output)
        stopifnot(is.logical(alternatives))
        stopifnot(is.logical(messaging))
        stopifnot(is.logical(sensor))
        origin <- paste(from,collapse="|")
        origin <- gsub(" ", "+", origin)
        origin <- paste("origins=", origin, sep = "")
        destination <- paste(to,collapse="|")
        destination <- gsub(" ", "+", destination)
        destination <- paste("destinations=", destination, sep = "")
        mode4url <- paste("mode=", mode, sep = "")
        unit4url <- paste("units=", "metric", sep = "")
        alts4url <- paste("alternatives=", tolower(as.character(alternatives)), 
                          sep = "")
        sensor4url <- paste("sensor=", tolower(as.character(sensor)), 
                            sep = "")
        posturl <- paste(origin, destination, mode4url, unit4url, 
                         alts4url, sensor4url, sep = "&")
        url_string <- paste("http://maps.googleapis.com/maps/api/distancematrix/json?", 
                            posturl, sep = "")
        url_string <- URLencode(url_string)
        #       check_route_query_limit(url_string, elems = 1, override = override_limit, 
        #                              messaging = messaging)
        if (messaging) 
                message("trying url ", url_string)
        connect <- url(url_string)
        tree <- fromJSON(paste(readLines(connect), collapse = ""))
        close(connect)
        if (output == "all") 
                return(tree)
        message(paste0("Information from URL : ", url_string))
        i=0
        out <- ldply(tree$rows, function(row) {
               route_df <- ldply(row$elements, function(elements) {
                       route_el=data.frame(
                               km = elements$distance$value/1000, 
                               minutes = elements$duration$value/60)
               }
               )
               route_df$j=1:length(row$elements)
               route_df
               })
        nrow=length(tree$rows)
        ncols=c()
        nrows=c()
        for(i in 1:nrow){ 
                ncol=length(tree$rows[[i]]$elements)
           #     ncols=c(ncols,1:ncol)
                nrows=c(nrows,rep(i,ncol))
        }
        out$i=nrows
       # out$j=ncols
        return(out)
}

                                   #, 
                                   #hours = oneLegList$duration$value/3600, 
                             #      startLon = oneLegList$start_location$lng, 
                              #     startLat = oneLegList$start_location$lat, endLon = oneLegList$end_location$lng, 
                               #    endLat = oneLegList$end_location$lat)
                #})
                #route_df$leg <- 1:nrow(route_df)
                
                #route_df
        #})
        #stepsPerRoute <- sapply(tree$routes, function(route) length(route$legs[[1]]$steps))
        #nRoutes <- length(stepsPerRoute)
        #routeLabel <- NULL
        #for (k in 1:nRoutes) {
        #        routeLabel <- c(routeLabel, rep(LETTERS[k], stepsPerRoute[k]))
        #}
        #if (nRoutes > 1) 
        #        out$route <- routeLabel
        #if (structure == "legs") {
        #        return(out)
        #}
        #else {
        #        return(legs2route(out))
        #}
#}


