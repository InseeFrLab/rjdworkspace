#' @importFrom stats frequency
#'
ts_r2jd <- function(s) {
    freq <- frequency(s)
    start <- start(s)
    jd_freq <- .jcall(
        obj = "ec/tstoolkit/timeseries/simplets/TsFrequency",
        returnSig = "Lec/tstoolkit/timeseries/simplets/TsFrequency;",
        method = "valueOf",
        as.integer(freq)
    )
    jd_period <- .jnew(
        class = "ec/tstoolkit/timeseries/simplets/TsPeriod",
        jd_freq, as.integer(start[1L]), as.integer(start[2L] - 1L)
    )
    ts <- .jnew(class = "ec/tstoolkit/timeseries/simplets/TsData",
                jd_period, as.double(s), FALSE)
    return(ts)
}


builder_from_sa <- function(sa_def,
                            ts,
                            pointSpec,
                            estimationSpec,
                            domainSpec,
                            estimationPolicy,
                            status,
                            priority,
                            quality,
                            warnings,
                            name,
                            metaData) {
    builder_sa <- sa_def$toBuilder()

    if (!missing(ts)) {
        builder_sa$ts(ts)
    } else {
        builder_sa$ts(sa_def$getTs())
    }

    if (!missing(pointSpec)) {
        builder_sa$pointSpec(pointSpec)
    } else {
        builder_sa$pointSpec(sa_def$getPointSpec())
    }

    if (!missing(estimationSpec)) {
        builder_sa$estimationSpec(estimationSpec)
    } else {
        builder_sa$estimationSpec(sa_def$getEstimationSpec())
    }

    if (!missing(domainSpec)) {
        builder_sa$domainSpec(domainSpec)
    } else {
        builder_sa$domainSpec(sa_def$getDomainSpec())
    }

    if (!missing(estimationPolicy)) {
        builder_sa$estimationPolicy(estimationPolicy)
    } else {
        builder_sa$estimationPolicy(sa_def$getEstimationPolicy())
    }

    if (!missing(status)) {
        builder_sa$status(status)
    } else {
        builder_sa$status(sa_def$getStatus())
    }

    if (!missing(priority)) {
        builder_sa$priority(priority)
    } else {
        builder_sa$priority(sa_def$getPriority())
    }

    if (!missing(quality)) {
        builder_sa$quality(quality)
    } else {
        builder_sa$quality(sa_def$getQuality())
    }

    if (!missing(warnings)) {
        builder_sa$warnings(warnings)
    } else {
        builder_sa$warnings(sa_def$getWarnings())
    }

    if (!missing(name)) {
        builder_sa$name(name)
    } else {
        builder_sa$name(sa_def$getName())
    }

    if (!missing(metaData)) {
        builder_sa$metaData(metaData)
    } else {
        builder_sa$metaData(sa_def$getMetaData())
    }
    return(builder_sa$build())
}


builder_from_ts <- function(jts,
                            data,
                            metaData,
                            moniker,
                            name,
                            type) {
    builder_ts <- jts$toBuilder()

    if (!missing(data)) {
        builder_ts$data(data)
    } else {
        builder_ts$data(jts$getData())
    }
    if (!missing(metaData)) {
        builder_ts$metaData(metaData)
    } else {
        builder_ts$metaData(jts$getMetaData())
    }
    if (!missing(moniker)) {
        builder_ts$moniker(moniker)
    } else {
        builder_ts$moniker(jts$getMoniker())
    }
    if (!missing(name)) {
        builder_ts$name(name)
    } else {
        builder_ts$name(jts$getName())
    }
    if (!missing(type)) {
        builder_ts$type(type)
    } else {
        builder_ts$type(jts$getType())
    }
    return(builder_ts$build())
}


clone_ws <- function(ws1) {
    if (!inherits(ws1, "workspace")) stop("The argument must be a workspace")
    # Clone initialisation
    clone <- new_workspace()
    # SAP extraction
    saps <- get_all_objects(ws1)
    # series extraction and addition to the clone
    for (sap in saps) {
        # sap <- saps[[1]]
        nom_sap <- get_name(sap)
        new_multiprocessing(clone, nom_sap)
        # a <- get_all_objects(clone)
        series <- get_all_objects(sap)
        lapply(series, function(s) {
            add_sa_item(clone, nom_sap, get_model(s, ws1), get_name(s))
            # names(series) == names(get_all_objects(sap))
        })
    }
    return(clone)
}
