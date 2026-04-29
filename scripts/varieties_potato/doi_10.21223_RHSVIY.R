# R script for "carob"

carob_script <- function(path) {
    
"Datasets from four experiments planted in Huancayo under normal conditions for potato crop and one experiment planted under high temperatures in order to determinate Parental value for yield in advanced clones of B3C3 population. In the field experiments, the randomized complete block design was used with 3 replications  and 40 plants."
    
    uri <- "doi:10.21223/RHSVIY"
    group <- "varieties_potato"
    ff  <- carobiner::get_data(uri, path, group)

    meta <- carobiner::get_metadata(uri, path, group, major=5, minor=0,
        data_organization = "CIP",
        publication = NA,
        project = NA,
        data_type = "experiment",
        treatment_vars = "variety",
        response_vars = "yield;yield_marketable", 
        carob_contributor = "Henry Juarez",
        carob_date = "2024-09-11",
        notes = NA
    )
    
    process <- carobiner::get_function("process_cip_lbvars", path, group)
    
    f <- ff[grep("PTYield", basename(ff))]
    f <- f[grep("^0._|_Data_dictionary", basename(f), invert=TRUE)]
    d <- lapply(f, process, addvars=c("AUDPC", "rAUDPC"))
    d <- do.call(rbind, d)
 
    carobiner::write_files(path = path, metad = meta, wide=d)
    
}

