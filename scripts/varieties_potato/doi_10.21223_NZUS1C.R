# R script for "carob"


carob_script <- function(path) {

"During the period 2020-2021, experiments were planted to study the phenotypic stability of tuber yield in thirty advanced clones of the B3C3 population, using the Row-Column statistical design with three replications of ten plants in each experiment. Amarilis, Canchan and Chucmarina var"
    
    uri <- "doi:10.21223/NZUS1C"
    group <- "varieties_potato"
    ff  <- carobiner::get_data(uri, path, group)
    
    meta <- carobiner::get_metadata(uri, path, group, major = 1, minor=1,
        data_organization = "CIP",
        publication = NA,
        project = NA,
        data_type = "experiment",
        treatment_vars = "variety",
        response_vars = "yield;yield_marketable",
        carob_contributor = "Henry Juarez",
        carob_effort = NA,
        carob_date = "2024-09-13",
        notes = NA
    )
    
    f <- ff[grep("PTYield", basename(ff))]
    
    r <- carobiner::read.excel(f[1], sheet = "Fieldbook")
    installation <- carobiner::read.excel(f[4], sheet = "Installation")
    
    n <- as.list(installation$Value)
    names(n) <- installation$Factor
    
    plot_size <- as.numeric(n$`Plot_size_(m2)`)
    plot_adj <- 10000 / plot_size
    
    df <- data.frame(
        rep = as.integer(r$REP),
        variety = r$INSTN,
        yield = as.numeric(r$TTWP) * plot_adj,
        yield_marketable = as.numeric(r$MTWP) * plot_adj,
        country = 'Peru',
        adm1 = 'Huamachuco',
        longitude = -78.042709,
        latitude = -7.834356,
        planting_date = "2020-11-23" ,
        harvest_date = "2021-05-10",
        plant_density = as.numeric(n$`Planting_density_(plants/Ha)`),
        row_spacing = as.numeric(n$`Distance_between_rows_(m)`) * 100,
        plant_spacing = as.numeric(n$`Distance_between_plants_(m)`) * 100,
        trial_id = gsub(".xls|.xlsx", "", basename(f[1]))
    )
    
    if (!is.null(r$AUDPC)) {
        df$AUDPC <- as.numeric(r$AUDPC) / 100
        df$pathogen <- "Phytophthora infestans"
    }
    if (!is.null(r$AUDPC)) {
        df$rAUDPC <- as.numeric(r$rAUDPC)
        df$pathogen <- "Phytophthora infestans"
    }
    
    
    df$on_farm <- TRUE
    df$is_survey <- FALSE
    df$irrigated <- FALSE
    df$treatment = "varieties"
    df$crop <- "potato"
    df$pathogen <- "Phytophthora infestans"
    df$yield_part <- "tubers"
    df$geo_from_source = FALSE
    df$N_fertilizer <- df$P_fertilizer <- df$K_fertilizer <- as.numeric(NA)
    
	carobiner::write_files(path = path, metadata = meta, wide=df)
    
}

