library(tuneR)
library(statcomp)
library(rslurm)

setwd("/LUSTRE/sacmod/indices_audio/complexity")

load("2017-09-12_audible_info.RData")

wave_complexity <- function(path_wave, seconds_partition = 60){
    sound_wave <- readWave(path_wave)
    # l_min: number of entries per partition
    l_min <- sound_wave@samp.rate * seconds_partition
    # number of complete minutes in recording
    n <- floor(length(sound_wave) / l_min)
    purrr::map(1:n, function(i) global_complexity(sound_wave@left[l_min * (i - 1) + 1 : l_min * i], ndemb = 6))
}

params_df <- dplyr::data_frame(path_wave = audible_info$lustre_path)

sjob <- slurm_apply(wave_complexity, params = params_df, jobname = "complexity_job_2", 
    nodes = 3, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "3", ntasks = "3"))
print_job_status(sjob)

### Read results
complexity_vector <- list.files("_rslurm_complexity_job_2", "results", 
    full.names = TRUE) %>% 
    map(readRDS) %>% unlist()

save(complexity_vector, file = "2017-09-15_complexity_vector.RData")
