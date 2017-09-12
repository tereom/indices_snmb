library(tidyverse)
library(lubridate)
library(stringr)
library(mongolite)
library(tuneR)
library(statcomp)
library(profvis)

audio <- mongo("audible_media", url = "mongodb://200.12.166.165/audio")

audible_files <- audio$find(query='{"readable":true,"media_info.samplerate":{"$in":[24000,48000]}}')

audible_info <- audible_files %>% 
    select(mimetype, lustre_path, rec_time, rec_date) %>% 
    mutate(
        rec_day_time = ymd_hms(str_c(rec_date, rec_time)), 
        mac_path = str_replace(lustre_path, "LUSTRE", "Volumes"), 
        duration = audible_files$media_info$duration, 
        samplerate = audible_files$media_info$samplerate
        ) %>% 
    filter(!is.na(rec_day_time), duration > 60)

# save(audible_info, file = "2017-09-12_audible_info.RData")

# Hay 391 fechas y horas con error
# Observations: 391
# Variables: 5
# $ mimetype     <chr> "audio/wav", "audio/wav", "audio/wav", "audio/wav", "audio...
# $ lustre_path  <chr> "/LUSTRE/sacmod/snmb_data/2504/2015_04/grabaciones_audible...
# $ rec_time     <chr> "[0:00:001]", "[0:00:002]", "[0:00:003]", "[0:00:004]", "[...
# $ rec_date     <chr> "[000-00-1]", "[000-00-2]", "[000-00-3]", "[000-00-4]", "[...
# $ rec_time_day <dttm> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...

wave_complexity <- function(path_wave, seconds_partition = 60){
    sound_wave <- readWave(path_wave)
    # l_min: number of entries per partition
    l_min <- sound_wave@samp.rate * seconds_partition
    # number of complete minutes in recording
    n <- floor(length(sound_wave) / l_min)
    map(1:n, function(i) global_complexity(sound_wave@left[l_min * (i - 1) + 1 : l_min * i], ndemb = 6))
}

audible_complexity <- audible_info %>% 
    mutate(complexity = map(lustre_path, ~wave_complexity(.)))
