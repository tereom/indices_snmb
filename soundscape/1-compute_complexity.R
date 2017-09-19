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
        samplerate = audible_files$media_info$samplerate, 
        cgl_id = audible_files$original_metadata$archivo_grabadora_filesystem$conglomerado_muestra_id
        ) %>% 
    filter(!is.na(rec_day_time), duration > 60)

# save(audible_info, file = "soundscape/datos_procesados/2017-09-12_audible_info.RData")

# Hay 391 fechas y horas con error
# Observations: 391
# Variables: 5
# $ mimetype     <chr> "audio/wav", "audio/wav", "audio/wav", "audio/wav", "audio...
# $ lustre_path  <chr> "/LUSTRE/sacmod/snmb_data/2504/2015_04/grabaciones_audible...
# $ rec_time     <chr> "[0:00:001]", "[0:00:002]", "[0:00:003]", "[0:00:004]", "[...
# $ rec_date     <chr> "[000-00-1]", "[000-00-2]", "[000-00-3]", "[000-00-4]", "[...
# $ rec_time_day <dttm> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...


### correr RSLURM
load("soundscape/datos_slurm/2017-09-15_complexity_vector.RData")
load("soundscape/datos_procesados/2017-09-12_audible_info.RData")

complexity_df <- data_frame(rec = names(complexity_vector), val = complexity_vector)

complexity <- complexity_df %>% 
    mutate(
        indice = str_extract(rec, "(?<=.)[^.]*$"), 
        lustre_path = str_extract(rec, ".*(?=[.])"), 
        ) %>% 
    select(-rec) %>% 
    group_by(lustre_path, indice) %>% 
    mutate(n_minute = row_number()) %>% 
    ungroup() %>% 
    spread(indice, val) %>% 
    left_join(audible_info, by = "lustre_path") %>% 
    mutate(
        day_time = rec_day_time + (n_minute - 1) * 60
        ) %>% 
    ungroup()

save(complexity, file = "soundscape/datos_procesados/2017-09-15_complexity.RData")

load("soundscape/datos_procesados/2017-09-15_complexity.RData")

ggplot(filter(complexity, cgl_id %in% 697:703)) +
    geom_line(aes(x = day_time, y = PE, group = lustre_path), alpha = 0.5) +
    facet_wrap(~cgl_id, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

filter(complexity, cgl_id %in% c(912, 697:701)) %>% 
    group_by(cgl_id) %>% 
    summarise(n_distinct(lustre_path))
    ggplot() +
        geom_line(aes(x = day_time, y = PE, group = lustre_path), alpha = 0.5) +
        facet_wrap(~cgl_id, scales = "free_x") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        scale_x_datetime(date_breaks = "4 hour", date_labels = "%I:%M %p")
            