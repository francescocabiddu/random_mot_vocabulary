# load libraries
lib <- c("plyr", "magrittr", "tidyverse", 
         "janitor", "fastmatch",
         "beepr")
lapply(lib, require, character.only = TRUE)
rm(lib)

#### Random maternal token samples ####
# Random tokens exclusion from each maternal stage to match children's number of tokens
# Children's tokens were matched because the models only give an output in word types
mot_token_samples <- list()

for (i in 1:10) {
  set.seed(i)
  
  mot_token_samples[[paste("df", i, sep = "_")]] <-
    mot_na_baby_section %>%
    (function(x) {
      x %>%
        left_join(., chi_na_id_sec %>% 
                    rename(baby = id) %>%
                    group_by(baby, section) %>% 
                    summarise(chi_n_tokens = n()),
                  by = c("baby", "section"))
    }) %>%
    (function(x) {
      matched_x <- x[0,]
      
      while(nrow(x) > 0) {
        x %<>%
          group_by(baby, section) %>%
          sample_frac(0.99, replace = FALSE) %>%
          ungroup() %>%
          group_by(baby, section) %>%
          mutate(mot_n_tokens = n()) %>%
          ungroup() %>%
          # small range given (less then 1% of tokens at a stage) to increase the match
          mutate(token_comparison = mot_n_tokens - chi_n_tokens <= 20)
        
        matched_x <- rbind(matched_x, x %>% filter(token_comparison == TRUE))
        
        x %<>%
          filter(token_comparison == FALSE)
      }
      
      matched_x
    }) %>%
    arrange(baby, section) %>%
    group_by(baby, section, chi_n_tokens, mot_n_tokens, token_comparison) %>%
    summarize(word = list(word), mor_raw = list(mor_raw)) %>%
    ungroup()
} ; rm(i)

#### syllabic, phonemic length ####
# list of 10 maternal samples of word types 
mot_uni_random <- lapply(rep("mot_uni", 10), function(x) {
  get(x) %>%
    select(baby, section, uni_diff, phon)
}) %>%
  (function(x) {
    names(x) <- paste("df", 1:10, sep = "_")
    x
  })

for (df_num in 1:10) {
  mot_uni_random[[paste("df", df_num, sep = "_")]] %<>%
    mutate(df_num = paste("df", df_num, sep = "_"))
}

# select only types in token random samples (by stage) 
for (df_num in 1:10) {
  for (id in unique(mot_uni$baby)) {
    for (stage in unique(mot_uni$section)) {
      mot_uni_random[[paste("df", df_num, sep = "_")]]$uni_diff[
        which(
          mot_uni_random[[paste("df", df_num, sep = "_")]]$baby == id &
            mot_uni_random[[paste("df", df_num, sep = "_")]]$section == stage
        )
        ][[1]] <-
        mot_uni_random[[paste("df", df_num, sep = "_")]]$uni_diff[
          which(
            mot_uni_random[[paste("df", df_num, sep = "_")]]$baby == id &
              mot_uni_random[[paste("df", df_num, sep = "_")]]$section == stage
          )
          ][[1]][
            mot_uni_random[[paste("df", df_num, sep = "_")]]$uni_diff[
              which(
                mot_uni_random[[paste("df", df_num, sep = "_")]]$baby == id &
                  mot_uni_random[[paste("df", df_num, sep = "_")]]$section == stage
              )
              ][[1]] %in%
              mot_token_samples[[paste("df", df_num, sep = "_")]]$word[
                which(
                  mot_token_samples[[paste("df", df_num, sep = "_")]]$baby == id &
                    mot_token_samples[[paste("df", df_num, sep = "_")]]$section == stage
                )
                ][[1]]
            ]
    }
  }
} ; rm(id, stage, df_num)

# select phonetic types from orthographic ones
for (df_num in 1:10) {
  for (row in seq_along(mot_uni$baby)) {
    mot_uni_random[[paste("df", df_num, sep = "_")]]$phon[[row]] <-
      mot_uni_random[[paste("df", df_num, sep = "_")]]$phon[[row]][
        which(
          names(mot_uni_random[[paste("df", df_num, sep = "_")]]$phon[[row]]) %in%
            mot_uni_random[[paste("df", df_num, sep = "_")]]$uni_diff[[row]]
        )
      ]
  }
} ; rm(df_num, row)

# types and cumulative types, syllabic and phonemic length 
mot_uni_random %<>%
  (function(df) {
    sapply(names(df), function(name) {
      df[[name]] %<>%
        mutate(n_uni = sapply(uni_diff, function(x) {
          length(x)
        })) %>%
        group_by(baby) %>%
        mutate(cum_uni = cumsum(n_uni)) %>%
        ungroup() %>%
        mutate(len = sapply(phon, function(x) {
          x %>% 
            str_split("_") %>%
            sapply(function(y) {y %in% vowels %>% sum()})
        } )) %>%
        mutate(len2 = sapply(phon, function(x) {
          x %>% 
            str_split("_") %>%
            sapply(length)
        })) %>%
        mutate(len_1 = sapply(len, function(x) {
          (x == 1) %>% sum()
        }),
        len_2 = sapply(len, function(x) {
          (x == 2) %>% sum()
        }),
        len_3 = sapply(len, function(x) {
          (x == 3) %>% sum()
        })) %>%
        group_by(baby) %>%
        mutate(len_1_cum = cumsum(len_1),
               len_2_cum = cumsum(len_2),
               len_3_cum = cumsum(len_3)) %>%
        ungroup() %>%
        mutate(len2_2 = sapply(len2, function(x) {
          (x == 2) %>% sum()
        }),
        len2_3 = sapply(len2, function(x) {
          (x == 3) %>% sum()
        }),
        len2_4 = sapply(len2, function(x) {
          (x == 4) %>% sum()
        }),
        len2_5 = sapply(len2, function(x) {
          (x == 5) %>% sum()
        }),
        len2_6 = sapply(len2, function(x) {
          (x == 6) %>% sum()
        }),
        len2_7 = sapply(len2, function(x) {
          (x == 7) %>% sum()
        }),
        len2_8 = sapply(len2, function(x) {
          (x == 8) %>% sum()
        })) %>%
        group_by(baby) %>%
        mutate(len2_2_cum = cumsum(len2_2),
               len2_3_cum = cumsum(len2_3),
               len2_4_cum = cumsum(len2_4),
               len2_5_cum = cumsum(len2_5),
               len2_6_cum = cumsum(len2_6),
               len2_7_cum = cumsum(len2_7),
               len2_8_cum = cumsum(len2_8)) %>%
        ungroup() %>%
        (function(x) {
          percentages <- x %>%
            select(baby, len_1_cum:len_3_cum) %>%
            adorn_percentages() %>%
            .[,-1] %>%
            (function(y) {
              colnames(y) <- gsub("_cum$", "_perc", colnames(y))
              y
            }) %>%
            {. * 100}
          
          x %>%
            ungroup() %>%
            mutate(len_1_perc = percentages$len_1_perc,
                   len_2_perc = percentages$len_2_perc,
                   len_3_perc = percentages$len_3_perc)
        }) %>%
        (function(x) {
          percentages <- x %>%
            select(baby, len2_2_cum:len2_8_cum) %>%
            adorn_percentages() %>%
            .[,-1] %>%
            (function(y) {
              colnames(y) <- gsub("_cum$", "_perc", colnames(y))
              y
            }) %>%
            {. * 100}
          
          x %>%
            ungroup() %>%
            mutate(len2_2_perc = percentages$len2_2_perc,
                   len2_3_perc = percentages$len2_3_perc,
                   len2_4_perc = percentages$len2_4_perc,
                   len2_5_perc = percentages$len2_5_perc,
                   len2_6_perc = percentages$len2_6_perc,
                   len2_7_perc = percentages$len2_7_perc,
                   len2_8_perc = percentages$len2_8_perc)
        })
    }, simplify = FALSE, USE.NAMES = TRUE)
  })

#### PP ####
# list of 10 maternal samples of word types 
mot_uni_on_subtlex_us_random <- lapply(rep("mot_uni_on_subtlex_us", 10), function(x) {
  get(x) %>%
    select(baby:phon)
}) %>%
  (function(x) {
    names(x) <- paste("df", 1:10, sep = "_")
    x
  })

for (df_num in 1:10) {
  mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]] %<>%
    mutate(df_num = paste("df", df_num, sep = "_"))
}

# select only types in token random samples (by stage) 
for (df_num in 1:10) {
  for (id in unique(mot_uni$baby)) {
    for (stage in unique(mot_uni$section)) {
      mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$phon[
        which(
          mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$baby == id &
            mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$section == stage
        )
        ][[1]] <-
        mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$phon[
          which(
            mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$baby == id &
              mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$section == stage
          )
          ][[1]][
            mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$phon[
              which(
                mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$baby == id &
                  mot_uni_on_subtlex_us_random[[paste("df", df_num, sep = "_")]]$section == stage
              )
              ][[1]] %in%
              mot_uni_random[[paste("df", df_num, sep = "_")]]$phon[
                which(
                  mot_uni_random[[paste("df", df_num, sep = "_")]]$baby == id &
                    mot_uni_random[[paste("df", df_num, sep = "_")]]$section == stage
                )
                ][[1]]
            ]
    }
  }
} ; rm(id, stage, df_num)

# assign pp and tertiles
mot_uni_on_subtlex_us_random %<>%
  sapply(function(df) {
    df_new <- df %>%
      assign_subtlex_us_pp() %>%
      quartiles_pp_sets()
    
    df_new
  }, simplify = FALSE, USE.NAMES = TRUE)






#### ND ####
mot_nei_on_random <- lapply(rep("mot_nei_on", 10), function(x) {
  get(x) %>%
    select(baby:phon)
}) %>%
  (function(x) {
    names(x) <- paste("df", 1:10, sep = "_")
    x
  })

for (df_num in 1:10) {
  mot_nei_on_random[[paste("df", df_num, sep = "_")]] %<>%
    mutate(df_num = paste("df", df_num, sep = "_"))
} ; rm(df_num)

# select only types in token random samples (by stage) 
for (df_num in 1:10) {
  for (id in unique(mot_uni$baby)) {
    for (stage in unique(mot_uni$section)) {
      mot_nei_on_random[[paste("df", df_num, sep = "_")]]$phon[
        which(
          mot_nei_on_random[[paste("df", df_num, sep = "_")]]$baby == id &
            mot_nei_on_random[[paste("df", df_num, sep = "_")]]$section == stage
        )
        ][[1]] <-
        mot_nei_on_random[[paste("df", df_num, sep = "_")]]$phon[
          which(
            mot_nei_on_random[[paste("df", df_num, sep = "_")]]$baby == id &
              mot_nei_on_random[[paste("df", df_num, sep = "_")]]$section == stage
          )
          ][[1]][
            mot_nei_on_random[[paste("df", df_num, sep = "_")]]$phon[
              which(
                mot_nei_on_random[[paste("df", df_num, sep = "_")]]$baby == id &
                  mot_nei_on_random[[paste("df", df_num, sep = "_")]]$section == stage
              )
              ][[1]] %in%
              mot_uni_random[[paste("df", df_num, sep = "_")]]$phon[
                which(
                  mot_uni_random[[paste("df", df_num, sep = "_")]]$baby == id &
                    mot_uni_random[[paste("df", df_num, sep = "_")]]$section == stage
                )
                ][[1]]
            ]
    }
  }
} ; rm(id, stage, df_num)

# assign nd and tertiles
mot_nei_on_random %<>%
  sapply(function(df) {
    df_new <- df %>%
      mutate(nei = sapply(phon, function(y) {
        mot_nei_on %>%
          (function(x) {
            tibble(phon = unlist(x$phon),
                   nei = unlist(x$nei))
          }) %>%
          (function(x) {
            x[!duplicated(x$phon), ]
          }) %>%
          (function(x) {
            tibble(phon = y) %>%
              left_join(x, by = "phon") %>%
              {.$nei}
          }) 
      })) %>%
      quartiles_nei_sets()
    
    df_new
  }, simplify = FALSE, USE.NAMES = TRUE)

#### Frequency ####
mot_uni_on_freq_random <- lapply(rep("mot_uni_on_freq", 10), function(x) {
  get(x) %>%
    select(baby:phon)
}) %>%
  (function(x) {
    names(x) <- paste("df", 1:10, sep = "_")
    x
  })

for (df_num in 1:10) {
  mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]] %<>%
    mutate(df_num = paste("df", df_num, sep = "_"))
} ; rm(df_num)

for (df_num in 1:10) {
  for (id in unique(mot_uni$baby)) {
    for (stage in unique(mot_uni$section)) {
      mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$phon[
        which(
          mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$baby == id &
            mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$section == stage
        )
        ][[1]] <-
        mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$phon[
          which(
            mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$baby == id &
              mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$section == stage
          )
          ][[1]][
            mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$phon[
              which(
                mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$baby == id &
                  mot_uni_on_freq_random[[paste("df", df_num, sep = "_")]]$section == stage
              )
              ][[1]] %in%
              mot_uni_random[[paste("df", df_num, sep = "_")]]$phon[
                which(
                  mot_uni_random[[paste("df", df_num, sep = "_")]]$baby == id &
                    mot_uni_random[[paste("df", df_num, sep = "_")]]$section == stage
                )
                ][[1]]
            ]
    }
  }
} ; rm(id, stage, df_num)

mot_uni_on_freq_random %<>%
  sapply(function(df) {
    df_new <- df %>%
      assign_freq() %>%
      quartiles_freq_sets()
    
    df_new
  }, simplify = FALSE, USE.NAMES = TRUE)

#### ####





  
