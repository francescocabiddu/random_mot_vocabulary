# load libraries
lib <- c("magrittr", "tidyverse", 
         "janitor", "fastmatch",
         "beepr")
lapply(lib, require, character.only = TRUE)
rm(lib)

# Random tokens exclusion from each maternal stage to match children's number of tokens
# Children's tokens were matched because the models only give an output in word types
mot_token_samples <- list()

for (i in 1:10) {
  set.seed(i)
  
  mot_token_samples[[paste("df", i, sep = "_")]] <-
    mot_na_baby_section %>%
    (function(x) {
      x %>%
        left_join(., mot_chi_na %>% 
                    filter(id == "CHI") %>% 
                    group_by(baby) %>% 
                    summarise(chi_n_tokens = string %>% 
                                unlist() %>% length()),
                  by = "baby")
    }) %>%
    (function(x) {
      matched_x <- x[0,]
      
      while(nrow(x) > 0) {
        x %<>%
          group_by(baby) %>%
          sample_frac(0.99, replace = FALSE) %>%
          ungroup() %>%
          group_by(baby) %>%
          mutate(mot_n_tokens = n()) %>%
          ungroup() %>%
          mutate(token_comparison = mot_n_tokens - chi_n_tokens <= 200)
        
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

# list of 10 maternal samples of word types 
mot_uni_random <- lapply(rep("mot_uni", 10), function(x) {
  get(x) %>%
    select(baby, section, uni_diff, phon)
}) %>%
  (function(x) {
    names(x) <- paste("df", 1:10, sep = "_")
    x
  })

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
  (function(x) {
    sapply(names(x), function(y) {
      x[[y]] %<>%
        mutate(n_uni = sapply(uni_diff, function(x) {
          length(x)
        })) %>%
        group_by(baby) %>%
        mutate(cum_uni = cumsum(n_uni)) %>%
        ungroup() %>%
        mutate(len = sapply(phon, function(y) {
          y %>% 
            str_split("_") %>%
            sapply(function(x) {x %in% vowels %>% sum()})
        } )) %>%
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
        (function(x) {
          percentages <- x %>%
            select(baby, len_1_cum:len_3_cum) %>%
            adorn_percentages() %>%
            .[,-1] %>%
            (function(y) {
              colnames(y) <- gsub("_cum$", "_perc", colnames(y))
              y
            }) %>%
            mutate(len_1_perc = len_1_perc*100,
                   len_2_perc = len_2_perc*100,
                   len_3_perc = len_3_perc*100)
          
          x %>%
            ungroup() %>%
            cbind(percentages)
        })
    }, simplify = FALSE, USE.NAMES = TRUE)
  })

beep()
