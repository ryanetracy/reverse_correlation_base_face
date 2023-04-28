######################################################################
# assessing the baseline perceptions of base faces used in reverse 
# correlation research studies

# faces are morphed as a combination of white females, black females, 
# white males, black males, biracial males and females, and a racially
# and sexually ambiguous face (white/black male/female combination)
######################################################################

# package installation and load
pckgs <- c('rstatix',
           'tidyverse')

for (p in pckgs) {
  if (! (p %in% installed.packages())) {
    install.packages(p)
  }
  lapply(p, library, character.only = T)
}


# data
df <- read.csv('base_face_ratings_raw.csv')

colnames(df)

df <- df %>% filter(Status != 1)

# demographics
df %>%
  get_summary_stats(age, type = 'mean_sd')

df %>%
  select(race) %>%
  count(race) %>%
  mutate(
    proportion = round(
      100 * (n / sum(n)), 2
    ),
    race = case_when(
      race == 1 ~ 'white',
      race == 2 ~ 'black',
      race == 8 ~ 'biracial',
      TRUE ~ 'no response'
    )
  )

df %>%
  select(gender) %>%
  count(gender) %>%
  mutate(
    proportion = round(
      100 * (n / sum(n)), 2
    ),
    gender = case_when(
      gender == 1 ~ 'male',
      gender == 2 ~ 'female',
      gender == 3 ~ 'non-binary',
      TRUE ~ 'no response'
    )
  )


df_1 <- df %>%
  select(ResponseId,
         OG_BI_male_afro_1:Ambig_traits_DO) %>%
  select(-contains('_DO'))

new_names <- paste0(
  rep(
    c('OG_biracial_male',
      'new_biracial_male',
      'new_biracial_female',
      'new_black_female',
      'new_white_female',
      'new_black_male',
      'new_white_male',
      'ambiguous_race_sex'),
    each = 9
  ),
  
  rep('__'),
  
  rep(
    c('afrocentricity',
      'skin_tone',
      'masculinity',
      'attractive',
      'trustworthy',
      'dominant',
      'intelligent',
      'warm',
      'competent')
  )
)

names(df_1)[2:73] <- new_names

# run some analyses
# really just looking to confirm how close/far the means are to scale midpoints

df_long <- df_1 %>%
  pivot_longer(
    cols = OG_biracial_male__afrocentricity:ambiguous_race_sex__competent,
    names_to = c('base_face', 'trait'),
    values_to = 'rating',
    names_sep = '__'
  ) %>% 
  pivot_wider(
    names_from = 'trait',
    values_from = 'rating'
  )

# afrocentricity
t.test(df_long$afrocentricity[df_long$base_face == 'OG_biracial_male'],
       mu = 50)

t.test(df_long$afrocentricity[df_long$base_face == 'new_biracial_male'],
       mu = 50)

t.test(df_long$afrocentricity[df_long$base_face == 'new_biracial_female'],
       mu = 50)

t.test(df_long$afrocentricity[df_long$base_face == 'new_black_female'],
       mu = 50)

t.test(df_long$afrocentricity[df_long$base_face == 'new_white_female'],
       mu = 50)

t.test(df_long$afrocentricity[df_long$base_face == 'new_black_male'],
       mu = 50)

t.test(df_long$afrocentricity[df_long$base_face == 'new_white_male'],
       mu = 50)

t.test(df_long$afrocentricity[df_long$base_face == 'ambiguous_race_sex'],
       mu = 50)


# skin tone
t.test(df_long$skin_tone[df_long$base_face == 'OG_biracial_male'],
       mu = 50)

t.test(df_long$skin_tone[df_long$base_face == 'new_biracial_male'],
       mu = 50)

t.test(df_long$skin_tone[df_long$base_face == 'new_biracial_female'],
       mu = 50)

t.test(df_long$skin_tone[df_long$base_face == 'new_black_female'],
       mu = 50)

t.test(df_long$skin_tone[df_long$base_face == 'new_white_female'],
       mu = 50)

t.test(df_long$skin_tone[df_long$base_face == 'new_black_male'],
       mu = 50)

t.test(df_long$skin_tone[df_long$base_face == 'new_white_male'],
       mu = 50)

t.test(df_long$skin_tone[df_long$base_face == 'ambiguous_race_sex'],
       mu = 50)


# masculinity
t.test(df_long$masculinity[df_long$base_face == 'OG_biracial_male'],
       mu = 50)

t.test(df_long$masculinity[df_long$base_face == 'new_biracial_male'],
       mu = 50)

t.test(df_long$masculinity[df_long$base_face == 'new_biracial_female'],
       mu = 50)

t.test(df_long$masculinity[df_long$base_face == 'new_black_female'],
       mu = 50)

t.test(df_long$masculinity[df_long$base_face == 'new_white_female'],
       mu = 50)

t.test(df_long$masculinity[df_long$base_face == 'new_black_male'],
       mu = 50)

t.test(df_long$masculinity[df_long$base_face == 'new_white_male'],
       mu = 50)

t.test(df_long$masculinity[df_long$base_face == 'ambiguous_race_sex'],
       mu = 50)


# attractive
t.test(df_long$attractive[df_long$base_face == 'OG_biracial_male'],
       mu = 4)

t.test(df_long$attractive[df_long$base_face == 'new_biracial_male'],
       mu = 4)

t.test(df_long$attractive[df_long$base_face == 'new_biracial_female'],
       mu = 4)

t.test(df_long$attractive[df_long$base_face == 'new_black_female'],
       mu = 4)

t.test(df_long$attractive[df_long$base_face == 'new_white_female'],
       mu = 4)

t.test(df_long$attractive[df_long$base_face == 'new_black_male'],
       mu = 4)

t.test(df_long$attractive[df_long$base_face == 'new_white_male'],
       mu = 4)

t.test(df_long$attractive[df_long$base_face == 'ambiguous_race_sex'],
       mu = 4)


# trustworthy
t.test(df_long$trustworthy[df_long$base_face == 'OG_biracial_male'],
       mu = 4)

t.test(df_long$trustworthy[df_long$base_face == 'new_biracial_male'],
       mu = 4)

t.test(df_long$trustworthy[df_long$base_face == 'new_biracial_female'],
       mu = 4)

t.test(df_long$trustworthy[df_long$base_face == 'new_black_female'],
       mu = 4)

t.test(df_long$trustworthy[df_long$base_face == 'new_white_female'],
       mu = 4)

t.test(df_long$trustworthy[df_long$base_face == 'new_black_male'],
       mu = 4)

t.test(df_long$trustworthy[df_long$base_face == 'new_white_male'],
       mu = 4)

t.test(df_long$trustworthy[df_long$base_face == 'ambiguous_race_sex'],
       mu = 4)


# dominant
t.test(df_long$dominant[df_long$base_face == 'OG_biracial_male'],
       mu = 4)

t.test(df_long$dominant[df_long$base_face == 'new_biracial_male'],
       mu = 4)

t.test(df_long$dominant[df_long$base_face == 'new_biracial_female'],
       mu = 4)

t.test(df_long$dominant[df_long$base_face == 'new_black_female'],
       mu = 4)

t.test(df_long$dominant[df_long$base_face == 'new_white_female'],
       mu = 4)

t.test(df_long$dominant[df_long$base_face == 'new_black_male'],
       mu = 4)

t.test(df_long$dominant[df_long$base_face == 'new_white_male'],
       mu = 4)

t.test(df_long$dominant[df_long$base_face == 'ambiguous_race_sex'],
       mu = 4)


# intelligent
t.test(df_long$intelligent[df_long$base_face == 'OG_biracial_male'],
       mu = 4)

t.test(df_long$intelligent[df_long$base_face == 'new_biracial_male'],
       mu = 4)

t.test(df_long$intelligent[df_long$base_face == 'new_biracial_female'],
       mu = 4)

t.test(df_long$intelligent[df_long$base_face == 'new_black_female'],
       mu = 4)

t.test(df_long$intelligent[df_long$base_face == 'new_white_female'],
       mu = 4)

t.test(df_long$intelligent[df_long$base_face == 'new_black_male'],
       mu = 4)

t.test(df_long$intelligent[df_long$base_face == 'new_white_male'],
       mu = 4)

t.test(df_long$intelligent[df_long$base_face == 'ambiguous_race_sex'],
       mu = 4)


# warm
t.test(df_long$warm[df_long$base_face == 'OG_biracial_male'],
       mu = 4)

t.test(df_long$warm[df_long$base_face == 'new_biracial_male'],
       mu = 4)

t.test(df_long$warm[df_long$base_face == 'new_biracial_female'],
       mu = 4)

t.test(df_long$warm[df_long$base_face == 'new_black_female'],
       mu = 4)

t.test(df_long$warm[df_long$base_face == 'new_white_female'],
       mu = 4)

t.test(df_long$warm[df_long$base_face == 'new_black_male'],
       mu = 4)

t.test(df_long$warm[df_long$base_face == 'new_white_male'],
       mu = 4)

t.test(df_long$warm[df_long$base_face == 'ambiguous_race_sex'],
       mu = 4)


# competent
t.test(df_long$competent[df_long$base_face == 'OG_biracial_male'],
       mu = 4)

t.test(df_long$competent[df_long$base_face == 'new_biracial_male'],
       mu = 4)

t.test(df_long$competent[df_long$base_face == 'new_biracial_female'],
       mu = 4)

t.test(df_long$competent[df_long$base_face == 'new_black_female'],
       mu = 4)

t.test(df_long$competent[df_long$base_face == 'new_white_female'],
       mu = 4)

t.test(df_long$competent[df_long$base_face == 'new_black_male'],
       mu = 4)

t.test(df_long$competent[df_long$base_face == 'new_white_male'],
       mu = 4)

t.test(df_long$competent[df_long$base_face == 'ambiguous_race_sex'],
       mu = 4)



# write out a final dataframe that summarizes the mean ratings for each face
# this will be the data for comparisons to CIs generated in reverse correlation
df_final <- df_1 %>%
  pivot_longer(
    cols = OG_biracial_male__afrocentricity:ambiguous_race_sex__competent,
    names_to = c('base_face', 'trait'),
    values_to = 'rating',
    names_sep = '__'
  ) %>% 
  group_by(base_face, trait) %>%
  summarize(
    rating = mean(rating, na.rm = T)
  ) %>%
  pivot_wider(
    names_from = 'trait',
    values_from = 'rating'
  )


# write.csv(df_final, 'base_face_norming_data.csv', row.names = F)
