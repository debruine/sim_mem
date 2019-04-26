library(dplyr)

stim_desc <- tribble(
  ~stim_id             , ~stim_type, ~stim_sex,
  "_bf_aliciakeys"     , "face", "female",
  "_bf_serenawilliams" , "face", "female",
  "_bm_neyo"           , "face", "male",  
  "_bm_nickcannon"     , "face", "male", 
  "_wf_jessicabiel"    , "face", "female",  
  "_wf_jessicasimpson" , "face", "female",
  "_wm_mattdamon"      , "face", "male", 
  "_wm_nicklachey"     , "face", "male", 
  "Count"              , "word", "male", 
  "Decimal"            , "word", "male",
  "Divide"             , "word", "male",
  "Essay"              , "word", "female",
  "Fraction"           , "word", "male",
  "Literature"         , "word", "female",
  "Math"               , "word", "male",
  "Multiply"           , "word", "male",
  "Number"             , "word", "male",
  "Paragraph"          , "word", "female",
  "Poetry"             , "word", "female",
  "Sentence"           , "word", "female",
  "Spelling"           , "word", "female",
  "Vocabulary"         , "word", "female"
)

iat_data <- IAT::IATData %>%
  filter(TRIAL_ERROR == 0, ATTEMPT == 1) %>%
  filter(BLOCK_PAIRING_DEFINITION_S %in% c("Math/Male,Language Arts/Female", 
                                           "Language Arts/Male,Math/Female")) %>%
  mutate(
    condition = recode(
      BLOCK_PAIRING_DEFINITION_S, 
      "Math/Male,Language Arts/Female" = "congruent",
      "Language Arts/Male,Math/Female" = "incongruent"
    ),
    TRIAL_NAME_S = gsub("(fpi|\\.jpg)", "", TRIAL_NAME_S)
  ) %>%
  select(
    sub_id = SESSION_ID,
    stim_id = TRIAL_NAME_S,
    condition,
    rt = TRIAL_LATENCY
  ) %>%
  left_join(stim_desc, by = "stim_id") %>%
  mutate(
    condition.e = recode(condition, "incongruent" = 0.5, "congruent" = -0.5),
    stim_sex.e = recode(stim_sex, "male" = 0.5, "female" = -0.5),
    stim_type.e = recode(stim_type, "word" = 0.5, "face" = -0.5)
  ) %>%
  filter(rt > mean(rt) - 2*sd(rt),
         rt < mean(rt) + 2*sd(rt))

readr::write_csv(iat_data, "iat_data.csv")
