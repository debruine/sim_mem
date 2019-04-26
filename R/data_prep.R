library(dplyr)

stim_desc <- tribble(
  ~stim_id             , ~stim_type, ~stim_sex,
  "_bf_aliciakeys"     , "person", "female",
  "_bf_serenawilliams" , "person", "female",
  "_bm_neyo"           , "person", "male",  
  "_bm_nickcannon"     , "person", "male", 
  "_wf_jessicabiel"    , "person", "female",  
  "_wf_jessicasimpson" , "person", "female",
  "_wm_mattdamon"      , "person", "male", 
  "_wm_nicklachey"     , "person", "male", 
  "Count"              , "object", "male", 
  "Decimal"            , "object", "male",
  "Divide"             , "object", "male",
  "Essay"              , "object", "female",
  "Fraction"           , "object", "male",
  "Literature"         , "object", "female",
  "Math"               , "object", "male",
  "Multiply"           , "object", "male",
  "Number"             , "object", "male",
  "Paragraph"          , "object", "female",
  "Poetry"             , "object", "female",
  "Sentence"           , "object", "female",
  "Spelling"           , "object", "female",
  "Vocabulary"         , "object", "female"
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
    stim_type.e = recode(stim_type, "object" = 0.5, "person" = -0.5)
  ) %>%
  filter(rt > mean(rt) - 2*sd(rt),
         rt < mean(rt) + 2*sd(rt))

readr::write_csv(iat_data, "iat_data.csv")