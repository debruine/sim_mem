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

blocks <- c("Math/Male,Language Arts/Female", 
            "Language Arts/Male,Math/Female"),
            "Math/Female,Language Arts/Male",
            "Language Arts/Female,Math/Male")

iat_data <- IAT::IATData %>%
  filter(TRIAL_ERROR == 0, ATTEMPT == 1) %>%
  filter(BLOCK_PAIRING_DEFINITION_S %in% blocks) %>%
  mutate(
    condition = recode(
      BLOCK_PAIRING_DEFINITION_S, 
      "Math/Male,Language Arts/Female" = "congruent",
      "Language Arts/Male,Math/Female" = "incongruent",
      "Math/Female,Language Arts/Male" = "incongruent",
      "Language Arts/Female,Math/Male" = "congruent"
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
  filter(rt > mean(rt) - 2*sd(rt),
         rt < mean(rt) + 2*sd(rt))

readr::write_csv(iat_data, "iat_data.csv")
