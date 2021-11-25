extract_sexbehav_phia <- function(ind, survey_id) {
  #' All of the sexual behaviour variables we're interested in

  #' From LePHIA 2016 - 2017 Adult Questionnaire
  sb_vars <- c(
    "firstsxage", #' Age at first vaginal sex
    "firstsxagedk", #' Age at first vaginal sex (don't know)
    "analsxever", #' Age at first anal sex
    "lifetimesex", #' Total sexual partners (lifetime)
    "lifetimesexdk", #' Total sexual partners (lifetime) (don't know)
    "part12monum", #' Total sexual partners (past 12 months)
    "part12modkr", #' Total sexual partners (past 12 months) (don't know)
    paste0("partlivew", 1:3), #' Does partner i live in this household
    paste0("partrelation", 1:3), #' Relationship to partner i
    paste0("partlastsup", 1:3), #' Expectation of gifts, payment, other help with partner i
    paste0("partlastsxtimed", 1:3), #' How long since last sex with partner i
    "sellsx12mo", #' Had sex for money, gifts during past 12 months
    "buysx12mo" #' Paid money or given gifts for sex during past 12 months
  )

  ind %>%
    mutate(
      survey_id = survey_id,
      individual_id = personid
    ) %>%
    select(survey_id, individual_id, all_of(sb_vars)) %>%
    mutate(
      #' Reports sexual activity in the last 12 months
      sex12m = case_when(
        (is.na(firstsxage) & (firstsxagedk %in% c(96, -7))) & (analsxever %in% c(2, -8, -9)) ~ FALSE, #' 96 is code for no sex
        part12monum > 0 ~ TRUE,
        part12modkr == -8 ~ TRUE, #' If don't know number of partners, assume > 1
        TRUE ~ FALSE
      ),
      #' Does not report sexual activity in the last 12 months
      nosex12m = case_when(
        sex12m == TRUE ~ FALSE,
        sex12m == FALSE ~ TRUE,
        is.na(sex12m) ~ NA
      ),
      #' Reports sexual activity with exactly one cohabiting partner in the past 12 months
      sexcohab = case_when(
        sex12m == FALSE ~ FALSE,
        (part12monum == 1) & (partlivew1 == 1) ~ TRUE,
        TRUE ~ FALSE
      ),
      #' Reports one or more non-regular sexual partner
      sexnonreg = case_when(
        nosex12m == TRUE ~ FALSE,
        part12monum > 1 ~ TRUE,
        (part12monum == 1) & (partlivew1 == 2) ~ TRUE,
        TRUE ~ FALSE
      ),
      #' Reports having exchanged gifts, cash, or anything else for sex in the past 12 months
      sexpaid12m = case_when(
        sellsx12mo == 1 ~ TRUE,
        buysx12mo == 1 ~ TRUE,
        TRUE ~ FALSE
      ),
      #' Either sexnonreg or sexpaid12m
      sexnonregplus = case_when(
        sexnonreg == TRUE ~ TRUE,
        sexpaid12m == TRUE ~ TRUE,
        TRUE ~ FALSE
      ),
      #' Just want the highest risk category that an individual belongs to
      nosex12m = ifelse(sexcohab | sexnonreg | sexpaid12m, FALSE, nosex12m),
      sexcohab = ifelse(sexnonreg | sexpaid12m, FALSE, sexcohab),
      sexnonreg = ifelse(sexpaid12m, FALSE, sexnonreg),
      #' Turn everything from TRUE / FALSE coding to 1 / 0
      across(sex12m:sexnonregplus, ~ as.numeric(.x))
    ) %>%
    select(-all_of(sb_vars))
}

check_survey_sexbehav <- function(survey_sexbehav) {
  df <- survey_sexbehav %>%
    mutate(
      r_tot = nosex12m + sexcohab + sexnonreg + sexpaid12m
    )

  cat(
    paste0(
      "The proportion of rows allocatated to one and only one category is ",
      round(sum(df$r_tot == 1) / nrow(df), 3) * 100, "%.\n",
      "The following rows are incorrectly allocated to multiple, or no, categories:\n"
    )
  )

  df %>%
    filter(r_tot != 1)
}
