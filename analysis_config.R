# TIMSS 2023 Grade 8 dissertation analysis configuration
#
# All paths are interpreted relative to the project directory. The official
# Grade 8 SPSS or R files should be placed in data/raw without renaming them.

analysis_config <- list(
  input = list(
    raw_dir = file.path("data", "raw"),
    student_file_regex = "(?i)(^|[/\\\\])bsg[^/\\\\]*\\.(sav|rds|rda|rdata)$"
  ),
  output = list(
    dir = "outputs",
    analytic_file = "timss2023_g8_analytic.rds"
  ),
  sample = list(
    # NULL retains every Grade 8 education system found in the files.
    # To restrict the analysis, enter TIMSS numeric IDCNTRY values, for example:
    # jurisdictions = c(36, 158)
    jurisdictions = NULL,
    minimum_country_n = 200,
    maximum_predictor_missingness = 0.40
  ),
  analysis = list(
    # Equal-country weights prevent education systems with larger target
    # populations from dominating pooled preliminary estimates.
    equal_country_contribution = TRUE,
    # TIMSS 2023 uses JK2-full: 125 variance zones and two replicate
    # subsamples per zone, producing 250 replicate weights.
    jrr_variance_zones = 125,
    confidence_level = 0.95,
    run_country_specific_models = TRUE,
    run_pooled_fixed_effect_models = TRUE
  ),
  variables = list(
    identifiers = list(
      country = c("IDCNTRY"),
      school = c("IDSCHOOL"),
      class = c("IDCLASS"),
      student = c("IDSTUD")
    ),
    survey_design = list(
      total_weight = c("TOTWGT"),
      senate_weight = c("SENWGT"),
      house_weight = c("HOUWGT"),
      jk_zone = c("JKZONE"),
      jk_replicate = c("JKREP")
    ),
    outcomes = list(
      number = sprintf("BSMNUM%02d", 1:5),
      algebra = sprintf("BSMALG%02d", 1:5),
      geometry_measurement = sprintf("BSMGEO%02d", 1:5),
      data_probability = sprintf("BSMDAT%02d", 1:5)
    ),
    resources = list(
      home_educational_resources_scale = list(
        candidates = c("BSBGHER"),
        label_pattern = "home educational resources"
      ),
      home_educational_resources_category = list(
        candidates = c("BSDGHER"),
        label_pattern = "home educational resources"
      ),
      own_computer_or_tablet = list(
        candidates = c("BSBG05A"),
        label_pattern = "own (computer|tablet)|computer or tablet.*own"
      ),
      shared_computer_or_tablet = list(
        candidates = c("BSBG05B"),
        label_pattern = "shared (computer|tablet)|computer or tablet.*shared"
      ),
      smartphone = list(
        candidates = c("BSBG05C"),
        label_pattern = "smartphone"
      ),
      internet_access = list(
        candidates = c("BSBG05D"),
        label_pattern = "access to (the )?internet|internet access"
      ),
      schoolwork_textbook_materials = list(
        candidates = c("BSBG14A"),
        label_pattern = "internet.*(textbook|course materials)|(textbook|course materials).*internet"
      ),
      schoolwork_assignments = list(
        candidates = c("BSBG14B"),
        label_pattern = "internet.*assignments posted|assignments posted.*internet"
      ),
      schoolwork_collaboration = list(
        candidates = c("BSBG14C"),
        label_pattern = "internet.*collaborat|collaborat.*internet"
      ),
      schoolwork_teacher_questions = list(
        candidates = c("BSBG14D"),
        label_pattern = "internet.*ask.*teacher|ask.*teacher.*internet"
      ),
      schoolwork_information_tutorials = list(
        candidates = c("BSBG14E"),
        label_pattern = "internet.*(information|articles|tutorials)|(information|articles|tutorials).*internet"
      ),
      schoolwork_learning_games = list(
        candidates = c("BSBG14F"),
        label_pattern = "internet.*learning games|learning games.*internet"
      )
    ),
    controls = list(
      sex = list(
        candidates = c("ITSEX"),
        label_pattern = "sex of student|student sex|gender"
      ),
      age = list(
        candidates = c("BSDAGE"),
        label_pattern = "student age|age of student"
      ),
      language_of_test_at_home = list(
        candidates = c("BSBG03"),
        label_pattern = "language of test.*home|speak.*at home"
      ),
      books_at_home = list(
        candidates = c("BSBG04"),
        label_pattern = "books.*home|number of books"
      ),
      parent_a_education = list(
        candidates = c("BSBG07A", "BSBG06A"),
        label_pattern = "(parent|guardian|mother).*highest.*education|education.*(parent|guardian|mother)"
      ),
      parent_b_education = list(
        candidates = c("BSBG07B", "BSBG06B"),
        label_pattern = "(parent|guardian|father).*highest.*education|education.*(parent|guardian|father)"
      ),
      student_born_in_country = list(
        candidates = c("BSBG10A", "BSBG09A"),
        label_pattern = "student.*born in|were you born in"
      )
    )
  )
)
