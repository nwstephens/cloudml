library(tensorflow)
tf$logging$set_verbosity(tf$logging$INFO)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Extract often-used layer components
layers         <- tf$contrib$layers
learn          <- tf$contrib$learn
input_fn_utils <- tf$contrib$learn$python$learn$utils$input_fn_utils

CSV_COLUMNS <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race", "gender",
  "capital_gain", "capital_loss", "hours_per_week", "native_country",
  "income_bracket"
)

LABEL_COLUMN <- "income_bracket"

DEFAULTS <- lapply(
  list(0, "", 0, "", 0, "", "", "", "", "", 0, 0, 0, "", ""),
  list
)

INPUT_COLUMNS <- list(

  gender = layers$sparse_column_with_keys(
    column_name = "gender",
    keys = c("female", "male")
  ),

  race = layers$sparse_column_with_keys(
    column_name = "race",
    keys = c(
      "Amer-Indian-Eskimo",
      "Asian-Pac-Islander",
      "Black",
      "Other",
      "White"
    )
  ),

  education      = layers$sparse_column_with_hash_bucket("education", 1000L),
  marital_status = layers$sparse_column_with_hash_bucket("marital_status", 100L),
  relationship   = layers$sparse_column_with_hash_bucket("relationship", 100L),
  workclass      = layers$sparse_column_with_hash_bucket("workclass", 100L),
  occupation     = layers$sparse_column_with_hash_bucket("occupation", 1000L),
  native_country = layers$sparse_column_with_hash_bucket("native_country", 1000L),

  # Continuous base columns.
  age            = layers$real_valued_column("age"),
  education_num  = layers$real_valued_column("education_num"),
  capital_gain   = layers$real_valued_column("capital_gain"),
  capital_loss   = layers$real_valued_column("capital_loss"),
  hours_per_week = layers$real_valued_column("hours_per_week")
)

build_estimator <- function(model_dir,
                            embedding_size = 8L,
                            hidden_units = NULL)
{
  # Attach input columns (for easy access)
  list2env(INPUT_COLUMNS, envir = environment())

  age_buckets <- layers$bucketized_column(
    age,
    boundaries = c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65)
  )

  wide_columns <- list(

    layers$crossed_column(
      list(education, occupation),
      hash_bucket_size = 1E4L
    ),

    layers$crossed_column(
      list(age_buckets, race, occupation),
      hash_bucket_size = 1E6L
    ),

    layers$crossed_column(
      list(native_country, occupation),
      hash_bucket_size = 1E4L
    ),

    gender,
    native_country,
    education,
    occupation,
    workclass,
    marital_status,
    relationship,
    age_buckets
  )

  deep_columns <- list(
    layers$embedding_column(workclass, dimension = embedding_size),
    layers$embedding_column(education, dimension = embedding_size),
    layers$embedding_column(marital_status, dimension = embedding_size),
    layers$embedding_column(gender, dimension = embedding_size),
    layers$embedding_column(relationship, dimension = embedding_size),
    layers$embedding_column(race, dimension = embedding_size),
    layers$embedding_column(native_country, dimension = embedding_size),
    layers$embedding_column(occupation, dimension = embedding_size),
    age,
    education_num,
    capital_gain,
    capital_loss,
    hours_per_week
  )

  learn$DNNLinearCombinedClassifier(
    model_dir = model_dir,
    linear_feature_columns = wide_columns,
    dnn_feature_columns = deep_columns,
    dnn_hidden_units = hidden_units %||% c(100, 70, 50, 25)
  )
}

is_sparse <- function(column) {
  inherits(column, "tensorflow.contrib.layers.python.layers.feature_column._SparseColumn")
}

feature_columns_to_placeholders <- function(feature_columns,
                                            default_batch_size = NULL)
{
  # create a dictionary mapping feature column names to placeholders
  placeholders <- lapply(feature_columns, function(column) {
    tf$placeholder(
      if (is_sparse(column)) tf$string else tf$float32,
      list(default_batch_size)
    )
  })

  keys <- vapply(feature_columns, function(column) {
    column$name
  }, character(1))

  names(placeholders) <- keys

  placeholders
}

serving_input_fn <- function() {

  feature_placeholders <- feature_columns_to_placeholders(INPUT_COLUMNS)

  features <- lapply(feature_placeholders, function(tensor) {
    tf$expand_dims(tensor, -1L)
  })

  input_fn_utils$InputFnOps(
    features,
    NULL,
    feature_placeholders
  )
}

generate_input_fn <- function(filename,
                              num_epochs = NULL,
                              batch_size = 40L)
{
  input_fn <- function() {

    filename_queue <- tf$train$string_input_producer(
      list(filename),
      num_epochs = num_epochs
    )

    reader <- tf$TextLineReader()
    tensors <- reader$read_up_to(filename_queue, num_records = batch_size)
    value_column <- tf$expand_dims(tensors$values, -1L)

    columns <- tf$decode_csv(value_column, record_defaults = DEFAULTS)
    names(columns) <- CSV_COLUMNS
    features <- columns

    features$fnlwgt <- NULL
    income_int <- tf$to_int32(tf$equal(features[[LABEL_COLUMN]], " >50K"))
    list(features, income_int)
  }

  input_fn
}
