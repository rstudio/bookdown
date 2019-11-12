library(testit)

suppressWarnings(
  assert(
    'google sharing is disable',
    # no sharing field
    check_gb_config(list(search = TRUE)) %==%
      list(search = TRUE),
    # sharing is false => do nothing
    check_gb_config(list(search = TRUE, sharing = FALSE)) %==%
      list(search = TRUE, sharing = FALSE),
    # sharing is null => do nothing
    check_gb_config(list(search = TRUE, sharing = NULL)) %==%
      list(search = TRUE, sharing = NULL),
    # sharing is true => do nothing
    check_gb_config(list(search = TRUE, sharing = TRUE)) %==%
      list(search = TRUE, sharing = TRUE),
    # sharing has no google field => do nothing
    check_gb_config(list(sharing = list(facebook = TRUE))) %==%
      list(sharing = list(facebook = TRUE)),
    # google sharing is activated => remove field
    check_gb_config(list(sharing = list(facebook = TRUE, google = TRUE))) %==%
      list(sharing = list(facebook = TRUE)),
    # google sharing is deactivated => remove field
    check_gb_config(list(sharing = list(facebook = TRUE, google = FALSE))) %==%
      list(sharing = list(facebook = TRUE)),
    check_gb_config(list(sharing = list(facebook = TRUE, google = NULL))) %==%
      list(sharing = list(facebook = TRUE)),
    # google is activated through the all field => remove it
    check_gb_config(list(sharing = list(facebook = TRUE, all = c("google", "twitter", "linkedin")))) %==%
      list(sharing = list(facebook = TRUE, all = c("twitter", "linkedin")))
  )
)
