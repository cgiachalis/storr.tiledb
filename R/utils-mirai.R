enable_mirai <- function(n = 2L) {
  mirai::daemons(n, .compute = .storr_profile)
}

disable_mirai <- function() {
  mirai::daemons(0L, .compute = .storr_profile)
}

mirai_stats <- function() {
  mirai::info(.storr_profile)
}
