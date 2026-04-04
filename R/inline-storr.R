
storr_traits <- utils::getFromNamespace("storr_traits", "storr")
make_hash_serialized_object <- utils::getFromNamespace("make_hash_serialized_object", "storr")
make_serialize_object <- utils::getFromNamespace("make_serialize_object", "storr")

# source: https://github.com/richfitz/storr/blob/master/R/exceptions.R
KeyError <- function(key, namespace) {
  structure(list(key = key,
                 namespace = namespace,
                 message = sprintf("key '%s' ('%s') not found", key, namespace),
                 call = NULL),
            class = c("KeyError", "error", "condition"))
}

HashError <- function(hash) {
  structure(list(hash = hash,
                 message = sprintf("hash '%s' not found", hash),
                 call = NULL),
            class = c("HashError", "error", "condition"))
}


ConfigError <- function(name, prev, requested) {
  msg <- sprintf("Incompatible value for %s (existing: %s, requested: %s)",
                 name, prev, requested)
  structure(list(name = name,
                 prev = prev,
                 requested = requested,
                 message = msg,
                 call = NULL),
            class = c("ConfigError", "error", "condition"))
}
