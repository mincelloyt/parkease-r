# security.R - Password hashing functions
library(openssl)

# Hash a password
hash_password <- function(password) {
  # Use SHA-256 hashing with a salt
  hash <- sha256(paste0("hotel_salt_", password))
  return(as.character(hash))
}

# Verify a password
verify_password <- function(password, hash) {
  # Hash the provided password and compare
  test_hash <- hash_password(password)
  return(identical(test_hash, hash))
}