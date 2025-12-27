# security.R
library(openssl)

# Password hashing functions
hash_password <- function(password) {
  # Use SHA-256 for demo (in production, use bcrypt with R package 'bcrypt')
  sha256(password)
}

# Verify password
verify_password <- function(input_password, stored_hash) {
  input_hash <- sha256(input_password)
  return(input_hash == stored_hash)
}