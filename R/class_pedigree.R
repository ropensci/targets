pedigree_init <- function(parent, child, index) {
  pedigree_new(parent, child, as.integer(index))
}

pedigree_new <- function(parent = NULL, child = NULL, index = NULL) {
  force(parent)
  force(child)
  force(index)
  environment()
}

pedigree_validate <- function(pedigree) {
  tar_assert_correct_fields(pedigree, pedigree_new)
  tar_assert_name(pedigree$parent)
  tar_assert_name(pedigree$child)
  tar_assert_match(pattern = pedigree$parent, x = pedigree$child)
  tar_assert_int(pedigree$index)
  tar_assert_positive(pedigree$index)
  invisible()
}
