#' ---
#' title: Test logit_notation argument in equatiomatic (branch logit_notation)
#' ---
#
# Tests the new `logit_notation = TRUE/FALSE` argument added in
# https://github.com/datalorax/equatiomatic/tree/logit_notation
# (see issue https://github.com/datalorax/equatiomatic/issues/247)
#
# Install the branch first:
#   remotes::install_github("datalorax/equatiomatic@logit_notation")

library(nestedLogit)
library(equatiomatic)
data(Womenlf, package = "carData")

wlf.nested <- nestedLogit(partic ~ hincome + children,
  dichotomies = logits(
    work = dichotomy("not.work", working = c("parttime", "fulltime")),
    full = dichotomy("parttime", "fulltime")
  ),
  data = Womenlf)

mod.work <- models(wlf.nested, "work")
mod.full <- models(wlf.nested, "full")

# --- Test 1: logit_notation on a single glm sub-model ---
extract_eq(mod.work, logit_notation = FALSE)  # baseline: log-fraction LHS
extract_eq(mod.work, logit_notation = TRUE)   # want: \operatorname{logit}[P(...)]

# Generates a perplexing NOTE: logit_notation = TRUE ignored when show_distribution is TRUE.
#

# Show preview in Viewer panel
extract_eq(mod.work, logit_notation = TRUE) |> preview_eq()

# --- Test 2: same for full ---
extract_eq(mod.full, logit_notation = TRUE)

# --- Test 3: passes through to nestedLogit method ---
extract_eq(wlf.nested, logit_notation = FALSE)  # baseline
extract_eq(wlf.nested, logit_notation = TRUE)   # want logit notation + correct dichotomy names

# --- what happens with preview_eq() here?
extract_eq(wlf.nested, logit_notation = TRUE) |> preview_eq()

# output:
# logit_notation = TRUE ignored when show_distribution is TRUE.
# logit_notation = TRUE ignored when show_distribution is TRUE.
# Error in UseMethod("extract_lhs", model) :
#   no applicable method for 'extract_lhs' applied to an object of class "c('equation', 'character')"


# --- Test 4: combined with other args ---
extract_eq(wlf.nested, logit_notation = TRUE, use_coefs = TRUE)
extract_eq(wlf.nested, logit_notation = TRUE, wrap = TRUE, terms_per_line = 2)
