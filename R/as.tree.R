#' Display the Tree Structure of Nested Dichotomies
#'
#' @description Display the nested structure of a \code{"dichotomies"} or
#' \code{"continuationDichotomies"} object as a 2-D ASCII tree diagram showing
#' how the response categories are split at each level of the nesting.
#'
#' @name as.tree
#' @aliases as.tree as.tree.dichotomies as.tree.continuationDichotomies
#'
#' @param x A \code{"dichotomies"} or \code{"continuationDichotomies"} object.
#' @param response Optional character string giving the name of the response
#'   variable, used as the root label of the tree. If \code{NULL} (default),
#'   the root is labeled \code{"(response)"}.
#' @param lobstr Logical. If \code{FALSE} (default), renders a 2-D ASCII tree
#'   with \code{/} and \code{\\} branch connectors, with each node centered
#'   above its two children. If \code{TRUE}, builds a nested list
#'   representation of the tree and renders it via \code{\link[lobstr]{tree}},
#'   which must be installed.
#' @param \dots additional arguments (currently unused).
#'
#' @details
#' The flat list of dichotomies is reconstructed into a binary tree by matching
#' each dichotomy's domain (the union of its two sides) to the multi-level
#' groups produced by earlier splits.  Branch labels are taken from the named
#' arguments to \code{\link{dichotomy}} when present, and are otherwise
#' generated automatically as \code{\{level1, level2, ...\}}.
#'
#' @importFrom stats setNames
#' @return Invisibly returns \code{x}; called for its side effect of printing.
#'
#' @seealso \code{\link{logits}}, \code{\link{continuationLogits}},
#'   \code{\link{print.dichotomies}}
#'   Other conversions:
#'   \code{\link{as.matrix.dichotomies}}, \code{\link{as.character.dichotomies}}
#'
#' @examples
#' ## Womenlf: named group on one branch
#' comparisons <- logits(work = dichotomy("not.work",
#'                                        working = c("parttime", "fulltime")),
#'                       full = dichotomy("parttime", "fulltime"))
#' as.tree(comparisons, response = "partic")
#'
#' ## GSS: continuation logits for ordered education levels
#' cont <- continuationLogits(c("l.t.highschool", "highschool",
#'                              "college", "graduate"))
#' as.tree(cont, response = "degree")
#'
#' ## gators data: Food choice
#' # create dichotomies
#' gators.dichots <- logits(d1=dichotomy("Other", c("Fish", "Invertebrates")),
#'                          d2=dichotomy("Fish", "Invertebrates"))
#' as.tree(gators.dichots, response = "Food")
#' as.tree(gators.dichots, response = "Food", lobstr = TRUE)
#
#'
#' @export
#' @importFrom stats setNames
as.tree <- function(x, ...) UseMethod("as.tree")

#' @rdname as.tree
#' @export
as.tree.dichotomies <- function(x, response = NULL, lobstr = FALSE, ...) {
  tree       <- .build_dichot_tree(x)
  root_label <- if (!is.null(response)) response else "(response)"

  if (isTRUE(lobstr)) {
    if (!requireNamespace("lobstr", quietly = TRUE))
      stop("Package 'lobstr' is required when lobstr = TRUE. ",
           "Install it with: install.packages('lobstr')")
    nested <- setNames(list(.dichot_tree_to_list(tree)), root_label)
    lobstr::tree(nested)
  } else {
    layout <- .layout_tree(tree, root_label)
    lines  <- trimws(layout$canvas, which = "right")
    cat(paste(lines, collapse = "\n"), "\n")
  }
  invisible(x)
}

#' @rdname as.tree
#' @exportS3Method as.tree continuationDichotomies
as.tree.continuationDichotomies <- function(x, response = NULL,
                                            lobstr = FALSE, ...) {
  NextMethod()
}


# ── internal helpers ──────────────────────────────────────────────────────────

# Reconstruct a binary tree from a flat dichotomies object.
# Each node is a named list with:
#   type        "node" | "leaf"
#   label       (leaf only) character label
#   name        (node only) name of the dichotomy at this split
#   left        (node only) left child
#   left_label  (node only) explicit group label or NULL
#   right       (node only) right child
#   right_label (node only) explicit group label or NULL
.build_dichot_tree <- function(x) {
  all_levels <- attr(x, "levels")

  find_split <- function(levels) {
    for (i in seq_along(x))
      if (setequal(union(x[[i]][[1L]], x[[i]][[2L]]), levels)) return(i)
    NULL
  }

  make_node <- function(levels) {
    if (length(levels) == 1L)
      return(list(type = "leaf", label = levels))

    idx <- find_split(levels)
    if (is.null(idx))
      return(list(type = "leaf", label = paste(levels, collapse = "/")))

    d       <- x[[idx]]
    grp_nms <- names(d)

    list(
      type        = "node",
      name        = names(x)[idx],
      left        = make_node(d[[1L]]),
      left_label  = if (!is.null(grp_nms) && nzchar(grp_nms[1L])) grp_nms[1L] else NULL,
      right       = make_node(d[[2L]]),
      right_label = if (!is.null(grp_nms) && nzchar(grp_nms[2L])) grp_nms[2L] else NULL
    )
  }

  make_node(all_levels)
}

# Collect all leaf labels in a subtree (depth-first, left to right).
.collect_leaf_labels <- function(node) {
  if (node$type == "leaf") return(node$label)
  c(.collect_leaf_labels(node$left), .collect_leaf_labels(node$right))
}

# Auto-generate a branch label when no explicit group name is available.
.auto_label <- function(node) {
  if (node$type == "leaf") return(node$label)
  paste0("{", paste(.collect_leaf_labels(node), collapse = ", "), "}")
}

# Compute a 2-D horizontal layout for a subtree.
#
# Arguments:
#   node  — a tree node from .build_dichot_tree()
#   label — the text label to display at the root of this subtree
#
# Returns a list:
#   canvas   — character vector; all strings have equal width (= $width)
#   root_col — 0-based column of the center of `label` in the canvas
#   width    — total character width of the canvas
#
# Algorithm:
#   Leaves produce a 1-line canvas equal to `label`.
#   Internal nodes lay the two child subtrees side by side, separated by
#   enough space so that:
#     (a) the subtrees do not overlap,
#     (b) the / and \ arms each have room to start,
#     (c) the parent label (centered between the two child centers) fits.
#   The / connector is placed one column right of the left child's center;
#   the \ connector one column left of the right child's center.
.layout_tree <- function(node, label) {
  label_w <- nchar(label)

  if (node$type == "leaf") {
    return(list(
      canvas   = label,
      root_col = label_w %/% 2L,
      width    = label_w
    ))
  }

  left_lbl  <- if (!is.null(node$left_label))  node$left_label  else .auto_label(node$left)
  right_lbl <- if (!is.null(node$right_label)) node$right_label else .auto_label(node$right)

  L <- .layout_tree(node$left,  left_lbl)
  R <- .layout_tree(node$right, right_lbl)

  lc <- L$root_col   # center of left child (0-based, within left canvas)

  # right_start: 0-based column where the right subtree's canvas begins.
  # Constraints:
  #   (1) L$width + 1       — at least 1 space gap between canvases
  #   (2) lc + 3 - R$root_col — so that rc >= lc+3 (room for / space \)
  #   (3) label_w - lc - R$root_col — so that parent label starts at col >= 0
  right_start <- max(
    L$width + 1L,
    lc + 3L - R$root_col,
    label_w - lc - R$root_col
  )

  rc <- right_start + R$root_col          # center of right child (merged)
  pc <- (lc + rc) %/% 2L                  # parent center (merged)
  ls <- pc - label_w %/% 2L               # 0-based start column of label
  le <- ls + label_w                       # 0-based end column (exclusive)

  total_width <- max(right_start + R$width, le)

  # ---- label line -----------------------------------------------------------
  lbl_line <- paste0(strrep(" ", ls),
                     label,
                     strrep(" ", total_width - le))

  # ---- arm line (/ and \) ---------------------------------------------------
  arm_line <- strrep(" ", total_width)
  # "/" at 0-based col lc+1  →  1-based substr position lc+2
  # "\" at 0-based col rc-1  →  1-based substr position rc
  substr(arm_line, lc + 2L, lc + 2L) <- "/"
  substr(arm_line, rc,      rc)        <- "\\"

  # ---- merge child canvases -------------------------------------------------
  depth_L <- length(L$canvas)
  depth_R <- length(R$canvas)
  depth   <- max(depth_L, depth_R)

  L_lines <- c(L$canvas, rep(strrep(" ", L$width), depth - depth_L))
  R_lines <- c(R$canvas, rep(strrep(" ", R$width), depth - depth_R))

  child_lines <- vapply(seq_len(depth), function(i) {
    l_pad <- paste0(L_lines[i], strrep(" ", max(0L, right_start - nchar(L_lines[i]))))
    line  <- paste0(l_pad, R_lines[i])
    paste0(line, strrep(" ", max(0L, total_width - nchar(line))))
  }, character(1L))

  list(
    canvas   = c(lbl_line, arm_line, child_lines),
    root_col = pc,
    width    = total_width
  )
}

# Convert the tree to a nested named list for lobstr::tree().
.dichot_tree_to_list <- function(node) {
  if (node$type == "leaf") return(node$label)

  left_lbl  <- if (!is.null(node$left_label))  node$left_label  else .auto_label(node$left)
  right_lbl <- if (!is.null(node$right_label)) node$right_label else .auto_label(node$right)

  setNames(
    list(.dichot_tree_to_list(node$left),
         .dichot_tree_to_list(node$right)),
    c(left_lbl, right_lbl)
  )
}
