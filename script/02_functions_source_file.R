



# Function For hazard and HR

# ──────────────────────────────────────────────────────────────────────
# quick_standsurv()
#     • fit       : flexsurv[ spline ] model object
#     • at, t     : exactly as in standsurv()
#     • type      : "hazard", "rmst", "survival", …
#     • contrast  : "ratio", "difference", or NULL
#     • ...       : any other arguments you would pass to standsurv()
# ──────────────────────────────────────────────────────────────────────




quick_standsurv <- function(fit,
                            at,
                            t,
                            type      = c("hazard", "rmst", "survival",
                                          "cumhaz", "quantile"),
                            contrast  = c("ratio", "difference", NULL),
                            ...) {
  
  type     <- match.arg(type)
  contrast <- match.arg(contrast)
  
  ## 1 ── model frame & collapse ---------------------------------------
  mf <- model.frame(fit)
  
  # drop the Surv(...) column (name can vary)
  surv_col <- grep("^Surv\\(", names(mf))
  mf_collapsed <- mf |>
    select(-all_of(surv_col)) |>
    count(across(everything()), name = "(weights)")
  
  ## 2 ── call standsurv ----------------------------------------------
  standsurv(
    object   = fit,
    newdata  = mf_collapsed,
    at       = at,
    t        = t,
    type     = type,
    contrast = contrast,
    ...
  )
}





# Function nr 2, for for RMST calculations
# Creates weights of unique patterns in the data, and then calls `standsurv` on the unique patterns. 
# This is much faster than calling `standsurv` on the full data set, especially when there are many unique patterns.




# ------------------------------------------------------------
# fast_stand():  fast marginal RMST / hazard (+ contrasts & CIs)
# ------------------------------------------------------------
fast_stand <- function(fit, data,
                       at,
                       t,
                       type        = c("rmst", "hazard", "survival",
                                       "cumhaz", "quantile"),
                       contrast    = c("difference", "ratio", NULL),
                       ## CI options ----------
                       ci          = TRUE,
                       ci_method   = c("delta", "boot", "none"),
                       B           = 300,          # only used if ci_method = "boot"
                       ci_times    = NULL,         # calc CI only on these times
                       ## numeric tweaks -------
                       n_quad      = 30,           # Gauss nodes for RMST etc
                       trans       = "none")
{
  
  type      <- match.arg(type)
  contrast  <- match.arg(contrast)
  ci_method <- match.arg(ci_method)
  
  ## -------------------------------------------------- ##
  ## 1. figure out which columns to collapse            ##
  ## -------------------------------------------------- ##
  mf <- model.frame(fit)
  
  collapse_vars <- names(mf) |>
    { \(v) v[!grepl("^Surv\\(", v)] }() |>          # drop Surv(...)
    setdiff(c("(weights)", "(id)")) |>              # drop specials
    intersect(names(data))                          # keep those present
  
  ## -------------------------------------------------- ##
  ## 2. collapse full data --> unique patterns + weight ##
  ## -------------------------------------------------- ##
  uniq <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(collapse_vars)),
                 name = "(weights)") |>
    ## ensure factor levels identical to fit ----------
  purrr::imap_dfc(\(col, nm){
    if (is.factor(col) && nm %in% names(mf))
      factor(col, levels = levels(mf[[nm]]))
    else col
  })
  
  ## -------------------------------------------------- ##
  ## 3. time vector for CI vs. point estimate           ##
  ## -------------------------------------------------- ##
  t_for_ci <- if (!is.null(ci_times) && ci) ci_times else t
  
  ## -------------------------------------------------- ##
  ## 4. progress message                                ##
  ## -------------------------------------------------- ##
  message(
    "[fast_stand] N = ", scales::comma(nrow(data)),
    "  →  ", nrow(uniq), " unique patterns.  ",
    "type = ", type,
    ", contrast = ", ifelse(is.null(contrast), "none", contrast),
    if (ci) paste0("  (CI via ", ci_method,
                   if (ci_method == "boot") paste0(", B = ", B), ")") else "",
    if (type == "rmst") paste0("  n_quad = ", n_quad)
  )
  
  ## helper that actually calls standsurv ----------------
  run_ss <- function(t_vec, ci_flag){
    standsurv(
      object        = fit,
      newdata       = uniq,
      at            = at,
      t             = t_vec,
      type          = type,
      contrast      = contrast,
      ci            = ci_flag,
      boot          = (ci_method == "boot" && ci_flag),
      B             = if (ci_method == "boot" && ci_flag) B else NULL,
      trans         = trans,
      n.gauss.quad  = n_quad
    )
  }
  
  ## -------------------------------------------------- ##
  ## 5. do the work                                     ##
  ## -------------------------------------------------- ##
  if (!ci || ci_method == "none") {
    ## just a single cheap call
    out <- run_ss(t, ci_flag = FALSE)
  } else if (is.null(ci_times)) {
    ## CI wanted on *all* t
    out <- run_ss(t, ci_flag = TRUE)
  } else {
    ## (a) point estimates everywhere
    pe <- run_ss(t,        ci_flag = FALSE)
    ## (b) CI only at selected times
    ci_tab <- run_ss(t_for_ci, ci_flag = TRUE)
    
    # merge: keep point-estimates column(s) from `pe`
    keep_cols <- names(pe)[!grepl("_lci$|_uci$|_se$", names(pe))]
    out <- dplyr::left_join(ci_tab, pe[, keep_cols], by = "time")
  }
  
  out
}
# ------------------------------------------------------------

