# Candidate datasets for nested logit examples

Datasets with polytomous responses that could profit from nested logit modeling,
either because the response categories have a natural hierarchical/tree structure,
or because the response is ordinal and continuation logits (a special case of
nested dichotomies) are appropriate.

The package already uses: `Womenlf` (parttime/fulltime employment), `GSS`
(educational attainment, continuation logits), `gators` (food choice),
`HealthInsurance` (product A/B/C/D).

---

## Transport mode choice — `AER::TravelMode`

**Package**: `AER`
**Response**: mode of travel — `air`, `train`, `bus`, `car` (4 levels)
**Predictors**: `income` (household income), `wait`, `vcost`, `travel` (travel time),
`gcost` (generalised cost) — some are alternative-specific
**N**: 210 individuals × 4 alternatives = 840 rows (long format)

**Nesting rationale**: This is the *canonical* nested logit example in the
econometrics literature (McFadden 1978). The IIA assumption of plain multinomial
logit is implausible here: adding a "blue bus" to {car, train, red bus} should
primarily steal share from red bus, not from car equally. A natural 3-level tree:

```
                  all modes
                 /          \
            car           public
                        /    |    \
                      air  train  bus
```

Or a 2-level nesting:
```
          all modes
         /         \
      private     public
       (car)     /      \
               air    ground
                      /    \
                   train   bus
```

**References**: McFadden (1978); Greene *Econometric Analysis* ch. 18;
Train *Discrete Choice Methods with Simulation* (2003).
**Note**: This dataset is in wide-ish format; would need reshaping. The `mlogit`
package has similar datasets already in long format.


### Examples

```r
library(nestedLogit)
library(nnet)     # for multinom()
library(car)      # for Anova()
library(dplyr)    # for data wrangling

data("TravelMode", package = "AER")
```

#### Why the earlier code failed to converge

The raw `TravelMode` data is in **long format** (840 rows = 210 individuals × 4
modes). The variables `wait`, `gcost`, etc. are *alternative-specific*: they
vary across modes for the same individual. Passing this long-format data to
`nestedLogit()` with those variables creates near-perfect separation in the
binary logit submodels — hence the non-convergence.

`nestedLogit()` expects **one row per individual** with the *chosen* mode as
the response. Only **individual-specific** predictors (the same value for all
four rows of one individual) work directly; here that means `income` and
`size`.

```r
## Reshape: one row per individual, response = chosen mode
tm <- TravelMode |>
  filter(choice == TRUE) |>
  select(mode, income, size) |>
  mutate(mode = relevel(factor(mode), ref = "car"))

table(tm$mode)
#  car  air  bus train
#   90   58   17   45   (approximately)
```

#### Nested dichotomy structure

The classic nesting for travel-mode choice separates *private* (car) from
*public* transit, then within public separates *air* from *ground* transport,
then within ground separates *train* from *bus*.

```r
travel_dichots <- logits(
  pvt_pub  = dichotomy("car", public = c("air", "train", "bus")),
  air_grnd = dichotomy("air", ground = c("train", "bus")),
  tr_bus   = dichotomy("train", "bus")
)

## Visualise the tree
as.tree(travel_dichots, response = "mode")
#          mode
#         /    \
#       car   public
#             /    \
#           air   ground
#                 /    \
#              train   bus
```

#### Multinomial logit (traditional approach)

```r
tm_multi <- multinom(mode ~ income + size, data = tm, trace = FALSE)
summary(tm_multi)
```

#### Nested logit

```r
tm_nested <- nestedLogit(mode ~ income + size,
                         dichotomies = travel_dichots,
                         data = tm)
summary(tm_nested)

## Omnibus tests for each predictor across all sub-models
car::Anova(tm_nested)
```

#### Comparing predicted probabilities

```r
## Predicted probabilities over the income range, solo traveller
new_data <- data.frame(income = seq(10, 60, by = 10), size = 1)

pred_nested <- as.data.frame(predict(tm_nested, newdata = new_data))
pred_multi  <- as.data.frame(predict(tm_multi,  newdata = new_data,
                                     type = "probs"))

## Nested logit
cbind(income = new_data$income, round(pred_nested, 3))

## Multinomial logit
cbind(income = new_data$income, round(pred_multi[, levels(tm$mode)], 3))
```

#### Comparing log-likelihoods

The nested logit log-likelihood is the *sum* of the three binary logit
log-likelihoods; the models are not directly comparable by likelihood
because they partition the data differently — but AIC is comparable.

```r
## Nested logit: sum of sub-model log-likelihoods
logLik(tm_nested)

## Multinomial logit
logLik(tm_multi)

## AIC comparison
AIC(tm_nested)
AIC(tm_multi)
```

---

## Recreational fishing mode — `mlogit::Fishing`

Choice of Fishing Mode
A sample of 1182 individuals in the United-States for the choice of 4 alternative fishing modes.

**Package**: `mlogit`
**Response**: fishing mode — `beach`, `pier`, `boat`, `charter` (4 levels)
**Predictors**: `price` and `catch` (alternative-specific), `income` (individual)
**N**: 1182 individuals

**Nesting rationale**: Shore-based (beach, pier) vs. vessel-based (boat, charter),
then within each group:

```
           fishing
          /        \
       shore      vessel
      /     \    /      \
   beach   pier boat  charter
```

Shore-based alternatives share unobserved attributes (no boat needed, lower cost)
that vessel-based do not, so IIA is plausible within but not across groups.

**References**: Herriges & Kling (1996); used as the main example in the `mlogit`
package vignette.

---

## Chilean plebiscite voting intent — `carData::Chile`

Voting Intentions in the 1988 Chilean Plebiscite

**Package**: `carData`
**Response**: `vote` — `A` (abstain), `N` (no), `U` (undecided), `Y` (yes) (4 levels)
**Predictors**: `age`, `sex`, `education`, `income`, `statusquo` (attitude toward
status quo, strong predictor), `region`, `population`
**N**: 2700

**Nesting rationale**: Two plausible tree structures:

*Engagement then direction*:
```
         vote
        /     \
   engaged   not engaged
    /   \      /      \
   Y     N    A        U
```
First dichotomy: will the person cast a meaningful vote (Y or N) vs. disengage
(abstain or undecided)? Second: among the engaged, yes vs. no; among the
disengaged, abstain vs. undecided.

*Direction then engagement*:
```
         vote
        /     \
      pro     anti/neutral
     /   \       /      \
    Y     U     N        A
```

`statusquo` should dominate the direction split; demographic variables the
engagement split. The two trees make different substantive claims and could be
compared via likelihood.

**References**: Fox & Andersen (2006); Fox *Applied Regression Analysis and
Generalized Linear Models* 3rd ed.
**Note**: this dataset is already in `carData`, which is a dependency of
`nestedLogit`, so no extra package needed.

### Examples

```r
library(nestedLogit)
library(nnet)    # for multinom()
library(car)     # for Anova()

data("Chile", package = "carData")
```

#### Data preparation

`Chile` has 2700 rows (one per respondent) and 168 missing values on `vote`.
`nestedLogit()` drops incomplete cases internally, but it is cleaner to remove
them up front so that sample sizes are identical across all models.

```r
chile <- Chile[!is.na(Chile$vote), ]   # n = 2532

table(chile$vote)
#   A    N    U    Y
# 187  889  588  868
```

#### Two competing tree structures

The key substantive question is the **order of the decision**:

*Engagement then direction* — Does the person decide first whether to
participate (`Y` or `N`) versus disengage (`A` or `U`), and only then choose
direction?

```r
dichots_eng <- logits(
  engage    = dichotomy(engaged    = c("Y", "N"),
                        disengaged = c("A", "U")),
  direction = dichotomy("Y", "N"),
  disengage = dichotomy("A", "U")
)

as.tree(dichots_eng, response = "vote")
#           vote
#          /    \
#      engaged  disengaged
#       /   \     /    \
#      Y     N   A      U
```

*Direction then engagement* — Does the person first form a pro- or
anti-status-quo opinion, and only then decide whether to act on it?

```r
dichots_dir <- logits(
  direction  = dichotomy(pro  = c("Y", "U"),
                         anti = c("N", "A")),
  engage_pro = dichotomy("Y", "U"),
  engage_ant = dichotomy("N", "A")
)

as.tree(dichots_dir, response = "vote")
#          vote
#         /    \
#       pro    anti
#      /   \   /   \
#     Y     U N     A
```

The expectation: `statusquo` (support for Pinochet's regime) should
overwhelmingly predict *direction*, while demographic variables (`age`, `sex`,
`education`, `income`) govern *engagement*. If so, the direction tree should
fit better — each sub-model does a cleaner job — and the `statusquo`
coefficient should be large in the direction dichotomy and small in the two
engagement dichotomies.

#### Multinomial logit (traditional approach)

```r
# "N" (no) is the natural reference for the 1988 plebiscite
chile_multi <- multinom(
  vote ~ statusquo + age + sex + education + income,
  data  = within(chile, vote <- relevel(vote, ref = "N")),
  trace = FALSE)
summary(chile_multi)
```

#### Nested logit — engagement tree

```r
chile_eng <- nestedLogit(
  vote ~ statusquo + age + sex + education + income,
  dichotomies = dichots_eng,
  data = chile)

summary(chile_eng)
car::Anova(chile_eng)
```

#### Nested logit — direction tree

```r
chile_dir <- nestedLogit(
  vote ~ statusquo + age + sex + education + income,
  dichotomies = dichots_dir,
  data = chile)

summary(chile_dir)
car::Anova(chile_dir)
```

#### Which tree fits better?

Both nested models have the same number of parameters (5 predictors × 3
sub-models = 15 coefficients plus 3 intercepts), so AIC is a fair comparison.

```r
AIC(chile_multi)   # multinomial baseline
AIC(chile_eng)     # engagement-first
AIC(chile_dir)     # direction-first

logLik(chile_multi)
logLik(chile_eng)
logLik(chile_dir)
```

If the direction tree has lower AIC, it means the data are better described
by "form an opinion first, then decide whether to act" — consistent with
`statusquo` being primarily a predictor of political direction rather than
of participation.

#### Predicted probabilities as `statusquo` varies

```r
new_data <- data.frame(
  statusquo = seq(-1.5, 1.5, by = 0.5),
  age       = median(chile$age),
  sex       = factor("F",  levels = levels(chile$sex)),
  education = factor("S",  levels = levels(chile$education)),
  income    = median(chile$income)
)

pred_eng   <- as.data.frame(predict(chile_eng, newdata = new_data))
pred_dir   <- as.data.frame(predict(chile_dir, newdata = new_data))
pred_multi <- as.data.frame(
  predict(chile_multi, newdata = new_data, type = "probs"))

## Side-by-side for the two nested models
round(cbind(statusquo = new_data$statusquo, pred_eng), 3)
round(cbind(statusquo = new_data$statusquo, pred_dir), 3)
```

#### Plot: predicted probabilities from both nested models

```r
library(tidyr)
library(ggplot2)

bind_preds <- function(pred, model_label) {
  cbind(new_data, pred) |>
    pivot_longer(cols = c("A", "N", "U", "Y"),
                 names_to = "vote", values_to = "prob") |>
    transform(model = model_label)
}

pred_long <- rbind(bind_preds(pred_eng, "Engagement tree"),
                   bind_preds(pred_dir, "Direction tree"))

ggplot(pred_long, aes(x = statusquo, y = prob, colour = vote)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ model) +
  labs(x = "Attitude toward status quo", y = "Predicted probability",
       colour = "Vote",
       title = "Chile 1988: predicted vote probabilities by tree structure") +
  theme_bw()
```

---

## Arthritis treatment outcome — `vcd::Arthritis`

**Package**: `vcd`
**Response**: `Improved` — `None`, `Some`, `Marked` (3 ordered levels)
**Predictors**: `Treatment` (Placebo/Treated), `Sex`, `Age`
**N**: 84

**Nesting rationale**: The response is *ordinal*, making continuation logits
(a special case of nested dichotomies) natural:

```
         outcome
        /        \
     None       Some improvement
               /               \
            Some             Marked
```

Dichotomy 1: any improvement vs. none.
Dichotomy 2: given some improvement, marked vs. only some.

This is a clean, small, well-known teaching dataset. Treated patients should
show higher probabilities of moving up both dichotomies.

**References**: Agresti *Categorical Data Analysis* 2nd ed., Table 8.6.

---

## Smoking status — `MASS::survey`

**Package**: `MASS`
**Response**: `Smoke` — `Never`, `Occas`, `Regul`, `Heavy` (4 ordered levels)
**Predictors**: `Sex`, `Age`, `Height`, `Pulse`, `Exer` (exercise frequency),
`Hand` (writing hand)
**N**: 237 (student survey, some NAs)

**Nesting rationale**: Ordinal, natural continuation logits:

```
      smoking
      /      \
   Never    Ever smoked
            /          \
         Occas        Regular+
                      /       \
                   Regul      Heavy
```

Dichotomy 1: ever vs. never.
Dichotomy 2: occasional vs. regular/heavy.
Dichotomy 3: regular vs. heavy.

**Note**: Small sample and few strong predictors may limit statistical interest,
but it is a readily available dataset for illustration.

---

## Pneumoconiosis severity — `VGAM::pneumo`

**Package**: `VGAM`
**Response**: severity — `normal`, `mild`, `severe` (3 ordered levels)
**Predictor**: `duration` (years of exposure to coal dust, continuous)
**N**: 8 grouped observations (miners aggregated by exposure duration)

**Nesting rationale**: Classic ordinal continuation logits — normal vs. any
disease; given disease, mild vs. severe. Duration should increase probability of
disease and of severity monotonically.

**References**: Ashford (1959); McCullagh & Nelder *Generalized Linear Models*
2nd ed.
**Note**: Very small aggregated dataset; more useful as a clean teaching example
than for a full vignette.

---

## Mental health status — Agresti / `vcdExtra`

**Response**: mental health status — `Well`, `MildSymptom`, `ModerateSymptom`,
`Impaired` (4 ordered levels)
**Predictors**: `SES` (socioeconomic status: low/high), `LifeEvents` (count of
adverse life events)
**Source**: Agresti *Categorical Data Analysis* Table 9.1; may be in `vcdExtra`
or needs manual entry from the book.

**Nesting rationale**: Ordinal — classic continuation logit application:
Well vs. any symptoms; mild vs. moderate+; moderate vs. impaired.
SES and life events are theoretically motivated predictors of each transition.

**References**: Agresti (2002) §9.3; also discussed in Fox *Applied Regression*
as a nested dichotomy example.

---

## Summary table

| Dataset | Package | Response levels | Structure | N |
|---------|---------|-----------------|-----------|---|
| `TravelMode` | AER | air/train/bus/car | tree (IIA violation) | 210 |
| `Fishing` | mlogit | beach/pier/boat/charter | shore vs. vessel | 1182 |
| `Chile` | carData | A/N/U/Y | engagement × direction | 2700 |
| `Arthritis` | vcd | None/Some/Marked | ordinal continuation | 84 |
| `survey$Smoke` | MASS | Never/Occas/Regul/Heavy | ordinal continuation | 237 |
| `pneumo` | VGAM | normal/mild/severe | ordinal continuation | 8 |
| Mental health | vcdExtra? | Well/Mild/Moderate/Impaired | ordinal continuation | ~1000 |

## Priority for a new vignette or example

- **Best substantive story + accessible data**: `carData::Chile` (no new
  dependency; strong predictors; two competing tree structures to compare)
- **Most canonical nested logit example**: `AER::TravelMode` or `mlogit::Fishing`
  (the transport/IIA motivation is the original reason nested logit was invented)
- **Best ordinal/continuation example beyond GSS**: `vcd::Arthritis` (clean,
  small, well-known, clear clinical story)
