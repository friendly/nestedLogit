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
data("TravelMode", package = "AER")

## travel vs. waiting time for different travel modes
library("lattice")
xyplot(travel ~ wait | mode, data = TravelMode)

## Greene (2003), Table 21.11, conditional logit model
library("mlogit")
# allow interaction of income with air travel
TravelMode$incair <- with(TravelMode, income * (mode == "air"))
tm_cl <- mlogit(choice ~ gcost + wait + incair, data = TravelMode,
  shape = "long", alt.var = "mode", reflevel = "car")
summary(tm_cl)

# nested logit model
travel.comp <- logits(
  public = dichotomy(c("air", "train", "bus"), "car"),
  ground = dichotomy(c("train", "bus"), "air"),
  train = dichotomy("train", "bus")
)

# These do not converge: WHY??
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred

travel.nested <- nestedLogit(
  mode ~ wait + gcost + incair + size,
  dichotomies = travel.comp,
  data = TravelMode
)
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
## Chile: named groups on both branches
chile.dichots <- logits(
   engage    = dichotomy(engaged    = c("Y", "N"),
                         disengaged = c("A", "U")),
   direction = dichotomy("Y", "N"),
   disengage = dichotomy("A", "U"))
 as.tree(chile.dichots, response = "vote")
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
