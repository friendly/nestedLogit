# timeDate 4052.112

- streamlined `holidayNYSE()`.

- fixed a minor bug in `holidayTSX()` and streamlined it a bit.

- removed the deprecated (in v4032.109) `timeDate` method for `cut`, use
  `window` instead.

- modified the `timeDate` method for `window` to return a 0-length `timeDate`
  object when the window is empty. Previously it was returning a length-1 object
  with a data value `NA`.

- removed the S4 timeDate method for `frequency` (the S3 method suffices). Also,
  `frequency` is no longer turned into an S4 generic and not exported.

- removed `getRmetricsOption`, use `getRmetricsOptions` (i.e., plural) instead.


# timeDate 4051.111

- new class `"timeInterval"` represents time intervals. Methods are defined for
  computing union, intersection, complement and set difference of
  `"timeInterval"` objects, as well as checking whether dates and time intervals
  are contained in such objects. See `?timeInterval`, `class ? timeInterval`,
  `?in_int`. Not systematically tested yet.

- the prototype of class `"timeDate"` now sets admissible values for the
  slots. The previous defaults (e.g., `character(0)` for slots `format` and
  `FinCenter`) were causing minor problems in rare cases.

- streamlined the initialisation method of `"timeDate"`. This changes slot
  `format` in some inconsequential cases involving `Inf` (e.g.,
  `timeDate(.POSIXct(-Inf))@format` was `"%Y-%m-%d"` but now is `"%Y-%m-%d
  %H:%M:%S"`, which even seems more appropriate).

- the `"timeDate"` methods for `trunc` and `round` now support also units
  "secs", "months" and "years". Previously only "days", "hours", "mins" were
  supported.

- The functions `timeLastDayInMonth`, `timeFirstDayInMonth`,
  `timeLastDayInQuarter`, `timeFirstDayInQuarter` `timeNthNdayInMonth`,
  `timeLastNdayInMonth`, `timeNdayOnOrAfter` and `timeNdayOnOrBefore` are now
  documented to accept `timeDate`, `POSIXlt`and other time-date objects (and
  take into account their time zone). Previously, the first argument of these
  functions was documented to be a character vector but they were silently
  accepting time-date without checking the associated time zone. This could lead
  to wrong results in some cases.

- new function `pasteMat`, like `paste` but doesn't convert `NA`s to the string
  `"NA"` and keeps `NA`s in the output. Also, argument `sep` can be a
  vector. Can be useful when creating date-times from individual pieces which
  may contain `NA`s, see examples for `timeDate()`.

- fixed a bug in the internal function `.formatFinCenterNum()`, which sometimes
  was causing `timeDate()` to throw error when there were `NA` date-times.

- removed the deprecated `GBBankHoliday`, use `GBSpringBankHoliday` instead.

- removed the deprecated `GBMayDay`, use `GBEarlyMayBankHoliday` instead.

- removed the deprecated `GBMilleniumDay`, use `specialHolidayGB(1999)` instead.

- removed the deprecated `JPKeirouNOhi`, use ``JPKeirouNoHi` instead.


# timeDate 4041.110

- new `timeDate` method for `$` for extraction of components of timeDate
  objects.  For example, given a timeDate object `td`, numeric vectors
  containing the components 'year', 'month' and 'sec' can be obtained by
  `td$year`, `td$month`, and `td$sec`, respectively. In interactive sessions
  completion is supported, usually by hitting the 'TAB' key.

- new` timeDate` methods for the base R functions `quarters` and `weekdays`
  (they return character vectors of names).

- new `timeDate` method for `months` returning a character vector. For now, a
  character vector will be returned only if argument 'abbreviate' is set
  explicitly (to `TRUE` or `FALSE`). If 'abbreviate' is missing, the numeric
  values are returned, see the deprecation note below.

  **Deprecation Warning:** a `timeDate` method for `months` has existed for a
  long time but it was returning a numeric vector, which is inconsistent with
  the other methods for months in base R (they return names of
  months). Returning a numeric vector when 'abbreviate' is missing is a
  temporary compromise, to avoid breaking old code, but this should be
  considered deprecated. Use `td$month` to get the numbers.

- now `timeDate(character(0))` returns a 0-length "timeDate" object.  Previously
  it was returning an `NA` "timeDate" object of length 1 if argument 'format'
  was missing and a 0-length object otherwise.

- changed the "timeDate" method for `show` to print something like 'timeDate of
  length 0' along with the FinCenter, which seems better than the previous
  output for this case.

- new function `InternationalWomensDay` (suggested by Alexander Bartel with
  code), fixes R-forge issue #6855). It's also included in the list returned by
  `listHolidays`.


# timeDate 4032.109

- new function `earlyCloseNYSE` gives datetimes of early closings of NYSE,
  including scheduled and unscheduled ones.  The information is incomplete. This
  answers issue [#6757], see also the related issue [#1356] mentioned below in
  the news for a previous version.

- all functions returning public and eclessiastical holidays get new arguments
  `value`, `na_drop`, and `...`, controlling the type of the return value and
  the handling of missing values. Argument `...` is for things like 'format'
  when applicable. Not all honour the new arguments though, see below.

- all functions returning public and eclessiastical holidays now honour the new
  argument `value`.

- some holiday functions were amended or rewritten to honour the new argument
  `na_drop`. These include the England holidays ('GBxxx'), some Japan holidays
  (JPxxx), and a number of other functions (e.g., `CAFamilyDay). Contributions
  and/or information needed to have them do so is wellcome (e.g. dates when a
  holiday was introduced and/or moved).


- `midnightStandard2()` (and hence `timeDate()`) was throwing a puzzling error
  when the input character vector contained one or more `NA`s mixed with
  non-NA's.

- `whichFormat()` was throwing error when the _first_ element of the input
  vector was `NA`. This could be considered a feature but that's inconsistent
  since `NA`s at other positions were not causing trouble. Now fixed.

- the 'character' method for `timeDate()` was passing only the first element of
  the input vector to `whichFormat()`. This was preventing it from inferring the
  format when argument 'format' was missing.


- new function `specialHolidayGB` gives the special UK one-off holidays for
  the requested years.

- `MilleniumDay` is now deprecated, use `specialHolidayGB(1999)`.


- fixed omissions in `holidayLONDON` and refactored it completely.  It became
  easier to maintain and orders of magnitude faster.

- fixed a bug in an internal function which was throwing errors from
  `holidayLONDON` for some ranges including years before 1916.


- `GBEarlyMayBankHoliday`, `GBSpringBankHoliday` and `GBSummerBankHoliday` now
  give dates according to historical rules, not simply according to current
  ones. See the remarks above about the new arguments of holiday functions.


- major overhaul and update of the Japan's holidays. Previously fixed dates were
  returned for each holiday. Now moving holidays are calculated (e.g., second
  month of January) and some exceptions handled (like holidays moved due to the
  Olympics in 2020 and 2021). Historical changes are (partially) taken into
  account. Further corrections and historical amendments are welcome.

  Thanks to Sylvie Lachaud for reporting the issues with Japan holiday
  functions, as well as providing current correct definitions and extensive list
  of links.

- new function `JPMountainDay` gives the relatively recently introduced Japan
  holiday 'Mountain Day'.

- `JPKeirouNoHi` replaces `JPKeirouNOhi`. The latter is an aberration from all
  other `xxxNoHi` Japanese holiday functions and a source of difficult to spot
  errors. The old one is now deprecated and will be removed in the future.


- modified the 'timeDate' method for `summary` to return an object from class
  "timeDate_summary" (rather than print directly) and created a print method for
  the latter.

- deprecated the `timeDate` method for `cut`, see deprecation note for v4021.105
  below.

- stopped exporting all but one (`.endpoints`) functions starting with a
  '.'. They all seem for internal purposes and/or have similarly named functions
  with normal names.

- removed `.whichFormat` and `.midnightStandard`, not officially deprecated but
  had been renamed to `whichFormat()` and `midnightStandard()`, respectively, a
  long time ago.

- removed several internal functions that are no longer used anywhere in the
  package.

- removed file 'namespace-tags.R' as it had not been updated recently, see
  revision r83578 or earlier if you need it.


# timeDate 4022.108

- added the 2023 UK Bank holiday for the coronation of King Charles III.

- `axis.timeDate` was not handling properly the case when `x` was missing,
  leading to errors from R-devel check (2023-01-07 r83578). Fix suggested by Uwe
  Lieges.

- refactored file NAMESPACE to facilitate maintenance (that revealed the
  two omissions listed below).

- `CAFamilyDay` (Canada Family Day) was missing from the list returned by
  `listHolidays()`. It was missing only from that list, `holidayTSX()` was
  including it when applicable.

- `JPVernalEquinox` was missing from the list returned by `listHolidays()`.

- the financial centers are now updated to reflect changes in time zones in
  recent years. The list returned by `listFinCenter()` is synchronised with
  current time zone names. Previous names supported by timeDate are available as
  aliases.
  
- import selectively from 'stats' and 'utils'.


# timeDate 4021.107

- London financial centre holidays - fixed and/or included non-standard holidays
  (e.g., Early May Bank holiday was moved in 2020 to VE day; Spring Bank holiday
  was moved in Queen's Jubilee years; state funeral of the Queen).  Millenium
  day now is included in the result of `holidayLONDON(1999)`.  The London
  holidays should now be complete up to the time of writing this (1 Oct 2022).

- renamed `GBMayDay` to `GBEarlyMayBankHoliday` and `GBBankHoliday` to
  `GBSpringBankHoliday`. The old names are somewhat ambiguous and strongly
  discouraged but still available. `listHolidays()` gives the new names.

- the generic `timeDate()` gets argument '...' to allow methods for it to have
  additional arguments (e.g., for DST gaps).

- the 'character' method for `timeDate()` gets a new argument `dst_gap` to
  control what to do with non-existent DST times at the requested `FinCenter`
  with options to add/subtract ("+", "-") the DST shift or set them to `NA`.

- `timeDate()` was not handling correctly some times just after the switch
  to/from DST. This was visible mostly for time zones away from GMT and GMT+1.

- In `timeSequence()`, if any of the generated times would fall in DST gaps,
  they are moved by "+1 hour", corresponding to `dst_gap = "+"` in `timeDate`.
  This is consistent with `seq` for other time objects.  Currently there is no
  option to change this behaviour of `timeSequence`.

  Previously `timeSequence` was moving DST gaps down by 1 hour (for by =
  'DSTday' and similar). This was not consistent similar time functions in R and
  was actually due to a bug (or unfinished DST handling) in `timeDate`, see
  remarks for `timeDate` above.
  
- `timeSequence()` now throws error if argument `from` is in a DST gap. It seems
  desirable to have a default action for this case. Rolling the faulty time by
  an hour in the case of 'DSTday' may be suitable in most cases but for other
  values of `by` it might be totally wrong. 

- updated the DST rules.

- internally, refactored the way the DST rules are generated (not visible to
  users).

- `rulesFinCenter()` now looks for a financial center starting from the
  namespace of `timeDate`. Previously it was starting from the environment of
  the caller which could result in using an unrelated object or, if `timeDate`
  was loaded but not attached, not finding it.

  
# timeDate 4021.106

- fix `whichFormat()` to accommodate a change in R-devel after which
  `as.character(Sys.time())` contains fractional seconds. (`format(Sys.time())`
  doesn't; before this change in R-devel both were dropping the fractional
  seconds). (fixed by Martin Maechler, see timeDate rev 6286)


# timeDate 4021.105

- the list returned by `holidaysNYSE()` was missing the special closing days of
  the New York stock exchange (NYSE). Now it should be complete (though there
  may be ommissions after 2011). This fixes issue #1356 reported by Corwin
  Joy. Thanks to him and Ian E for the insigthful discussion and useful links.

  See also below. Contributions for the other exchanges and corrections are
  welcome.

- `holidaysNYSE()` gets a new argument, `type`, to select what type of the
  exchange's closing days to return. The default is to return all days in the
  requested years when NYSE was closed for whatever reason. Use `type = "standard"`
  and `type = special` to get the standard holidays and the special closings,
  respectively.

  Returning any closing day by default might be considered a breaking
  change. However, not returning all closing days was perceived as erroneous by
  users (eg issue #1356). In fact, the package itself calculates business days
  by dropping weekends and days returned by `holidayXXXX`.

  Note that `holiday()` returns the actual dates of the public holidays, while
  the corresponding days returned by `holidayXXXX` are the resulting non-weekend
  closing days, if any.

- `holidayTSX()` now correctly calculates Christmas and Boxing day closures when
   Christmas is on Monday.  Fixes part (2) of issue #1288 reported by Stefan
   Wilhelm (part (1) was fixed in a previous release). The fix is really a patch
   for the specific issue, maybe the same should be done when Christmas is on
   Sunday, for example. Information/contribution on Canadian holidays is
   welcome.

- now `holiday()` accepts also a function or a list of functions for argument
  'Holiday'.

- `timeNthNdayInMonth` could return a value in the following month. Now
  fixed. This is bug #1463 reported with a fix by Manny C. Note that the bug was
  not present for dates in the first day of a month.

- `timeLastNdayInMonth` could return a value in the following month,
  e.g. '1996-06-04' for the last Tuesday in May 1996. Now fixed. The check of
  this function was prompted by the bug report for #1463 (see above) for
  `timeNthNdayInMonth` but the error was different.

- the `data.frame` methods for `kurtosis()` and `skewness()` now set attribute
  `method` as for the other methods and as documented.

- removed `.holidayList()` which had been replaced by `listHolidays()` a long
  time ago and was not exported in recent versions of `timeDate`.

- updated documentation files.

## Deprecation notes

- the `timeDate` method for `cut` has been discouraged in the sources for a long
  time with a recommendation to use `window` instead (just replace `cut(x,
  from = xx , to = yy)` with `window(x, start = xx, end = yy)`. The `cut` method
  will be deprecated in the next release and later removed or replaced by a
  method that is consistent with the methods for `cut` in base R.
  

# timeDate 4021.104

- new maintainer: Georgi N. Boshnakov.

- updated DESCRIPTION with links and moved all `Depends:` to `Imports:`.

- removed the line `LazyData: yes` from DESCRIPTION to fix the NOTE on CRAN.

- added the new US holiday, Juneteenth National Independence Day. Fixes #6755 by
  Ian E (ene100).

- `holidayTSX()` now includes the Labour Day. Fixes part (1) of issue #1288
  reported by Stefan Wilhelm.

- created a first version of `_pkgdown.yml` for more organised view of the large
  number of objects in the package. Unpack the tarball and run
  `pkgdown::build_site()` to build the site locally. Don't know if this could
  work directly off the R-forge repository.
  

# timeDate 3043.102 and older versions

  See file `ChangeLog` for changes before 4021.104.
