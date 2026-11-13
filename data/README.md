# Data

`rba_snapshot.csv`: one row per month, Jan 2005 – Sep 2026.

| Columns | Source |
|---|---|
| `a_*`, `bbb_*` yields and tenors (3/5/7/10y), bond counts | RBA F3: https://www.rba.gov.au/statistics/tables/csv/f3-data.csv |
| `cgs_2y`, `cgs_3y`, `cgs_5y`, `cgs_10y` | RBA F2 daily, last trading day of each month: https://www.rba.gov.au/statistics/tables/csv/f2-data.csv and (pre-2013) https://www.rba.gov.au/statistics/tables/xls-hist/f02dhist.xls |

Government yields use the daily tables because the monthly F2 table is a monthly average, which would not line up with F3's month-end values.
Source: Reserve Bank of Australia.