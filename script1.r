#---------------------------------------------------------------------------#
# script1.r
# 
# Objective: Demonstrate what I (Chuck Burks) consider best practices in
# approaching data sets and useful feature of ggplot2 and other parts of
# the tidyverse
#
#---------------------------------------------------------------------------#

library(tidyverse)
library(DescTools)

### Get overview of the example data set. How many columns? How many rows?
### What type of variables? What seem to be useful questions

x <- read_csv("y21-walnut-traps-ssjv.csv")
x
# A tibble: 912 x 6
#   site  date_in    date_out   trap_type trap_numer now_total
#   <chr> <date>     <date>     <chr>          <dbl>     <dbl>
# 1 Fox   2021-05-03 2021-05-10 Ovibait            1         6
# 2 Fox   2021-05-03 2021-05-10 Ovibait            2         9
# 3 Fox   2021-05-03 2021-05-10 Ovibait            3        16

tail(x,3)
# A tibble: 3 x 6
#   site   date_in    date_out   trap_type trap_numer now_total
#   <chr>  <date>     <date>     <chr>          <dbl>     <dbl>
# 1 Hutson 2021-10-04 2021-11-03 Ovibait            6         0
# 2 Hutson 2021-10-04 2021-11-03 PPO               NA        29
# 3 Hutson 2021-10-04 2021-11-03 Biolure           NA         1

### So these are trapping data from May to November. The variable "now_total"
### is apparently all NOW (regardless of sex) on a trap. For each date at
### each site there are 6 values for Ovibait, 1 for Biolure, and 1 for PPO.
### The date in and date out values are about a week apart, so apparently this
### is weekly trapping data

### I frequently use the package lubridate with this type of data; not only
### to allow R to recognize non-ISO formats as dates but also for functions
### like yday() (day of the year as integer 1-365, aka Julian date) and 
### epiweek() (the week of the year as an integer from 1 to 52). Here we won't
### mess with it.

### Before we graph the data set it is good to get a rough idea of the
### relationship between the variables and the number of NA values. The 
### DescTools package can help with that.

### Values of now_total by site
Desc(now_total ~ site, data = x)
#    No copy-and-paste here, but the data displayed in the console and in
#    plots is cool. This tells use there are 6 sites, only two NAs for 
#    now_total in the 912 rows, and there is significant variation in 
#    now_total between the sites.

### Values of now_total by date_out
Desc(now_total ~ date_out, data = x)
#    25 groups (i.e., weeks), also significant variation between weeks

### Values of now_total by trap_type
Desc(now_total ~ trap_type, data = x)
#    Biolure captures more than ovibait captured more than PPO. The 
#    Kruskal-Wallis test only tells about differences between the three 
#    categories. With this non-parametric test you would need the Dunn test
#    to distinguish between the individaul categories. I usually do that with
#    the pacakage FSA; see ""






