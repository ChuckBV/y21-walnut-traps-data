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
library(FSA) # for se() function

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
#    the pacakage FSA; see "https://www.rcompanion.org/rcompanion/a_02.html".
#    We can skip that here.

### How I use ggplot2

### I use ggplot2 for both working graphs and graphs for presentations. Base 
### R provides quick and useful but ugly graphs through plot() and histogram()
### But I must remember ggplot2 because I need the presentation plots, and it
### is simpler to use just one package even if the base functions are simpler.

### In the current data set, there are two logical ways that one might compare:
### trap types overall, and how trap types change over time. Both are best done
### with pre-processing.

### Season-long lures: We need to
### 1) get one number for the 6 ovibait traps per sight--an average
### 2) get one season-long number for each site (part of that has to do with
###    degrees of freedom, statistical comparisons, and pseudo-replication)
### We use dplyr for that splicing and dicing

vbar_dat <- x %>% 
  # Start with one value for ovibait traps per site per week
  # We will do an average of for all trap types, but it does not change the
  # value for an n of 1
  group_by(site,date_out,trap_type) %>% 
  summarise(now_total = mean(now_total, na.rm = TRUE))
  # na.rm = TRUE is the default behavior, means including an na are thrown out
  # That is fine with only 2 NA values in 912 observations. Date to this point
  # looks like:
# A tibble: 342 x 4
# Groups:   site, date_out [114]
#   site  date_out   trap_type now_total
#   <chr> <date>     <chr>         <dbl>
# 1 Fox   2021-05-10 Biolure       39   
# 2 Fox   2021-05-10 Ovibait        8.83
# 3 Fox   2021-05-10 PPO            2  

  # Now we take out date_out so we get a mean for each trap type
vbar_dat <- vbar_dat %>% 
  group_by(site,trap_type) %>% 
  summarise(now_total = mean(now_total))
vbar_dat
  # Now down to 18 values, 6 sites (reps) for each trap_type. The sites are
  # the true replicates, and the standard error value that will be used in 
  # the graph needed to be based on that.
# A tibble: 18 x 3
# Groups:   site [6]
#   site   trap_type now_total
#   <chr>  <chr>         <dbl>
# 1 Fox    Biolure      29.6  
# 2 Fox    Ovibait       1.87 
# 3 Fox    PPO           1.72 
# 4 Hutson Biolure       9.63 
# 5 Hutson Ovibait       1.71 

  # Now to get mean and standard error by trap type. As best practice, we will
  # also get n (called nObs here). Note that we are using se() from the FSA
  # package. Base R provides standard deviation, but use of standard error in
  # plots is more common in my discipline. Conversion from SD to SE is simple
  # math and can be coded, but using an existing function keeps it simple and
  # reduces the possibility for introduced errors.
vbar_tbl <- vbar_dat %>% 
  group_by(trap_type) %>% 
  summarise(nObs = sum(!is.na(now_total)), # n() is easier and works when there is no NA
            mn = mean(now_total, na.rm = FALSE),
            se = FSA::se(now_total))
vbar_tbl
# A tibble: 3 x 4
#   trap_type  nObs    mn    se
#   <chr>     <int> <dbl> <dbl>
# 1 Biolure       6 22.2  4.43 
# 2 Ovibait       6  1.27 0.322
# 3 PPO           6  5.21 1.82 

  # vbar_tbl would be used for a table or a graph, but vbar_dat is what would 
  # be used for a statistical procedure such as ANOVA

  # Statistical comparison--one of the assumptions for ANOVA is that your
  # different means have the same variance. The very different standard errors
  # suggest that assumption is violated. There are various ways of dealing with
  # that--transformation (more frowned on these days than in the past), use
  # of a different distribution frequency (generalized linear models with
  # Poisson or negative binomial distribution), or non-paramatric statistical.
  # Here we will use the non-parametric Kruswal Wallis test we saw in 
  # DescTools. From "https://www.rcompanion.org/rcompanion/a_02.html"

kruskal.test(now_total ~ trap_type,
             data = vbar_dat)
# data:  now_total by trap_type
# Kruskal-Wallis chi-squared = 12.117, df = 2, p-value = 0.002338

  # For the kruskal-wallis test the df are based on the number of of categories,
  # 3 categories so 2 df. p-values < 0.05 are considered significant, so there
  # are differences. The Dunn test is used to determine which means are different

vbar_dat$trap_type <- factor(vbar_dat$trap_type, levels = c("Biolure","PPO","Ovibait"))
  # this was done simpple to get the categories on order from largest to smallest

PT = FSA::dunnTest(now_total ~trap_type,
                   data = vbar_dat,
                   method = "bh")

PT
#          Comparison         Z      P.unadj       P.adj
# 1 Biolure - Ovibait  3.460724 0.0005387255 0.001616177
# 2     Biolure - PPO  2.054805 0.0398978752 0.059846813
# 3     Ovibait - PPO -1.405919 0.1597481837 0.159748184

   # as it turns out, if we consider only the 6 sites in the southern SJV,
   # the only significant difference is that pheromone traps captured more
   # than ovibait traps (P.adj < 0.05). Biolure is not significantly different
   # from either. Mean separators would be a,ab,b

vbar_tbl$trap_type <- factor(vbar_tbl$trap_type, levels = c("Biolure","PPO","Ovibait"))
# this was done simply to get the categories on order from largest to smallest

subscr <- c("a","ab","b")

vbar_tbl <- cbind(vbar_tbl,subscr) 
vbar_tbl
#   trap_type nObs        mn       se subscr
# 1   Biolure    6 22.249708 4.425150      a
# 2       PPO    6  5.214460 1.821257     ab
# 3   Ovibait    6  1.273032 0.321722      b

### Make vertical error bar graph
p1 <- ggplot(vbar_tbl, aes(x = trap_type, y = mn)) +
  geom_col() + 
  # makes bars
  geom_errorbar(aes(ymax = mn + se, ymin = mn - se), width = 0.5) + 
  # makes errror bars, must be done in aes())
  geom_text(aes(label = subscr), vjust = -5) +
  # https://r-graphics.org/recipe-bar-graph-labels, section 3.92
  # scale for vjust not clear, adjustment on trial and error based on ggsave
  ylim(0,35) + 
  # so all superscripts show
  theme_bw() + 
  # A style preference, from gray to white background
  # below, xlab and ylab for to make a df$var into "Variable" for good plotting
  xlab("Attractant") +
  ylab("Moths/trap") +
  # The rest of this sets the size of the text and the results will be based
  # partially on the dimensions set in ggsave below. This is copied and pasted
  # from one time to the next
  theme(axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p1
  # Note that what you see on the plots windo is not the same as what you will
  # see after ggsave. Optimizing based on the plot window can make problems 
  # with the saved graft

# ggsave--the final version is not determined until size and density are specified
ggsave(filename = "plot1_vertical_bar_std_error.jpg", p1,
       #path = "../Desktop", 
       # I use path if there is a ./results subdirectory, here irrelevant
       width = 2.83, height = 2.83, dpi = 300, units = "in", device='jpg')
       # the size and density information provides the context for all other formatting
  
  