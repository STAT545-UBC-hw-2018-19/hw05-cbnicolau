Homework 05: Factor and figure management
================
Coni
October 16, 2018

-   [Homework 05: Factor and figure management](#homework-05-factor-and-figure-management)
    -   [Part 1: Factor management](#part-1-factor-management)
    -   [Part 2: File I/O](#part-2-file-io)
    -   [Part 3: Visualization design](#part-3-visualization-design)
    -   [Part 4: Writing figures to file](#part-4-writing-figures-to-file)

Homework 05: Factor and figure management
=========================================

Load required libraries

``` r
library(gapminder)
library(tidyverse)
library(forcats)
```

Part 1: Factor management
-------------------------

First check the structure of the data set and ensure there are variables which are **factors**

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

We can see that indeed the variables `country` and `continent` are factors

1.  Drop factor / levels;

**Drop Oceania.** Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.

Let's filter the data first

``` r
(gapminder_notOceania <- gapminder %>% 
  filter(continent != "Oceania"))
```

    ## # A tibble: 1,680 x 6
    ##    country     continent  year lifeExp      pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.
    ## # ... with 1,670 more rows

And investigate it's structure

``` r
str(gapminder_notOceania) 
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
levels(gapminder_notOceania$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
nlevels(gapminder_notOceania$continent)
```

    ## [1] 5

We can see from looking at the structure that Oceania is still a factor, but because `gapminder_notOceania` has fewer observations (1680 rows) that the unmodified `gapminder` (1704 rows), we can see that we effectively filtered out all the cuntries in Oceania

Let's confirm this with a plot:

``` r
ggplot(gapminder_notOceania, aes(continent)) +
  geom_bar() +
  scale_x_discrete(drop=FALSE) #to prevent ggplot from dropping the unused factors automatically
```

![](hw05-cbnicolau_files/figure-markdown_github/unnamed-chunk-4-1.png)

Now let's drop the unused factors

``` r
gapminder_notOceania %>%
  droplevels() %>%
  str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

Now we see that the variable `continent` has only 4 factors.

1.  Reorder levels based on knowledge from data.

**Reorder the levels of country or continent.** Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median.

Let's filter the data for just the year 2007, and reorder the plot

``` r
gapminder_notOceania %>%
  filter(year == 2007) %>%
  mutate(continent = fct_reorder(continent,gdpPercap)) %>% #reorder according to the median, increasing
  ggplot(aes(continent, gdpPercap)) +
  geom_boxplot() +
  theme_bw()
```

![](hw05-cbnicolau_files/figure-markdown_github/unnamed-chunk-6-1.png)

Now let's try reordering according to the mean

``` r
#gapminder_notOceania %>%
#  filter(year == 2007) %>%
#  mutate(continent = fct_reorder(continent,gdpPercap,fun = mean) %>% #reorder according to the mean, increasing
#  ggplot(aes(continent, gdpPercap)) +
#  geom_boxplot() +
#  theme_bw()
```

*This didn't work for some reason...*

Be sure to also characterize the (derived) data before and after your factor re-leveling:

1.  Explore the effects of arrange(). Does merely arranging the data have any effect on, say, a figure?

2.  Explore the effects of reordering a factor and factor reordering coupled with arrange(). Especially, what effect does this have on a figure?

These explorations should involve the data, the factor levels, and some figures.

Part 2: File I/O
----------------

Experiment with one or more of write\_csv()/read\_csv() (and/or TSV friends), saveRDS()/readRDS(), dput()/dget(). Create something new, probably by filtering or grouped-summarization of Singer or Gapminder. I highly recommend you fiddle with the factor levels, i.e. make them non-alphabetical (see previous section). Explore whether this survives the round trip of writing to file then reading back in.

Part 3: Visualization design
----------------------------

Remake at least one figure or create a new one, in light of something you learned in the recent class meetings about visualization design and color. Maybe juxtapose your first attempt and what you obtained after some time spent working on it. Reflect on the differences. If using Gapminder, you can use the country or continent color scheme that ships with Gapminder. Consult the dimensions listed in All the Graph Things.

Then, make a new graph by converting this visual (or another, if you’d like) to a plotly graph. What are some things that plotly makes possible, that are not possible with a regular ggplot2 graph?

Part 4: Writing figures to file
-------------------------------

Use ggsave() to explicitly save a plot to file. Then use `![Alt text](/path/to/img.png)` to load and embed it in your report. You can play around with various options, such as:

-   Arguments of ggsave(), such as width, height, resolution or text scaling.
-   Various graphics devices, e.g. a vector vs. raster format.
-   Explicit provision of the plot object p via ggsave(..., plot = p). - Show a situation in which this actually matters.
