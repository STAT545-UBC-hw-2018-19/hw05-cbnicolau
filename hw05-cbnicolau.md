Homework 05: Factor and figure management
================
Coni
October 16, 2018

-   [Homework 05: Factor and figure management](#homework-05-factor-and-figure-management)
    -   [Part 1: Factor management](#part-1-factor-management)
        -   [Elaboration for the gapminder data set](#elaboration-for-the-gapminder-data-set)
    -   [Part 2: File I/O](#part-2-file-io)
    -   [Part 3: Visualization design](#part-3-visualization-design)
    -   [Part 4: Writing figures to file](#part-4-writing-figures-to-file)

Homework 05: Factor and figure management
=========================================

Part 1: Factor management
-------------------------

With the data set of your choice, after ensuring the variable(s) you’re exploring are indeed factors, you are expected to:

1.  Drop factor / levels;
2.  Reorder levels based on knowledge from data. We’ve elaborated on these steps for the gapminder and singer data sets below.

Be sure to also characterize the (derived) data before and after your factor re-leveling:

1.  Explore the effects of arrange(). Does merely arranging the data have any effect on, say, a figure?
2.  Explore the effects of reordering a factor and factor reordering coupled with arrange(). Especially, what effect does this have on a figure?

These explorations should involve the data, the factor levels, and some figures.

### Elaboration for the gapminder data set

**Drop Oceania.** Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.

**Reorder the levels of country or continent.** Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median.

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
