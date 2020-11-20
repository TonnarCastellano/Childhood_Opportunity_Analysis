Team Five EDA
================




# New York City

## Introduction

For the other cities we choose to the surrounding areas as it can be
hard to break up the city itself into very distinct areas. However, New
York city is a bit odd in this respect because it so easily is broken up
into the five borrows. These five boroughs have unique characteristics
that are represented similarly as suburban and other surrounding areas
would be for other major metropolitan areas. As such, we decided to opt
for looking at the education and economic opportunities exclusively
within New York City. We believe that this better represents our
motivating question because it would allow us to slice the data across
distinct axis while still retaining the heart of where is the most
opportunity.

## Life in The City

Life quality is in a city is an exceptionally hard thing to measure and
will tend to vary from person to person. While quality of life might
seem like an extraneous variable it is intimately linked to success even
if it is not always in an obvious manner. Unfortunately, our study is
focused more on the educational and economic indicators of success in a
city. However, the data as mentioned is still exceptionally important so
while not the main focus on the analysis it is still worth including if
for no other reason than to paint a picture of the overall understanding
of the boroughs in the city.

### Commutes and Walkability


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> 

The first part of the analysis will link the walkability. As a general rule
of thumb big cities tend to be more “walkable”. As such, it seems there
should be a direct negative relationship between the commute\_over1hour
and the walkability of the city. It seems however that most of the
boroughs in the city tend to be equally walkable and have similar
percentages of commutes over an hour.

### Green Spaces and Pollution

![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-14-1.png)<!-- --> 

It does seem that there also a lack of relationship between the number of
green spaces and the mean micro-particles i.e. pollution. The amount of
pollution does seem to be fairly high in all of them expect Staten
Island which consequently appears to exhibit the least amount of green
spaces. However, it is worth mentioning that might be skewed for
Manhattan due to Central Park.

### Neighborhood Population and Poverty

![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-15-1.png)<!-- --> 

The last factor worth examining in city life is the number of impoverished
people and the amount of housing vacancy. Here we do see a bit more of a
relationship with less poverty as a general role of them leading to less
vacancy. This should not be a surprising trend. We do seem to see not as
stark of a relationship as might be viewed in cities with more space for
houses than New York.

### Education Centers


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

The last factor is a perfect sequeway to the topic of education. This
graph is fairly inline also with what we would expect to see which is
that as the income increases in the borough better education centers
appear.

## Childhood Education Status

### Math and Reading in Elementary School Students


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-18-2.png)<!-- --> 

The first and most obvious indicator of education success is how well
elementary students are doing in a given area. This obviously hinges on
numerous factors such as teachers and income just for starters. However,
first we will want to look at the trends and correlation between the two
factors and across the two years given in our data set.

As expected, reading and math scores at the third grade level track in
nearly lockstep across all boroughs. This makes sense because most of
the students have not specialized like they will in college and the
material at this level more tests cognitive ability than skills within
that field(find citation). However, what is interesting is the
dispersion of the data of exhibits a higher variance over time. Due to
the amount of points it can be hard to see who did better, worse, or
stayed the same.


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-19-2.png)<!-- --> 

Given our code above the next logical step is to break out the boroughs into
more easily identifiable parts while stilling maintaining a comparison
between them. The comparison is broken up by reading and math scores
independently because those made up the axis from the above graph. Two
major facets quickly become apparent in the data. The first is that math
scores trended down over time. The other major structure within the data
is how the five boroughs faired. Manhattan was clearly the best and the
Bronx was clearly the worst from this analysis.

Moving forward we will compare Manhattan and the Bronx because of the
stark differences it should be easier to find potentially patterns that
*could* cause or represent these differences.


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-20-1.png)<!-- --> 

The first and most obvious one to check is the levels of poverty exhibited
by the school. If Manhattan and the Bronx exhibit different levels of
poverty especially significant ones it could be a leading cause in what
causes the differences in math and reading ability. The graph above
which plots the frequency of which schools are afflicted with poverty
paints a clear picture that Manhattan has both less severe poverty and
less frequent poverty. However, it is possible that this maybe has no
affect on ability.


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-21-2.png)<!-- --> 

This final graph for this section helps us to confirm the **correlation**
between math and reading ability. It seems like there is a direct
connection between the amount of poverty and the ability of the
students. However, further testing would need to be done to establish
causation. One thing worth noting is that it seems that being more
impoverished causes worse reading but being wealthy causes better math.
Said another way math seems to do better if you are rich and reading
seems to do worse if you are poor.

## Economic Variablity in the City

### Median Income

![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-24-1.png)<!-- --> 

The first economic indicator of economic success seems pretty obvious - the
amount of money made in terms of the median. The mean is avoided here to
prevent extreme cases from driving up or down the amount. From the graph
the first two that stick out are Manhattan and the Bronx for the
opposite reasons. Manhattan seems to have more economic success and the
Bronx seems to be struggling. As the financial center of the world it
does not come as a surprise to Manhattans success. However, given the
proximity of the Bronx to Manhattan the reason for the Bronx’s struggle
is not as clear.


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

The first factor that one could guess that would potentially drive
economic success is college degrees(add citation). As such, it does not
come as a surprise that there appears to be a nearly one to one
relationship between the two. However, what is interesting is that on
the first graph Staten Island seemed to come in second in terms of
median income but appears to have less college degrees as a percentage
than Brooklyn(look more into this). Unsurprisingly Manhattan seems to
have the most college degrees and highest financial success.


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-26-1.png)<!-- --> 

What is interesting to note here is that there does not seem to be much in
the way of difference in terms of the employment rate. However, this
graph can be a bit misleading as everything seems to be high. 75%
unemployment is a very large portion of the population. To put this into
perspective, unemployment in the United States is generally around 90%.
Staten Island seems like the outliers here exhibiting a high income but
a lower rate of college degrees and employment (examine more).


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-27-1.png)<!-- --> 

This is one of the most interesting graphs. It appears from this graph that
the employment rate is nearly devoid of a relationship with a college
degree expect in the extreme cases of a lack of degree. However, it is
very possible that relationship is actually inverted with the employment
generating whether someone has the opportunity to go to college instead
of the other way around.


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-28-1.png)<!-- --> 

Given the relationship above it seems to make sense that perhaps is being a
high skilled worked that drives employment rate. However, that also does
not seem to be true expect again in the extreme case.


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

However, college degrees are not the sole measure for financial
stability. There are numerous other factors like being high skilled
worker which would include things like trade school. Here you do see a
bit more of a relationship between high skill and median income though
not one as stark as having a college degree.


![](Tonnar_Graphs_files/figure-gfm/unnamed-chunk-30-1.png)<!-- --> 

The last graph might seem like it belongs in livability which is an argument
is fairly solid. However, given that often times a house seems to be the
largest asset housing rates are also right at home in financial success.
While one might expect Manhattan to have the largest amount of homes it
is apparent this is not the case until you progress into the super
wealthy. Given the price of real estate in the city this makes sense.
The Bronx also matchs what you would expect as does Staten Island as
real estate prices drop.
