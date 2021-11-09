iteration\_and\_listcols
================
Yang Gao
11/9/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
set.seed(1)
```

## Lists

``` r
l=
  list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, TRUE, FALSE),
  summary= summary(rnorm(1000, mean = 5, sd = 3))
  )

l[[2]]
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.024   2.908   4.894   4.965   7.065  16.431

## Define functions

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

## list of normals

``` r
list_norms = 
  list(
    a=rnorm(100, mean = 5, sd = 3),
    b=rnorm(100, mean = 10, sd = 3),
    c=rnorm(100, mean = 2, sd = 1),
    d=rnorm(100, mean = 12, sd = 0.5)
    
  )

mean_and_sd(list_norms[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.97  3.09

## for loops

``` r
output = vector("list", length = 4)

output[[1]] = mean_and_sd(list_norms[[1]])



for( i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.97  3.09
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.54  2.96
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24  1.08
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.9 0.431

## using map instead

``` r
output = map(list_norms, mean_and_sd)

output = map(list_norms, median)

output = map(list_norms, summary)


output = map_dbl(list_norms, median, .id = "input")
```

## List columns!!!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norms
  )


listcol_df %>% 
  filter(name =="a")
```

    ## # A tibble: 1 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [100]>

``` r
listcol_df %>%  pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##   [1]  8.40489527  8.33579554  2.38766710  5.63219476  5.20818694  0.01205344
    ##   [7]  7.43251994 -0.73703739  1.25973971  7.99446334  3.37738177  4.35087263
    ##  [13]  0.13418812  0.64710810  6.05272919  4.47635921  3.22571459  0.99791822
    ##  [19]  1.70810450 11.10831083  4.02053122  7.32201564  7.35501920  7.28973824
    ##  [25]  5.88442628  1.24293223  1.97148874  7.25417358  1.07493946  6.58262029
    ##  [31]  3.39938128  3.80487196  2.63129165  4.30957659  7.63155452  6.36119953
    ##  [37]  4.30260756  7.61001657  9.96801120  4.98089321  6.41146836  5.83465595
    ##  [43]  2.06629118  2.22024157 10.75931139  7.64383336  7.22624532  5.44272021
    ##  [49]  6.45616569  5.45556812  5.12599626  5.67026694  1.96860474 12.20366631
    ##  [55]  7.40588537  4.24637612  8.63866811  3.11822574 10.13347552  3.81687934
    ##  [61] -1.96447257  9.09235759  8.39668740  2.67705104  0.76887510 -0.50358274
    ##  [67]  4.19295939 -0.50178573  2.55659594  5.49071637  7.56655767  2.54011062
    ##  [73]  4.62919172  5.76484471 10.15677901  2.12436942  0.18706921 -0.53682827
    ##  [79]  6.66721156  4.81964243  7.31625891  4.57748184  6.17928178  5.67265572
    ##  [85]  5.07062596  3.13111202  8.78602814  3.78267787  7.00029131  5.49391746
    ##  [91] 10.34457342  7.13364189  3.98692653  4.97255314  4.62407237 -1.27253829
    ##  [97] 10.09218168  8.19164346  2.70015009  6.14602268
    ## 
    ## $b
    ##   [1] 10.7256877  6.6017218 14.4697222  9.2552587 10.5507511 11.2146130
    ##   [7]  7.0176266  6.7437120  9.8543723 11.7282568 10.2214916 12.1178367
    ##  [13] 11.0049403 11.6361634  5.7912823 12.0311617  7.6305987  8.6028133
    ##  [19]  9.6854438  5.0564467  9.7013891  8.6804271  7.8444656  8.3362072
    ##  [25] 13.7364675  6.2232359  9.3538466  2.5841149  7.9774920  8.4961084
    ##  [31] 14.6269774  7.1139458  7.3834614  5.8071111 10.5394155 13.4622760
    ##  [37]  6.4043992  8.7228268 14.0989258  7.9471078 12.0565366 11.1685106
    ##  [43]  6.0838122 13.6506640 12.3855221  8.5353925  7.2880196  8.8287447
    ##  [49] 12.4421902  8.3072522  4.3773840  9.5712859 12.3157120  6.5226462
    ##  [55]  9.2862534  6.3334200 10.3504186  9.6025011  9.8989457  8.1330207
    ##  [61]  7.8719096 12.6143362 10.3154141  9.4391942  0.3604344  6.1731439
    ##  [67] 12.2887190  8.7795546  6.3750466  8.6820320  8.8732578  8.4956728
    ##  [73] 11.4898377 14.5626428 12.9642755 13.7383676  9.0103980 12.5330341
    ##  [79]  7.0567727  9.5823358 16.5563137  9.9615160  9.0840732  8.2473596
    ##  [85] 12.3138055 16.3185681 11.2364711  9.2162054 16.2213509  7.6635093
    ##  [91] 13.3945975  8.7359646  6.9347579 13.6549161  4.6007180  9.0752502
    ##  [97] 10.0465457  8.6730468  5.0859768  8.0757965
    ## 
    ## $c
    ##   [1]  0.4429643  3.9231637  0.1431704 -0.1061184  2.6976485  2.9074444
    ##   [7]  1.8040118  1.7931795  2.7250432  3.3987190  0.4094449  3.3044971
    ##  [13]  2.1960117  1.6606566  3.2651441  2.9397737  3.2779525  1.7088399
    ##  [19]  2.7761724  2.2957256  1.4828004  3.7572330  2.1617268  1.9453156
    ##  [25]  2.5289716  2.3897345  1.2942286  1.9468215  4.6064445  1.9274069
    ##  [31]  1.7037281  2.7343303  2.4014807  1.4616989  0.9172772  2.8478014
    ##  [37]  2.1188450  2.4822472  1.7086752  0.3478897  2.0690946  4.3905767
    ##  [43]  0.5382283  1.0151937  0.6639463  1.6348684  3.3814542  1.8466827
    ##  [49]  1.7444284  0.7113729  2.0652664  3.0353264  4.2602158  3.3146963
    ##  [55]  1.1299767  1.4968700  2.6074556  1.9899586  2.2753303  0.6877012
    ##  [61]  1.5734701  1.0735958  1.0714421  1.2810994  1.5852094  1.9943321
    ##  [67]  2.6334489  1.4908192  1.1429914  3.6161676  2.9938664  2.6968459
    ##  [73]  3.6999707  1.0222581  4.0488176  3.2264493  2.3070230  2.6243330
    ##  [79]  2.0613414  1.8891742  0.4237664  2.7425898  4.1423234  4.0897730
    ##  [85]  2.1697943  1.8921222  2.1819945  3.1459282  3.4772988  2.4079442
    ##  [91]  3.1142787  1.9729242  2.4977222  3.1856400  5.6395736  1.9459974
    ##  [97]  1.3318527  2.4458080  1.5941748  2.6352831
    ## 
    ## $d
    ##   [1] 12.17062 12.65808 11.52011 11.39721 12.78379 12.11264 11.53879 11.46311
    ##   [9] 11.72382 12.29523 11.73636 12.67092 12.12192 12.09794 11.99224 11.85121
    ##  [17] 11.91616 12.57824 11.33062 12.54338 11.82466 12.36736 11.97079 11.32709
    ##  [25] 12.17345 11.68080 12.56220 11.58613 11.10611 11.57299 11.71399 11.58703
    ##  [33] 11.88121 11.70234 11.79463 12.16400 11.41818 12.38009 11.99872 11.98089
    ##  [41] 10.98188 11.84939 11.72218 12.42365 12.69541 12.61562 11.44333 11.60197
    ##  [49] 11.95524 11.47017 11.19784 12.39568 12.03408 12.30720 11.39847 11.82977
    ##  [57] 11.41438 11.58529 12.11461 11.59250 11.50328 11.34692 11.28289 11.56858
    ##  [65] 12.85190 11.67218 11.44342 11.29725 11.92468 11.80300 12.12603 11.85865
    ##  [73] 12.43383 12.36005 12.52803 12.12175 12.55718 11.96172 12.46646 11.92101
    ##  [81] 11.65925 11.38995 11.64554 11.62566 12.28804 11.97395 11.68483 11.55083
    ##  [89] 12.75634 11.88163 11.37560 11.78500 12.04665 12.41652 11.95753 12.05593
    ##  [97] 11.48028 12.39821 12.04995 12.36848

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.97  3.09

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.97  3.09
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.54  2.96
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24  1.08
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.9 0.431

``` r
#could store the results as a new list column … !!!

listcol_df_new = 
  listcol_df %>% 
  mutate(summary = map(samp, mean_and_sd))

listcol_df_new
```

    ## # A tibble: 4 × 3
    ##   name  samp         summary         
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [100]>  <tibble [1 × 2]>
    ## 2 b     <dbl [100]>  <tibble [1 × 2]>
    ## 3 c     <dbl [100]>  <tibble [1 × 2]>
    ## 4 d     <dbl [100]>  <tibble [1 × 2]>

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-05 10:31:13 (7.602)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-05 10:31:22 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-05 10:31:26 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-09-30

``` r
weather_nest = 
  nest(weather_df, data = date:tmin)

weather_nest %>% filter(name=="CentralPark_NY") %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

``` r
unnest(weather_nest, cols = data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
lm(tmax ~tmin, data = weather_nest$data[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
lm(tmax ~tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
weather_lm = function(df){
  
  lm(tmax~tmin, data = df)
}

weather_lm (weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nest = weather_nest %>% 
  mutate(lm_result = map(data, weather_lm))
```

``` r
read_page_reviews = function(url) {
  
  html = read_html(url)
  
  title = 
    html %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  text = 
    html %>%
    html_nodes(".review-data:nth-child(5)") %>%
    html_text()
  
  tibble(title, stars, text)
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)



nap_df = 
  tibble(
    vec_urls = vec_urls
  )

nap_df %>% 
  mutate(reviews = map(vec_urls, read_page_reviews)) %>% 
  select(reviews)
```

    ## # A tibble: 5 × 1
    ##   reviews          
    ##   <list>           
    ## 1 <tibble [10 × 3]>
    ## 2 <tibble [10 × 3]>
    ## 3 <tibble [10 × 3]>
    ## 4 <tibble [10 × 3]>
    ## 5 <tibble [10 × 3]>

``` r
output = vector("list", 5)

for (i in 1:5) {
  output[[i]] = read_page_reviews(vec_urls[[i]])
}

dynamite_reviews = bind_rows(output)

dynamite_reviews = map_df(vec_urls, read_page_reviews)
```
