writing\_functions
================
Yang Gao
11/4/2021

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

## Z-scores

``` r
set.seed(1)

x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

``` r
#writing functions
z_scores = function(x) {
    z = (x - mean(x))/ sd(x)
  z}

z_scores (x = x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

## Multiple outputs

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) < 3) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
}

mean(x_vec)
```

    ## [1] 5.674661

``` r
sd(x_vec)
```

    ## [1] 3.800432

## Differnt sample sizes, means, sds

``` r
sim_data = tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )


sim_data %>% 
    summarize(
      mean= mean(x),
      sd = sd(x)
    )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.12  2.22

lets write a function that simulates data, computes the mean and sd

``` r
sim_mean_sd = function(n, mu, sigma){
  
  #check inputs
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )


  sim_data %>% 
    summarize(
      mean= mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(30,4,3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.26  3.09

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

write a fucntion that get reviews based on page url

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)
  
  review_titles = 
    page_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    page_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  review = 
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  return(review)
}


base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews (urls[1]),
  get_page_reviews (urls[2]),
  get_page_reviews (urls[3]),
  get_page_reviews (urls[4]),
  get_page_reviews (urls[5])
)
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 I Just everyone to know this....                          5 VOTE FOR PEDRO !…
    ##  2 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein…
    ##  3 Best quirky movie ever                                    5 You all know the…
    ##  4 Classic Film                                              5 Had to order thi…
    ##  5 hehehehe                                                  5 goodjobboys      
    ##  6 Painful                                                   1 I think I sneeze…
    ##  7 GRAND                                                     5 GRAND            
    ##  8 Hello, 90s                                                5 So nostalgic mov…
    ##  9 Cult Classic                                              5 Watched it with …
    ## 10 Format was inaccurate                                     4 There was an opt…
    ## # … with 40 more rows
