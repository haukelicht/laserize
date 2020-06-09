# {Laserize}: An R package for obtain LASER text embeddings by interfacing the Python laserembeddings module

The **laserize** package provides an R interface to the `embed_sentence` functionality provided by the [`laserembeddings`](https://pypi.org/project/laserembeddings/, "PyPi: laserembeddings") Python module.
`laserembeddings` is a port of Facebook Research's [LASER](https://github.com/facebookresearch/LASER)

LASER provides models to compute multilingual sentence embeddings that are aligned in a common, language-independent vector space.
Sentences with similar semantics from different languages are thus mapped to "close" vectors.

The `embed_sentence` function of the `laserembeddings` Python module allows to obtain these vector representations based on the Facebook's pre-trained model.
The `laserize` package provides an itnerface to this functionality.

# Installation

```r
devtools::install_github("haukelicht/laserize")
```

# Usage

## Setup 

To setup laserize, use `setup_laser`. 
This *interactive* function downloads all required modules and LASER model.

```{r }
library(laserize)
setup_laser()
```

If provided a valid file path to its `.py.venv` argument,
`setup_laser` creates a Python virtual environment (if not already exists) at the desired location.
```{r }
library(laserize)
# with path to _existing_ Python vortual environment
setup_laser(.py.venv = "path/to/venv")
# with path to "dir" that should contain a _new_ Python vortual environment
setup_laser(.py.venv = "path/to/existing/dir/venv")
```

For mor information see `?laserize::setup_laser`.


## Embedding sentences 

```{r}
test_df <- tibble::tribble(
  ~id, ~text, ~lang,
  001, "Hallo Welt", "de",
  002, "Auf wiedersehen", "de",
  003, "Hello world", "en",
  004, "XXGWRXYYFGEG", "unkown",
)

# obtain LASER embeddings 
res <- laserize(test_df)
# 'res' is a name list with four elements
str(res, 1) 
# each list element is a list with elements 'id', 'text', 'lang', and 'e'
str(res[[1]], 1) 

# simplified output (matrix with IDs as row names)
res <- laserize(test_df, simplify = TRUE)
is.matrix(res) # a matrix
# rows as many as sentences in 'test_df',
# columns as many as embedding dimensions
dim(res)

# check sentence similarities
cosine_sim <- function(x, y) sum(x*y)/sqrt(sum(x**2)*sum(y**2))
# representations of greetings in German and English are very similar
cosine_sim(x = res[1, ], y = res[3, ])
# representations of German greeting and goodbye are somewhat dissimilar
cosine_sim(x = res[1, ], y = res[2, ])
```
