# fst - Fuzzy Set Theory implementation in Haskell

## Description

If *X* is a collection of objects denoted generically by *x*, then a fuzzy set *F(A)* in *X* is a set of ordered pairs. Each of them consists of an element *x* and a membership function which maps *x* to the membership space *M*. The current implementation is inspired by the work of J.A. Goguen **L-Fuzzy Sets** [[1](#lfuzzysets)].

## Setup 

This library could be easily compiled using **cabal** issuing the command `cabal build` in the root directory.

## Usage

Using this library you can easily apply the **Extension Principle** [[2](#fuzzysets)] in the following way:
```
let fuzzy_set = fromList [(-1, Z 0.5), (0, Z 0.8), (1, Z 1.0), (2, Z 0.4)]

fmap (^2) fuzzy_set 

> FuzzySet {(0, Z 0.8), (1, Z 1.0), (4, Z 0.4)}
```

## Authors

All the following authors have equally contributed to this project (listed in alphabetical order by surname):

- Marco Di Pietro ([github](https://github.com/mdip))
- Claudio Greco ([github](https://github.com/claudiogreco))
- Corrado Mencar ([webpage](https://sites.google.com/site/cilabuniba/people/faculty/mencar))
- Alessandro Suglia ([github](https://github.com/aleSuglia))
 
## References
<a name="lfuzzysets">[1]:</a> [Goguen, Joseph A. "L-fuzzy sets." Journal of mathematical analysis and applications (1967)](http://www.sciencedirect.com/science/article/pii/0022247X67901898)

<a name="fuzzysets">[2]:</a> [Zadeh, Lotfi A. "Fuzzy sets." Information and control (1965)](http://www.sciencedirect.com/science/article/pii/S001999586590241X)

