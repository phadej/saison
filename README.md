# Saison

Stream Aeson, fruity, spicy, well carbonated.

Saison represents JSON document as well-formed token stream.
This approach is potentially faster than document model (i.e. representing
document as an ADT) used in `aeson`. Also the approach is more
flexible, as more structure is preseved, especially key-value
pairs in records are not ordered.

Preliminary benchmarks, using `laureate.json`, a 438k JSON of Nobel laureates:

```
benchmarking ToValue/Aeson
time                 23.28 ms   (23.09 ms .. 23.46 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 23.89 ms   (23.65 ms .. 24.22 ms)
std dev              632.3 μs   (449.0 μs .. 950.4 μs)

benchmarking ToValue/Saison
time                 19.85 ms   (19.74 ms .. 19.98 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 20.05 ms   (19.96 ms .. 20.19 ms)
std dev              272.8 μs   (185.4 μs .. 382.8 μs)
```

Before package is uploaded to Hackage,
[haddocks are on my site](https://oleg.fi/haddocks/saison/)
