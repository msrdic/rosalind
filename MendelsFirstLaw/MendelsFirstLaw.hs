module MendelsFirstLaw where

hd, hz, hr :: Double

hd = 22 -- homozygous dominant
hz = 29 -- heterozygous
hr = 15 -- homozygous recessive

-- I did this by hand

main = do
  let population = hd + hz + hr

  -- homozygous dominant branch
  let b1 = hd * (hd - 1 + hz + hr) / (population * (population - 1))

  -- heterozygous branch
  let b2 = hz * ((12 * hd) + (9 * (hz - 1)) + (6 * hr)) / (12 * population * (population - 1))

  -- homozygous recessive
  let b3 = hr * ((2 * hd) + hz) / (2 * population * (population - 1))

  print $ "population: " ++ show population
  print $ "homozygous dominant branch: " ++ show b1
  print $ "heterozygous branch: " ++ show b2
  print $ "homozygous recessive: " ++ show b3

  print $ "result: " ++ show (b1 + b2 + b3)
