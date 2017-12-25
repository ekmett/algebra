4.3.1
-----
* Compatibility with GHC 8.4.x: added `Semigroup (End r)` instance.

4.3
---
* Compatibility with GHC 8.0.x
* Dropped incomplete instance for `Algebra r (Map a b)` instance
* Restructured Ring hierarchy (Thanks @dfoxfranke!)
* Added DecidableNilpotence class (Thanks @dfoxfranke!)

4.2
---
* Support for `nats` version 1 and `base` 4.8's version of `Numeric.Natural`. This required monomorphizing some stuff to `Natural`, but that is more accurate than the previous hack anyways.

4.1
---
* Added Euclidean domains and the field of fractions.

4.0
---
* Compatibility with GHC 7.8.x
* Removed `keyed` and `representable-tries` dependencies

3.0.2
-----
* Started CHANGELOG
