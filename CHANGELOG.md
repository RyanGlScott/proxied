## next
* Update for GHC 8.2
  * Since `typeRep#` is no longer exported from `base`, `theTypeRep#` is now a
    synonym for `theTypeRep`
  * Happily, the new type signature for `GHC.OverloadedLabels.fromLabel` is now
    exactly the same as `theFromLabel`, so the latter is now a synonym for the
    former

## 0.2
* Added the `Data.Proxyless` module
* Added `proxyHashed` to `Data.Proxied`

### 0.1.1
* Enabled `Safe`

## 0.1
* Initial commit
