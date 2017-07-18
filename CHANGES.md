## Version 0.8 (next)

* Add simple constraint solver

## Version 0.7 (2017-06-23)

* Utils
   * New flow operators (>.~!!> and all the MaybeCatchable variants)
   * Preliminary continuation stuff
* Various
   * Package renamed from ViperVM to haskus-system
   * Package split into haskus-utils, haskus-binary and haskus-system
   * Switch to GHC 8.0
   * Most uses of Proxy have been replaced with type applications
   * Tests now use Tasty
   * Removed useless dependencies
   * Add Travis CI support


## Version 0.6 (2016-11-13)

* Switch to BSD-3-clause license
* Utils
   * STM Tree data structure
   * New flow operators: const variants
   * Add Tuple, List, Maybe, etc. modules
   * (Embed) Embed bytes
   * (Monad) Use MonadIO and MonadInIO to support both Sys and IO monads
* Some tests added

## Version 0.4 (2016-06-10)

* Haskus.Utils
    * Variant: type-safe open sum type
    * Flow: generic function composition combinators based on Variants
