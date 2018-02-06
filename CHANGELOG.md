# CHANGELOG for cabal-toolkit

## 0.0.5

* Invoke `addForeignFile` when relying on the content of `.buildinfo` in TH splices, to ensure they are re-run when the files are updated. (Credits to @nh2)

## 0.0.4

* Support `Cabal-1.24` and `ghc-8.0.2`. (Credits to @sgraf812)

## 0.0.3

* Add `Program`s for `cmake`/`make`/`ninja` for users who need to compile C/C++.

## 0.0.2

* Add `getGHCPackageDBFlags` for users working with GHC API.

## 0.0.1

* Initial commit.
