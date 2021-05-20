## 1.7.1.7

* Remove upper bound on `persistent-sqlite` in tests

## 1.7.1.6

* Bump upper bound on `yesod-form` to allow 1.7

## 1.7.1.3

* Support `persistent-2.11` [#8](https://github.com/paul-rouse/yesod-auth-hashdb/pull/8)

## 1.7.1.2

* Fix test to allow use of persistent-template-2.8

## 1.7.1.1

* Fix test and relax upper bound for persistent-2.10 / persistent-template-2.7
* Replace use of deprecated `requireJsonBody`

## 1.7.1

* Relax upper bounds to allow persistent-2.9 (for GHC 8.6 versions of Stackage nightly)
* Remove testing of GHC below 8.0.2, and lts below 9

## 1.7

* Update for changes in yesod version 1.6, but retain compatibility with previous versions
* Remove support for GHC below 7.10, and lts below 6

## 1.6.2

* Use `PasswordStore` from `yesod-auth` instead of `pwstore-fast` (uses `cryptonite` instead of `cryptohash`)

## 1.6.1

* Relax upper bound on persistent

## 1.6.0.1

* Fix serious documentation layout problem caused by typo

## 1.6

This release completes the breaking changes started in 1.5.  For details
of upgrading, please see
[Upgrading.md](https://github.com/paul-rouse/yesod-auth-hashdb/blob/master/Upgrading.md).

* Complete removal of compatibility with old databases designed for versions before 1.3
* Add JSON support

## 1.5.1.3

* Fix test failure with basic-prelude >= 0.6 (#6)

## 1.5.1.2

* Relax upper bound to allow persistent-2.6

## 1.5.1.1

* Minor documentation improvement
* Reduce external-library dependencies for tests

## 1.5.1

* Include CSRF token in default form

## 1.5

This release can break both old code and old database entries.  For details
of upgrading, please see
[Upgrading.md](https://github.com/paul-rouse/yesod-auth-hashdb/blob/master/Upgrading.md).

* First phase of removing compatibility with old databases designed for versions before 1.3
* Remove deprecated utilities (`getAuthIdHashDB` and pre-defined `User` data type)

## 1.4.3

* Changes to work with persistent-2.5

## 1.4.2.2

* Relax upper bound to allow persistent-2.2.*

## 1.4.2.1

* Add ChangeLog

## 1.4.2

* Deprecate `getAuthIdHashDB` (see [#5](https://github.com/paul-rouse/yesod-auth-hashdb/issues/5))

## 1.4.1.2

* Use internationalized messages
* Increase `defaultStrength`

## 1.4.1.1

* Minor documentation change

## 1.4.1

* Expose additional validation function which does not need to read the database
* Deprecate compatibility with old data which includes a salt field

## 1.4.0

* Changes for Yesod 1.4

## 1.3.2

* Documentation improvement

## 1.3.1

* Optional custom login form
* Deprecate predefined `User` data type
* Changes for Persistent 2

## 1.3.0.1

* Version bounds
* Minor documentation changes

## 1.3

* First release as a separate package, not part of yesod-auth
