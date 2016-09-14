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
