yesod-auth-hashdb
=================

This is the Yesod.Auth.HashDB plugin, which used to be part of yesod-auth prior to version 1.3, but has now been moved out of main package.

Versions of this plugin prior to its removal from yesod-auth used a relatively weak hashing algorithm (a single round of SHA1) which does not provide adequate protection against an attacker who discovers the hashed passwords.  See: <https://github.com/yesodweb/yesod/issues/668>.

It has now been rewritten to use Crypto.PasswordStore, but this has been done in a way which preserves compatibility both with the API and with databases which have been set up using older versions of this module.

See the module documentation for further details.


