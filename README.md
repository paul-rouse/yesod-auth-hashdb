yesod-auth-hashdb
=================

This is the Yesod.Auth.HashDB plugin, which used to be part of yesod-auth prior to version 1.3, but has now been moved out of the main package.

Versions of this plugin prior to its removal from yesod-auth used a relatively weak hashing algorithm (a single round of SHA1) which does not provide adequate protection against an attacker who discovers the hashed passwords.  See: <https://github.com/yesodweb/yesod/issues/668>.

It has now been rewritten to use Crypto.PasswordStore, which provides stronger protection against cracking passwords.

See the module documentation for further details.


