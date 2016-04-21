Upgrading to Version 1.5
========================

Version 1.5 starts the process of removing old, deprecated functionality.
This will be completed in version 1.6, which will probably be timed to
go into [LTS Haskell](https://github.com/fpco/lts-haskell#readme) 7.0.

Ever since 1.3, released two years ago, the salt for newly generated
passwords has been stored as part of the password hash, rather than as
a separate field.  However, for compatibility with databases designed
for earlier versions and password hashes produced by those versions,
the `HashDBUser` type class retained methods to set and get a separate
salt field.  These methods were explicitly deprecated in version 1.4.1,
and are now being removed in two stages.

The first stage is to remove the *setter* methods.  This is likely to
make old code fail in compilation now, providing a reminder that both
code and database need to be migrated urgently!  At the moment, the
*getter* for the salt remains a method of the type class, so any
existing old-style password hashes, with separate salt, can still be
verified.

The second stage, in version 1.6, will finally remove the *getter* for
the salt, and it will not be possible to use old-style password hashes
at all.

If your database model does *not* have a separate salt field, and you
do *not* declare `setSaltAndPasswordHash` or `setUserHashAndSalt` in
your `HashDBUser` instance, you can ignore the next section, otherwise
read on!

Additionally some utilities have been removed.  These were deprecated,
but were unrelated to the salt.  See the section at the end for details.

Compatibility with Old Databases
--------------------------------

The `HashDBUser` type class no longer has the method `setSaltAndPasswordHash`
or its older synonym `setUserHashAndSalt`.  These were used for *setting*
a separate salt field in a user record.  **If you see compilation errors
as a result of this change, you should take it as a final warning to complete
the migration of old database models!**

###Fixing the Compilation Error

Fixing the compilation error is easy: you must define `setPasswordHash`
instead.  Temporarily, while completing the migration, make sure that
`setPasswordHash` sets the salt field of the user to the **empty string**.

You should retain your implementation of `userPasswordSalt` until the
database migration has been completed, and users no longer have a
separate salt field.

As an example, you might have a `HashDBUser` instance something like this:

```
instance HashDBUser User where
    userPasswordHash = userPassword
    userPasswordSalt = userSalt       -- Until salt field removed completely
    setPasswordHash h u =
        u { userSalt     = Just "",   -- Until salt field removed completely
            userPassword = Just h
          }
```

###Migrating an Old Database

A new password must be set for any user who still has a non-empty salt
field; this will ensure that the salt is set to the empty string, which
is a special value indicating that it is unused.

1.  Inspect the database to determine if any user still has
    non-empty salt in a separate field associated with their password.

2.  A *new* password must be set for any such user (ie any user who still
    has non-empty salt).  Depending on your application, this might need
    to be done by the user in question, or by an administrator.

3.  After step 2, all users will have empty salt.  You should now remove
    the separate salt field from the model, remove your implementation of
    `userPasswordSalt` from your `HashDBUser` instance, and remove the
    empty-string setting of the salt from `setPasswordHash`.

Note that you cannot simply use the `upgradePasswordHash` function to
upgrade the database entries, since it keeps any existing salt value.
A *new* password must be set.


Deprecated Utilities
--------------------

The following, which have been deprecated for at least a year, have also
been removed in version 1.5:

-   `getAuthIdHashDB` - see
    https://github.com/paul-rouse/yesod-auth-hashdb/issues/5
-   The predefined `User` data type and automatically generated
    definitions related to it, such as `migrateUsers` and `UserId`
