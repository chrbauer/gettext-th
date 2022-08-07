# gettext-th

The [gettext](https://www.gnu.org/software/gettext/) project provides a library and tools for internationalization and localization of software. Haskell has already support for gettext. It is possible either to use to C-library with FFI with [hgettext](https://hackage.haskell.org/package/hgettext) or to use a pure Haskell implementation with [haskell-gettext](https://hackage.haskell.org/package/haskell-gettext-0.1.2.0).

With gettext the executables and the translations are separated and the language strings are looked up at runtime. The [loadCatalog](https://hackage.haskell.org/package/haskell-gettext-0.1.2.0/docs/Data-Gettext.html) of gettext-haskell has to read the translations at runtime with IO.

This is difficult if you transpile your haskell code to javascript. To close this gap gettext-th moves the lookup of messages to compile time. A similar approach was taken for [angular-i18n](https://angular.io/guide/i18n-overview) for permormance reasons.
This of course has some drawbacks (less flexible and one program per language) and some benefits (simple and performant).

Theoretically gettext-th could alsa choose between runtime and compile time lookups.

# Warning

gettext-th will use IO at compile time and will write a file.

# How to use it

See for an exemple in example/hello.

To use gettext-th in youp app use it and compile it. It will fail, but it will create the file po/messages.pot.
Run in the po folder:

```sh
msginit
msgfmt en_US.po
```

With that the program compiles.
Now copy en_US.po to de_DE.po, translate the messages and run `msgfmt de_DE.po`.
If you recompile (maybe run cabal clean first) the output of your app is in german.


