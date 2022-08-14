# gettext-th

The [gettext](https://www.gnu.org/software/gettext/) project provides a library and tools for internationalization and localization of software. Haskell has already support for gettext. It is possible either to use the C-library with FFI ([hgettext](https://hackage.haskell.org/package/hgettext)) or to use a pure Haskell implementation ([haskell-gettext](https://hackage.haskell.org/package/haskell-gettext-0.1.2.0)).

With gettext the executables and the translations are separated and the language strings are looked up at runtime. The [loadCatalog](https://hackage.haskell.org/package/haskell-gettext-0.1.2.0/docs/Data-Gettext.html) of gettext-haskell has to read the translations at runtime with IO.

This is difficult if you transpile your haskell code to javascript, which runs in a browser. To close this gap gettext-th moves the lookup of messages to compile time. A similar approach was taken for [angular-i18n](https://angular.io/guide/i18n-overview) for performance reasons.
This of course has some drawbacks (less flexible and one program per language) and some benefits (simple and performant).

Theoretically gettext-th could also choose between runtime and compile time lookups.

# Warning

gettext-th will use IO at compile time and will write a file.

# How to use it

See for an example in example/hello.

To use gettext-th in your app use it and compile it. It will fail, but it will create the file po/messages.pot.
Run in the po folder:

```sh
msginit
msgfmt en_US.po
```

With that the program compiles. And you can update en_US.po again.

Now copy en_US.po to de_DE.po, translate the messages and run `msgfmt de_DE.po`.
If you recompile (maybe run cabal clean first) the output of your app is in german.


