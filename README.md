# Haskeller Answers

![Haskeller Answers](https://raw.githubusercontent.com/vrom911/haskeller-answers/master/images/haskeller-answers-logo.png)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Web app for typical Haskeller answers to everything.

## For Developers

`haskeller-answers` is a `miso` framework based Haskell web application.

To build the app you should have `nix` installed.

To ease the process of building, it uses `miso` as a GitHub submodule. So, first
you would need to run:

```
$ git submodule update --init --recursive
```

After this is up-to-date, you can run

```
$ nix-build
```

And to see the result just open `index.html` file in your favourite browser.

## Disclaimer

FOR ENTERTAINMENT PURPOSES ONLY. ALL CHARACTERS APPEARING IN THIS WORK ARE
FICTITIOUS. ANY REFERENCE TO LIVING PERSONS OR REAL EVENTS IS PURELY
COINCIDENTAL.
