
# CHANGELOG

## 2026-01-03 - Version 0.0.1 - Initial posting of project

## 2026-08-29 - Version 0.1.0 - Rewrite for Optimization

  - Gemini helped me tremendously this week to rewrite, cleanse, purge, simplify, and optimize the earlier code. Everything is now much simpler, with many of the unnecessary FFI calls requiring passing by value removed (because Nick B already supplied them in his notcurses-ffi library).
  - Also, all 4 examples now work both from within Emacs and also from the CLI
  - README.md updated with more explanation
  - Next plans are to add an event loop.
