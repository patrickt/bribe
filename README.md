# bribe

> Alas! the small discredit of a bribe
> Scarce hurts the lawyer, but undoes the scribe.
>
> – Alexander Pope, "Epilogue to Satire"

`bribe` is a small utility that aggregates the licenses of [Stack][stack] projects in a manner compatible with that of [`licensed`][licensed]. In contrast to `licensed`, `bribe` does not actually build a Haskell project, instead inferring license information from the output of `stack ls dependencies`.

[licensed]: https://github.com/github/licensed
[stack]: https://github.com/commercialhaskell/stack

## Usage
