# Solana Staking CSVs

[![solana-staking-csvs Build Status](https://github.com/prikhi/solana-staking-csvs/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/solana-staking-csvs/actions/workflows/main.yml)


Generate CSVs of your Solana staking rewards.

Requires [`stack`][get-stack] & a SolanaBeach API key, which you can request
[here][solanabeach-api].

```sh
stack run -- <YOUR_API_KEY> <ACCOUNT_PUBKEY>
```

[get-stack]: https://docs.haskellstack.org/en/stable/README/
[solanabeach-api]: https://github.com/solana-beach/api


## Install

You can install the CLI exe by running `stack install`. This lets you call the
executable directly instead of through stack:

```sh
$ stack install
$ export PATH="${HOME}/.local/bin/:${PATH}"
$ solana-staking-csvs <YOUR_API_KEY> 6MTkiDNY5N5PoJHN862D91jM5ztF3KQWDyBeobo2rSgK
time,amount
2021-07-03 19:49:49UTC,135.477285991
2021-07-06 21:44:25UTC,135.889369126
2021-07-10 00:02:06UTC,136.058196192
2021-07-13 03:47:41UTC,134.691741829
```


## Build

You can build the project with stack:

```code
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:

```code
stack test --haddock --fast --file-watch --pedantic
```

To build & open the documentation, run

```code
stack haddock --open solana-staking-csv
```


## LICENSE

BSD-3
