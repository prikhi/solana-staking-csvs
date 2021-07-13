# Solana Staking CSVs

[![solana-staking-csv Build Status](https://travis-ci.org/prikhi/solana-staking-csv.svg?branch=master)](https://travis-ci.org/prikhi/solana-staking-csv)


Generate CSVs of your Solana staking rewards.

Requires `stack` & a SolanaBeach API key, which you can request
[here](https://github.com/solana-beach/api).

```code
stack run -- <YOUR_API_KEY> <ACCOUNT_PUBKEY>
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
