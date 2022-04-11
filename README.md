# prelude.fut
An expansion of the default Futhark prelude with heavy-inspiration from the F#/OCaml APIs. Keep in mind the API is likely to change over time, at least while it is still in its infancy.

## Installation
```bash
futhark pkg add github.com/programmerino/prelude.fut
```

## Documentation
The documentation is automatically generated based on the master branch and is available at https://prelude-fut.0bit.dev/docs.

## Development
This project is using [Nix](https://nixos.org/download.html) flakes for dependency management. Note that the commands will not need the impure argument once the Futhark version is bumped in nixpkgs.

### Development shell
You can enter an environment with the dependencies of the project pre-installed with `nix shell --impure`

### Generating documentation
The documentation can be generated with `nix build .#docs --impure` into `result`

### Outputting the library
The directory structure with only the files needed by Futhark can be output with `nix build --impure` into `result`