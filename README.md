# prelude.fut
An expansion of the default Futhark prelude with heavy-inspiration from the F#/OCaml APIs. Keep in mind the API is likely to change over time, at least while it is still in its infancy.

## Installation
```bash
futhark pkg add github.com/programmerino/prelude.fut
```

## Documentation
The documentation is automatically generated based on the master branch and is available at https://prelude-fut.0bit.dev/docs.

## Development/Contribution
This project is using [Nix](https://nixos.org/download.html) flakes for dependency management, so you'll have the best experience if you install Nix.

### Tasks
- [ ] Increase test coverage
- [ ] Replace implementations nonidiomatic sequential loops which are (probably) slower than they could be
- [ ] Implement more standard library functions for IArray

### Running tests
You can run all the tests with `nix flake check -L`

### Development shell
You can enter an environment with the dependencies of the project pre-installed with `nix shell`

### Generating documentation
The documentation can be generated with `nix build .#web` into `result`

### Outputting the library
The directory structure with only the files needed by Futhark can be output with `nix build` into `result`