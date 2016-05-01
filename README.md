## Example usage of Reflex.Dom.LazyGrid

See [`Reflex.Dom.LazyGrid`](https://github.com/mulderr/reflex-dom-lazy-grid).

### How to build

There is a simple build script provided that will use `stack` to install `GHCJS` if needed and then build the project:

    ./build.sh

You may want to adjust compiler version first if you already have `GHCJS` installed. Will save an hour or two...

Refer to `stack` [documentation](http://docs.haskellstack.org/en/stable/ghcjs/).

### How to run

The build script copies all the necessary files to the `serve` directory. If your system has `python3` available just:

    cd serve && python3 -m http.server 8080

Finally visit: http://localhost:8080
