# servant-test

Run `stack install` to generate the server binary. The binary can be run as a standalone application with `docker/servant-test-exe [-- -p PORT]`.

## Docker

The run the server in a Docker container (with SSL), see the instructions in [docker/](docker).

## Generate HTML documentation with pandoc

`pandoc -f markdown+lhs pandoc.yaml $(cat toc.txt) -s --number-sections --toc -t html`
