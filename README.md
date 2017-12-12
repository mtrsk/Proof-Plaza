# Coq-Software Foudations
Some studies of the Coq theorem prover. Includes some notes about the Software Foundations series.

## Building the Coq Dockerfile

```
docker built -t coq:8.7 .
```

## Running the Image

```
docker run --rm -it -v $(pwd):/workspace coq:8.7 coqc /workspace/FILE.v
```
