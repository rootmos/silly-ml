FROM ocaml/opam:alpine-3.3_ocaml-4.03.0_flambda as builder

RUN sudo apk add m4 expect
RUN opam depext conf-m4.1
RUN opam install "core_kernel>=v0.9.0" "ocamlfind>=1.7.3" ppx_deriving "menhir>=20170607"

RUN sudo mkdir -m a=rwx /silly-ml
WORKDIR /silly-ml
COPY Makefile .
COPY _tags .
COPY myocamlbuild.ml .
COPY repl.expect .
COPY src src

RUN eval `opam config env` && make test

FROM alpine:3.3
WORKDIR /root/
COPY --from=builder /silly-ml/repl.native .
CMD ["./repl.native"]
