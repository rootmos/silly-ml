FROM alpine:3.20 AS builder

RUN apk update && apk add bash opam m4 expect make binutils gcc musl-dev shadow

RUN useradd -m opam
WORKDIR /silly-ml
RUN chown opam:opam /silly-ml
USER opam

RUN opam init --bare --no-setup --disable-sandboxing
RUN opam switch create 4.03.0

RUN opam install --yes "core_kernel>=v0.9.0" "ocamlfind>=1.7.3" ppx_deriving "menhir>=20170607"

COPY Makefile _tags myocamlbuild.ml .
COPY src src
COPY --chown=opam runtime runtime

RUN opam exec make

COPY repl.expect *.sh .
COPY --chown=opam examples examples

RUN opam exec make test

FROM alpine:3.20
RUN apk update && apk add binutils
WORKDIR /silly-ml
COPY --from=builder /silly-ml/*.native /silly-ml/runtime/libruntime.a .
CMD ["/silly-ml/repl.native"]
