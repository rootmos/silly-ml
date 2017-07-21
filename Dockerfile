FROM ocaml/opam:alpine-3.3_ocaml-4.03.0 as builder

RUN sudo apk add m4 expect make binutils gcc
RUN opam depext conf-m4.1
RUN opam install "core_kernel>=v0.9.0" "ocamlfind>=1.7.3" ppx_deriving "menhir>=20170607"

RUN sudo mkdir -m a=rwx /silly-ml
WORKDIR /silly-ml
COPY Makefile .
COPY _tags .
COPY myocamlbuild.ml .
COPY repl.expect .
COPY *.sh ./
COPY src src
RUN sudo mkdir -m a=rwx examples
ADD examples/*.silly-ml examples/
ADD examples/*.exit-code examples/

RUN sudo mkdir -m a=rwx runtime
ADD runtime/include runtime/include/
ADD runtime/*.asm runtime/
ADD runtime/*.c runtime/
ADD runtime/*.h runtime/
ADD runtime/*.sh runtime/
ADD runtime/*.expected runtime/
ADD runtime/*.env runtime/
ADD runtime/Makefile runtime/

RUN eval `opam config env` && make test

FROM alpine:3.3
RUN apk update && apk add binutils
WORKDIR /root/
COPY --from=builder /silly-ml/repl.native .
COPY --from=builder /silly-ml/compiler.native .
COPY --from=builder /silly-ml/runtime/libruntime.a .
CMD ["./repl.native"]
