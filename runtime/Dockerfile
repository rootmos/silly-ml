FROM alpine:3.3

RUN apk update
RUN apk add make binutils gcc

RUN mkdir /runtime
WORKDIR /runtime

ADD include include
ADD *.asm ./
ADD *.c ./
ADD *.h ./
ADD *.sh ./
ADD *.expected ./
ADD *.env ./
ADD Makefile .

ENTRYPOINT ["make"]
