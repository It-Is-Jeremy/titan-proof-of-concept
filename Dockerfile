FROM haskell:latest

WORKDIR /app
ADD . /app


RUN stack setup
RUN stack build