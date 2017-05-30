FROM naartjie/alpine-lein
COPY . /usr/src/app
WORKDIR /usr/src/app
RUN lein deps
CMD ["lein", "repl"]
