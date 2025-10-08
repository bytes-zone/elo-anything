FROM alpine:3.22.2 AS build

ADD https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz elm.gz
RUN gunzip -c elm.gz > /usr/bin/elm && chmod +x /usr/bin/elm

COPY elm.json /app/
COPY src /app/src
WORKDIR /app
RUN elm make src/Main.elm --output dist/index.html

FROM caddy:2.10.2-alpine
COPY static /public
COPY --from=build /app/dist/index.html /public/index.html
CMD ["caddy", "file-server", "--root", "/public"]
