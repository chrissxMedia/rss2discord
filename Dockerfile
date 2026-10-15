FROM alpine:latest AS build
RUN apk add --no-cache build-base ca-certificates gmp-dev gmp-static ncurses-libs perl stack xz zlib-dev zlib-static
WORKDIR /build
COPY Main.hs LICENSE rss2discord.cabal stack.yaml stack.yaml.lock ./
RUN --mount=type=cache,target=/root/.stack stack build --copy-bins --local-bin-path /out --ghc-options="-optl=-static"

FROM scratch
ENV LANG=C.UTF-8
COPY --from=build /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
COPY --from=build /out/rss2discord /rss2discord
COPY --from=build /build/LICENSE /LICENSE
WORKDIR /data
ENTRYPOINT ["/rss2discord"]
