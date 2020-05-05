FROM haskell:8.6.5 AS build
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
    && apt-get -y install --no-install-recommends libfcgi-dev 2>&1
RUN apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY . ./
RUN mkdir -p $HOME/.local/bin
RUN stack build --copy-bins 2>&1
ENV DEBIAN_FRONTEND=dialog
ENV SHELL /bin/bash

FROM nginx:1.17.10
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
    && apt-get -y install --no-install-recommends libfcgi-bin 2>&1
RUN apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY nginx.conf ./
ENV HOST 0.0.0.0
ENV PORT 8080
RUN sh -c "envsubst '\$PORT'  < nginx.conf > /etc/nginx/conf.d/default.conf"
COPY --from=build /root/.local/bin/term-graph ./
COPY --from=build /app/nginx.sh ./
EXPOSE 8080
ENTRYPOINT ["/app/nginx.sh"]
ENV DEBIAN_FRONTEND=dialog
ENV SHELL /bin/bash

