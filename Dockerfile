
FROM clojure:openjdk-11-tools-deps AS builder

RUN curl -sL https://deb.nodesource.com/setup_15.x | bash -
RUN apt-get install -y nodejs

# Create and change to the app directory.
WORKDIR /usr/src/app

# Copy application dependency manifests to the container image.
# A wildcard is used to ensure both package.json AND package-lock.json are copied.
# Copying this separately prevents re-running npm install on every code change.
COPY package*.json ./

# Install dependencies.
RUN npm install

# Copy local code to the container image.
COPY . ./

# Cljs Build
RUN npm run build

FROM node:15-alpine3.10

WORKDIR /usr/src/app

COPY --from=builder /usr/src/app/build ./build/
COPY --from=builder /usr/src/app/package*.json ./
COPY --from=builder /usr/src/app/app.js .
COPY --from=builder /usr/src/app/config.prod.edn ./config.edn

# Install production dependencies.
RUN npm install --only=production

ENV PORT=8080
EXPOSE 8080

# Run the web service on container startup.
CMD [ "node", "app.js" ]
