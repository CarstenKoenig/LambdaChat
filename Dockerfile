FROM haskell
EXPOSE 80
RUN apt-get update && \
    apt-get -y install make xz-utils
RUN stack upgrade
ADD ./Haskell /app/Haskell
ADD ./Elm /app/Elm
WORKDIR /app/Haskell
RUN cd /app/Haskell && stack setup
RUN apt-get -y install curl gnupg apt-transport-https ca-certificates netbase && \
    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - && \
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    apt-get update && \
    apt-get -y install yarn && \
    curl -sL https://deb.nodesource.com/setup_8.x | bash - && \
    apt-get update && \
    apt-get install -y nodejs && \
    yarn global add elm
RUN cd /app/Haskell && make dist
ENTRYPOINT /app/Haskell/dist/LambdaChat