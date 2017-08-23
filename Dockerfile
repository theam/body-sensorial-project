FROM nixos/nix:1.11

WORKDIR /app
ADD . /app

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update
RUN nix-env -i gnumake
RUN make && make combine
CMD make run
