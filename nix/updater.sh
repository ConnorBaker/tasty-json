#!/usr/bin/env -S nix develop --accept-flake-config --command bash 
# shellcheck shell=bash

echo "Updating flake inputs..."
cd "$(dirname "$0")"/..
nix flake --accept-flake-config update

echo "Entering new devShell to use updated flake inputs..."
nix develop --accept-flake-config --command bash <<-'EOF'
    echo "Regenerating nix files..."
    cd ./nix/packages
    for package in tasty-json-reporter tasty-json-markdown; do
        cabal2nix "../../$package" > "./$package.nix"
    done
    cd ../..

    echo "Formatting nix files..."
    nix --accept-flake-config fmt

    echo "Formatting cabal files..."
    for package in tasty-json-reporter tasty-json-markdown; do
        cabal format "$package/$package.cabal"
    done

    echo "Regenerating cabal freeze file with haskell packages available in updated flake..."
    # Note: we use temporary directories to avoid drawing on whatever the user 
    # has in their /.cabal directory, since this is visible to us from within 
    # the devShell.
    CABAL_DIR="$(mktemp -d)" cabal freeze --builddir="$(mktemp -d)"

    echo "Leaving new devShell..."
EOF

echo "Done!"