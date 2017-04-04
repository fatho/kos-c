#!/bin/bash

# Prepare and package a linux release

cabal_file="kos-c.cabal"
release_dir="release-packages"

if [ ! -f "$cabal_file" ]; then
    # require that this is run from the root of our stack project
    echo "Not ran inside project root." >&2
    exit 1
fi

# create dir
mkdir -p "$release_dir"

# build project
stack build
if [ $? -ne 0 ]; then
    echo "Error building project." >&2
    exit 1
fi

# find version
VERSION=$(grep -oP 'version:\s*\K[0-9].[0-9].[0-9].[0-9]' "$cabal_file")

echo "Packaging release v$VERSION"

# create target directory
package_name="kos-c-$VERSION-linux-$(uname -m)"
package_dir="$release_dir/$package_name"
mkdir -p "$package_dir"

# copy stuff to target (hacky hard-coded path)
cp ".stack-work/install/x86_64-linux/lts-8.5/8.0.2/bin/koscc" "$package_dir/"
cp -r "kosc-prelude" "$package_dir/"

# create zip
pushd "$release_dir"
zip -r "$package_name.zip" "$package_name"
popd

echo "Done!"
