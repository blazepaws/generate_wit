# Wit Builder

This crate provides a Rust type wrapper for the wit file format.
Use this to create or parse wit files. 
Its aim is _not_ to allow generating every possible wit file. 
Instead its goal is to parse any wit file and generate a valid wit file for any abstract wit tree.

It is based on the preliminary wit spec at https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md.