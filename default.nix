{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, Cabal, cases, HUnit
      , network, network-uri, scientific, stdenv, text, time-units
      , unordered-containers, vector
      }:
      mkDerivation {
        pname = "atlassian-connect-descriptor";
        version = "0.4.4.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base cases network network-uri text time-units
          unordered-containers
        ];
        testHaskellDepends = [
          aeson base bytestring Cabal cases HUnit network network-uri
          scientific text time-units unordered-containers vector
        ];
        doHaddock = false;
        doCheck = false;
        description = "Code that helps you create a valid Atlassian Connect Descriptor";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
