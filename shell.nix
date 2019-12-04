{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, criterion, deepseq, megaparsec
      , nonempty-containers, prettyprinter, recursion-schemes, stdenv
      , text
      }:
      mkDerivation {
        pname = "adventofcode2019";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base deepseq megaparsec nonempty-containers prettyprinter
          recursion-schemes text
        ];
        executableHaskellDepends = [ base ];
        benchmarkHaskellDepends = [ base criterion ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
