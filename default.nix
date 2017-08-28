{ mkDerivation, base, brick, containers, lens, linear, mtl, random, vty
, reactive-banana, tagged, stm, stdenv
}:
mkDerivation {
  pname = "snik";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base brick containers lens linear mtl random reactive-banana
    vty tagged stm
  ];
  description = "OMG";
  license = stdenv.lib.licenses.gpl3;
}
