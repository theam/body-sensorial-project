{ mkDerivation, aeson, base, binary, bytestring, cassava
, exceptions, fetchgit, foldl, free, hashable, lucid, QuickCheck
, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "analyze";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/ejconlon/analyze";
    sha256 = "0crwm949p64lf58x06h8lvfl8cpjj8vrij3l13yl7zhqzc60m0gq";
    rev = "b38eef0b735c01504980a155187517992a77f1f6";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base binary bytestring cassava exceptions foldl free hashable
    lucid text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring exceptions foldl QuickCheck tasty tasty-hunit
    tasty-quickcheck text unordered-containers vector
  ];
  homepage = "https://github.com/ejconlon/analyze#readme";
  description = "making data science easy and safe with data frames";
  license = stdenv.lib.licenses.bsd3;
}
