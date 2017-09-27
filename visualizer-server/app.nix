{ mkDerivation, aeson, analyze, array, base, bytestring, directory
, dsp, either, exceptions, foundation, HCodecs, inline-r
, optparse-simple, process, servant, servant-server, split, stdenv
, text, transformers, vector, wai, wai-cors, warp
, R, data_table, zoo, ffmpeg, sox
}:
mkDerivation {
  pname = "visualizer-server";
  version = "0.1.0.0";
  src = builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "data") ./.
  ;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson analyze array base bytestring directory dsp either exceptions
    foundation HCodecs inline-r optparse-simple process servant
    servant-server split text transformers vector wai wai-cors warp
  ];
  executableSystemDepends = [
    R data_table zoo ffmpeg sox
  ];
  homepage = "https://github.com/theam/visualizer-server#readme";
  license = stdenv.lib.licenses.bsd3;
}
