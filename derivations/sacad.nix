{ lib, python38Packages, fetchPypi, web_cache }:

python38Packages.buildPythonPackage rec {
  pname = "sacad";
  version = "2.3.4";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-OVkWW8sPhr0W4kBTkWWFp8lrAuA7yQz/oVr4bX6uYuM=";
  };

  doCheck = false;

  propagatedBuildInputs = with python38Packages; [
    aiohttp
    appdirs
    bitarray
    cssselect
    fake-useragent
    lxml
    mutagen
    pillow
    tqdm
    unidecode
    web_cache
  ];

  meta = with lib; {
    description = "Smart Automatic Cover Art Downloader";
    homepage = "https://github.com/desbma/sacad";
    license = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
