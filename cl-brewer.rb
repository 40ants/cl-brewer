class ClBrewer < Formula
  desc "Homebrew formula builder for common lisp applications"
  homepage "https://github.com/svetlyak40wt/cl-brewer"
  url "https://github.com/svetlyak40wt/cl-brewer/archive/v0.5.2.tar.gz"
  sha256 "b79f3086b8f2deb6337df43f5c758b3a577c79aee66ff4e2222ec578147b0a9d"
  head NIL

  depends_on "sbcl"
  depends_on "buildapp" => :build

  resource "alexandria" do
    url "http://beta.quicklisp.org/archive/alexandria/2019-07-10/alexandria-20190710-git.tgz"
    sha256 "e0642bd6f8af8eb71e3359b45e11a135fa3c9a511492bc9dbbcd10ec7d694704"
  end

  resource "cffi" do
    url "http://beta.quicklisp.org/archive/cffi/2019-07-10/cffi_0.20.1.tgz"
    sha256 "6a427cc08f0418900bae8a76a690bb1c51fd61caf7efcb677d31701e0ce3ec5e"
  end

  resource "chipz" do
    url "http://beta.quicklisp.org/archive/chipz/2019-02-02/chipz-20190202-git.tgz"
    sha256 "aa58d80c12151f854b647b4d730ca29bba24c57f8954cb9ae777ee2968b568ee"
  end

  resource "cl+ssl" do
    url "http://beta.quicklisp.org/archive/cl+ssl/2019-07-10/cl+ssl-20190710-git.tgz"
    sha256 "fb57e0dba4f795f8f160daea60dc59ebd1e287c37a3af01e7269429f2d6abe53"
  end

  resource "cl-babel-babel" do
    url "http://dist.ultralisp.org/ultralisp/archive/c/cl-babel-babel-20190618134909.tgz"
    sha256 "db6c1b6e21e95519dc5626cd0ab041d2036062178f021da68214b6cd1f00ee0a"
  end

  resource "cl-base64" do
    url "http://beta.quicklisp.org/archive/cl-base64/2015-09-23/cl-base64-20150923-git.tgz"
    sha256 "17fab703f316d232b477bd2f8b521283cc0c7410f9b787544f3924007ab95141"
  end

  resource "command-line-arguments" do
    url "http://beta.quicklisp.org/archive/command-line-arguments/2019-07-10/command-line-arguments-20190710-git.tgz"
    sha256 "639c006a44487ef683acc2b6dc2d68122548cbfb90c66bdce3b594d955b64188"
  end

  resource "edicl-chunga" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/edicl-chunga-20190618200207.tgz"
    sha256 "11705039af9cf28135938a11fa434a0605ab3388b8e97eb335e3d6d8d8918aa7"
  end

  resource "edicl-cl-ppcre" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/edicl-cl-ppcre-20190618135807.tgz"
    sha256 "abddf50018b7d4593e962e6920e74cc5a08015bca29d8c79fc378466a30aa716"
  end

  resource "edicl-drakma" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/edicl-drakma-20190516030136.tgz"
    sha256 "28175bde04566d09fe3303529ddc238a0ce1a9b3a57baf35d8f44eebe9bee6c3"
  end

  resource "edicl-flexi-streams" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/edicl-flexi-streams-20190618143408.tgz"
    sha256 "7c5bf9b5106da9137b7a85d859a32eaf37c9b5f53f8d906ca4be6422bb6531e8"
  end

  resource "eudoxia0-trivial-download" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/eudoxia0-trivial-download-20190319084324.tgz"
    sha256 "ed50e00e527d8457004b0a0db844e1323043a36e1c7a0e06445ccc575fa13f08"
  end

  resource "puri" do
    url "http://beta.quicklisp.org/archive/puri/2018-02-28/puri-20180228-git.tgz"
    sha256 "7fd9fce21a83fb6d4f42bf146bdc6e5e36d8e95c6cf5427cd6aa78999b2a99e8"
  end

  resource "quicklisp-quicklisp-client" do
    url "http://dist.ultralisp.org/ultralisp/archive/q/quicklisp-quicklisp-client-20190721192409.tgz"
    sha256 "e19814f81b0ca81d13c7663bd9c00155fad722508f4bdc3f7fb5b96b3333330a"
  end

  resource "sharplispers-ironclad" do
    url "http://dist.ultralisp.org/ultralisp/archive/s/sharplispers-ironclad-20190713195502.tgz"
    sha256 "fe749dfb07c1c46e7de680672e373557110a3d8775e73499a5f6ffb7ff11f098"
  end

  resource "sharplispers-nibbles" do
    url "http://dist.ultralisp.org/ultralisp/archive/s/sharplispers-nibbles-20190319063629.tgz"
    sha256 "4b86d0c60e1e4351cf9b187e12d027e226ccb5c34bd7783ecafbd1194a394557"
  end

  resource "sharplispers-split-sequence" do
    url "http://dist.ultralisp.org/ultralisp/archive/s/sharplispers-split-sequence-20190517073140.tgz"
    sha256 "1b0bc2fb49ec66b845c2a85a0dcf6f675c8cd0b7d283de1822401ef58df944a5"
  end

  resource "sionescu-bordeaux-threads" do
    url "http://dist.ultralisp.org/ultralisp/archive/s/sionescu-bordeaux-threads-20190618135307.tgz"
    sha256 "ca77a1ccc7eecf8927484b8cf8061efc8ecda3d26ef94c6579ef2be8d0d437a6"
  end

  resource "trivial-features-trivial-features" do
    url "http://dist.ultralisp.org/ultralisp/archive/t/trivial-features-trivial-features-20190709040636.tgz"
    sha256 "438ff741c756902c5878565201ad703ab775ce76ac8a085c4149e6ccc3e19060"
  end

  resource "trivial-garbage-trivial-garbage" do
    url "http://dist.ultralisp.org/ultralisp/archive/t/trivial-garbage-trivial-garbage-20190520001607.tgz"
    sha256 "bbc86d072823cea26356b8ae994b1424579d0a9efa48506edac4c7ef7ab19f3a"
  end

  resource "trivial-gray-streams-trivial-gray-streams" do
    url "http://dist.ultralisp.org/ultralisp/archive/t/trivial-gray-streams-trivial-gray-streams-20190319050358.tgz"
    sha256 "6f85c53751a6c39897907582673b33ab25f2b2703f3b041f972cf1dfa4b8b596"
  end

  resource "uiop" do
    url "http://beta.quicklisp.org/archive/uiop/2019-05-21/uiop-3.3.3.tgz"
    sha256 "f5a978849233b3e02c8f70d2373c53f74b13c815a355ca074d21855f255e09e5"
  end

  resource "usocket" do
    url "http://beta.quicklisp.org/archive/usocket/2019-07-10/usocket-0.8.2.tgz"
    sha256 "f5239e0eab6c1d77cc64c76b81bf01e97940bfc7a14b4b8b70c613f8398eb63c"
  end

  def install
    resources.each do |resource|
      resource.stage buildpath/"lib"/resource.name
    end

    ENV["CL_SOURCE_REGISTRY"] = "#{buildpath}/lib//:#{buildpath}//"
    ENV["ASDF_OUTPUT_TRANSLATIONS"] = "/:/"

    system "buildapp", "--compress-core", "--load-system", "quicklisp-starter", "--load-system", "cl-brewer", "--output", "cl-brewer", "--entry", "cl-brewer::buildapp-main"

    bin.install "cl-brewer"
  end
end
