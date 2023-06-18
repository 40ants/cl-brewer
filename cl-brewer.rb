class ClBrewer < Formula
  desc "Homebrew formula builder for common lisp applications"
  homepage "https://github.com/40ants/cl-brewer"
  url "https://github.com/40ants/cl-brewer/archive/v0.8.1.tar.gz"
  sha256 "46f3ee8ebd869075d2d9662ee5a062fb4f5e7d37057a23f41951b7a9258c19b0"
  head NIL

  depends_on "sbcl"
  depends_on "buildapp" => :build

  resource "alexandria" do
    url "http://beta.quicklisp.org/archive/alexandria/2022-07-07/alexandria-20220707-git.tgz"
    sha256 "8e6173120de8786c7ae0fefba8102d82620fac42e9aade954af9135e0870d6b1"
  end

  resource "cffi" do
    url "http://beta.quicklisp.org/archive/cffi/2023-02-14/cffi-20230214-git.tgz"
    sha256 "cffcb66a21a0d126c6f7f835f151b86176d57b70e2d16ec2e546ba3ac74b39cd"
  end

  resource "cl+ssl" do
    url "http://beta.quicklisp.org/archive/cl+ssl/2023-02-14/cl+ssl-20230214-git.tgz"
    sha256 "910718552c6f84a76681f39b8402c36a74d484bba217e2732f213d4b9e264581"
  end

  resource "cl-babel-babel" do
    url "http://dist.ultralisp.org/archive/1237/cl-babel-babel-20230131043757.tgz"
    sha256 "cd3c1154be061834514a41ccc826735edfdf376ea9124e338f54209861a8a1b1"
  end

  resource "cl-base64" do
    url "http://beta.quicklisp.org/archive/cl-base64/2020-10-16/cl-base64-20201016-git.tgz"
    sha256 "3ff50faf5ddccd409f8954eb70c2d4e76329cc916f070de95f79c7ecf6d3a2f1"
  end

  resource "command-line-arguments" do
    url "http://beta.quicklisp.org/archive/command-line-arguments/2021-08-07/command-line-arguments-20210807-git.tgz"
    sha256 "939b3966e2887dd0b81bd1c3d051c42bce78c10fa32661263c9aa6c355fbf9bd"
  end

  resource "edicl-chunga" do
    url "http://dist.ultralisp.org/archive/1244/edicl-chunga-20220912164619.tgz"
    sha256 "a34e2b85c76b3633f4f5dba26548a116d2b64f15db7c7e31ba04f913baf251c8"
  end

  resource "edicl-cl-ppcre" do
    url "http://dist.ultralisp.org/archive/1239/edicl-cl-ppcre-20230614075200.tgz"
    sha256 "fcf1d505007aa0fce3d09752c5fd1459efbaae43fb80f06980e591c7ec393918"
  end

  resource "edicl-drakma" do
    url "http://dist.ultralisp.org/archive/839/edicl-drakma-20230221010508.tgz"
    sha256 "d741be9ce6ccb46013d73f01b873fbd8576997c6cb97048695e8e181f97c6082"
  end

  resource "edicl-flexi-streams" do
    url "http://dist.ultralisp.org/archive/1242/edicl-flexi-streams-20220112225522.tgz"
    sha256 "bfbb31674c02abdea0ed8135e1894a2f743df4b03e8bab7db1506b90270d95f9"
  end

  resource "eudoxia0-trivial-download" do
    url "http://dist.ultralisp.org/archive/202/eudoxia0-trivial-download-20230124040434.tgz"
    sha256 "03e7abf5b523a127537abdca9fb1e6a379d2d69c54a2febdfa2dd8ffe67a5eee"
  end

  resource "lmj-global-vars" do
    url "http://dist.ultralisp.org/ultralisp/archive/l/lmj-global-vars-20190319075150.tgz"
    sha256 "b8a52b9ef2152f4087c1bf1ea9ea29f137419aa81d6ce4e250af2c34b169168d"
  end

  resource "puri" do
    url "http://beta.quicklisp.org/archive/puri/2020-10-16/puri-20201016-git.tgz"
    sha256 "fd3bd9acd2438eb89fcd0c8651b6f6b5d92f337872a8e59326a680666e7c079a"
  end

  resource "quicklisp-quicklisp-client" do
    url "http://dist.ultralisp.org/archive/1254/quicklisp-quicklisp-client-20221213010143.tgz"
    sha256 "9d1432aa6b1f5153872b165c6fa5ae263e22e769197ce1e5336c5a09f6373796"
  end

  resource "sharplispers-chipz" do
    url "http://dist.ultralisp.org/archive/1771/sharplispers-chipz-20230418140035.tgz"
    sha256 "62f582c1904782dff1e23995b80cf5ad7faa4d0408a7a096cb46ade63d2fe991"
  end

  resource "sharplispers-ironclad" do
    url "http://dist.ultralisp.org/archive/655/sharplispers-ironclad-20230617153333.tgz"
    sha256 "b0d7f107cbbeb5c9a55d362210b81a4e2ec126c460891618032c5250f206a006"
  end

  resource "sharplispers-split-sequence" do
    url "http://dist.ultralisp.org/archive/273/sharplispers-split-sequence-20211208061629.tgz"
    sha256 "98c27530444a65fcdc71f2dc539a252efa977f1a2ebfd6a0453ddfa572ca1d83"
  end

  resource "sionescu-bordeaux-threads" do
    url "http://dist.ultralisp.org/archive/1238/sionescu-bordeaux-threads-20230604143150.tgz"
    sha256 "388625b5f352f5099bffa548569582c6eeb751e0293b4e861c3e534643d43587"
  end

  resource "trivial-features-trivial-features" do
    url "http://dist.ultralisp.org/archive/197/trivial-features-trivial-features-20230614074348.tgz"
    sha256 "6dab2a6ee703a8d444d3e0438afc838138a8943c4ae1640cc5d710dabba35b34"
  end

  resource "trivial-garbage-trivial-garbage" do
    url "http://dist.ultralisp.org/archive/195/trivial-garbage-trivial-garbage-20211229223228.tgz"
    sha256 "d828515970e9d6e70f6307dbd3a7da67b523e5c76be3473c65ad1bdf48eaaee7"
  end

  resource "trivial-gray-streams-trivial-gray-streams" do
    url "http://dist.ultralisp.org/archive/194/trivial-gray-streams-trivial-gray-streams-20210118211457.tgz"
    sha256 "5f9e8264e4bae7febcb11237d117a50fa061ea46535570475e9df0da5532db47"
  end

  resource "uiop" do
    url "http://beta.quicklisp.org/archive/uiop/2022-11-06/uiop-3.3.6.tgz"
    sha256 "302acb92b985b4b44a2ae2bdcc0d385084138c17acaf2cdc7ed2dc155172ec70"
  end

  resource "usocket" do
    url "http://beta.quicklisp.org/archive/usocket/2022-11-06/usocket-0.8.5.tgz"
    sha256 "aef6f5b4b9232cf422a05e3f958e1d85a6d9ad1a6f05b1833598a96f3c215c2c"
  end

  def install
    resources.each do |resource|
      resource.stage buildpath/"lib"/resource.name
    end

    ENV["CL_SOURCE_REGISTRY"] = "#{buildpath}/lib//:#{buildpath}//"
    ENV["ASDF_OUTPUT_TRANSLATIONS"] = "/:/"

    system "buildapp", "--compress-core", "--load-system", "cl-brewer", "--output", "cl-brewer", "--entry", "cl-brewer.main"

    bin.install "cl-brewer"
  end
end
