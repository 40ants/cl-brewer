class ClBrewer < Formula
  desc "Homebrew formula builder for Common Lisp applications."
  homepage "https://40ants.com/cl-brewer/"
  url "https://github.com/40ants/cl-brewer/archive/v0.11.0.tar.gz"
  sha256 "a465bd2a5737e92a505bd01461e2e50b74a71062376557170acb5de6de3ecbb9"
  head "https://github.com/40ants/cl-brewer"

  depends_on "sbcl" => :build
  # required by: CL+SSL/CONFIG::LIBCRYPTO and CL+SSL/CONFIG::LIBSSL
  depends_on "openssl@3"
  # required by: DEPLOY::COMPRESSION-LIB
  depends_on "zstd"

  resource "40ants-40ants-asdf-system" do
    url "http://dist.ultralisp.org/archive/1825/40ants-40ants-asdf-system-20230724171226.tgz"
    sha256 "6a1f4785fb233bdf38fe4cbcb6e8181b9a3e6b3e6790eaa16e6f92dbfd4e3ab0"
  end

  resource "40ants-cl-brewer" do
    url "http://dist.ultralisp.org/archive/1977/40ants-cl-brewer-20230725165337.tgz"
    sha256 "10b4d88c0d79a920230b012a8b3e12ef544534003d507e70da753ea44893bff9"
  end

  resource "40ants-cl-plus-ssl-osx-fix" do
    url "http://dist.ultralisp.org/archive/1962/40ants-cl-plus-ssl-osx-fix-20230618185126.tgz"
    sha256 "f26673e427545bfc7affae80d14d74a78c6b5f17b1a68d4a38c6223e894f92bb"
  end

  resource "alexandria" do
    url "http://beta.quicklisp.org/archive/alexandria/2023-06-18/alexandria-20230618-git.tgz"
    sha256 "d5fbb349204082b61a7a6c58aed1d1606e524bfa38ecd0ec6ebed7f658061819"
  end

  resource "cffi" do
    url "http://beta.quicklisp.org/archive/cffi/2023-06-18/cffi-20230618-git.tgz"
    sha256 "223f9fa6f1c50df1a8d5b7db0d02c520a4454965ed1a996cf5a6d20f447058c3"
  end

  resource "cl+ssl" do
    url "http://beta.quicklisp.org/archive/cl+ssl/2023-06-18/cl+ssl-20230618-git.tgz"
    sha256 "c5653c3d96b169003a386628cf0c92aaf256ba27377f569fe45e4e1012c80343"
  end

  resource "cl-babel-babel" do
    url "http://dist.ultralisp.org/archive/1237/cl-babel-babel-20230131043757.tgz"
    sha256 "cd3c1154be061834514a41ccc826735edfdf376ea9124e338f54209861a8a1b1"
  end

  resource "cl-base64" do
    url "http://beta.quicklisp.org/archive/cl-base64/2020-10-16/cl-base64-20201016-git.tgz"
    sha256 "3ff50faf5ddccd409f8954eb70c2d4e76329cc916f070de95f79c7ecf6d3a2f1"
  end

  resource "cl-change-case" do
    url "http://beta.quicklisp.org/archive/cl-change-case/2021-04-11/cl-change-case-20210411-git.tgz"
    sha256 "b2920b37fe4eae7864c0c2674405cb84ed951891462edcb8d536c003d2324293"
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

  resource "edicl-cl-unicode" do
    url "http://dist.ultralisp.org/archive/1241/edicl-cl-unicode-20210222215228.tgz"
    sha256 "8bbf6b8ff8d51caa9e3c329509345f59636cbd61e8c896112c792bbcbd3a0751"
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
    url "http://dist.ultralisp.org/archive/1254/quicklisp-quicklisp-client-20210216010650.tgz"
    sha256 "d8e39e878da43e54ea69194c075da371e57c7c76f01000d541dc43e3c3ee6721"
  end

  resource "sharplispers-chipz" do
    url "http://dist.ultralisp.org/archive/1771/sharplispers-chipz-20230418140035.tgz"
    sha256 "62f582c1904782dff1e23995b80cf5ad7faa4d0408a7a096cb46ade63d2fe991"
  end

  resource "sharplispers-ironclad" do
    url "http://dist.ultralisp.org/archive/655/sharplispers-ironclad-20230710201532.tgz"
    sha256 "769001a6509105daa01df15427ca28147c8c59249a2a69ff0f94ca5a98335442"
  end

  resource "sharplispers-split-sequence" do
    url "http://dist.ultralisp.org/archive/273/sharplispers-split-sequence-20211208061629.tgz"
    sha256 "98c27530444a65fcdc71f2dc539a252efa977f1a2ebfd6a0453ddfa572ca1d83"
  end

  resource "Shinmera-deploy" do
    url "http://dist.ultralisp.org/archive/858/Shinmera-deploy-20230710201211.tgz"
    sha256 "1ad782e17010c250f69965b514add3cc5fffa3fddba3066c9d062007905c462b"
  end

  resource "Shinmera-documentation-utils" do
    url "http://dist.ultralisp.org/archive/843/Shinmera-documentation-utils-20230711001755.tgz"
    sha256 "e8d287de786216763a2fafbc7d18da539b7a124497b8529f066c33f0148f2182"
  end

  resource "Shinmera-trivial-indent" do
    url "http://dist.ultralisp.org/archive/193/Shinmera-trivial-indent-20230710204252.tgz"
    sha256 "7b4c8b6db7250fb2794a79967fcb5c0410146b206233e43227b708d92d3ec944"
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
    url "http://dist.ultralisp.org/archive/195/trivial-garbage-trivial-garbage-20230621104435.tgz"
    sha256 "debc94633569e61b4f12fbee52b127b4234385795e0dd2dde9099925a5ab66cd"
  end

  resource "trivial-gray-streams-trivial-gray-streams" do
    url "http://dist.ultralisp.org/archive/194/trivial-gray-streams-trivial-gray-streams-20230630171731.tgz"
    sha256 "52883684b77d874de095dd56752dda1226a54030b5ede28a3ae71de0ee93e284"
  end

  resource "uiop" do
    url "http://beta.quicklisp.org/archive/uiop/2023-06-18/uiop-3.3.6.tgz"
    sha256 "302acb92b985b4b44a2ae2bdcc0d385084138c17acaf2cdc7ed2dc155172ec70"
  end

  resource "usocket" do
    url "http://beta.quicklisp.org/archive/usocket/2023-06-18/usocket-0.8.6.tgz"
    sha256 "33b94a7d3b2258bf6f06fbc52560dfbc3d2d8dd87ef407bfb18f10c40b17eb6e"
  end

  resource "vindarel-cl-str" do
    url "http://dist.ultralisp.org/archive/36/vindarel-cl-str-20230710200441.tgz"
    sha256 "5619486c2b5184d6b1ca99c452ee63b393ddca6d1b007c373c262fb7e62408f7"
  end

  def install
    resources.each do |resource|
      resource.stage buildpath/"_brew_resources"/resource.name
    end

    ENV["LIBEXEC_PATH"] = "#{libexec}/"
    ENV["CL_SOURCE_REGISTRY"] = "#{buildpath}/:#{buildpath}/_brew_resources//"
    ENV["ASDF_OUTPUT_TRANSLATIONS"] = "/:/"

    system "sbcl", "--eval", "(require :asdf)", "--eval", "(push :deploy-console *features*)", "--eval", "(asdf:load-system :cl-brewer-deploy-hooks)", "--eval", "(HANDLER-BIND ((ERROR
                (LAMBDA (E)
                  (UIOP/IMAGE:PRINT-BACKTRACE :CONDITION E)
                  (UIOP/IMAGE:QUIT 1)))
               (WARNING #'MUFFLE-WARNING))
  (ASDF/OPERATE:LOAD-SYSTEM \"quicklisp-starter\"))", "--eval", "(HANDLER-BIND ((ERROR
                (LAMBDA (E)
                  (UIOP/IMAGE:PRINT-BACKTRACE :CONDITION E)
                  (UIOP/IMAGE:QUIT 1)))
               (WARNING #'MUFFLE-WARNING))
  (ASDF/OPERATE:LOAD-SYSTEM \"cl-plus-ssl-osx-fix\"))", "--eval", "(HANDLER-BIND ((ERROR
                (LAMBDA (E)
                  (UIOP/IMAGE:PRINT-BACKTRACE :CONDITION E)
                  (UIOP/IMAGE:QUIT 1)))
               (WARNING #'MUFFLE-WARNING))
  (ASDF/OPERATE:MAKE \"cl-brewer\"))"

    system "bash", "-c", "mkdir dyn-libs && find bin/ -name '*.dylib' -exec mv '{}' dyn-libs/ \\;"

    bin.install Dir["bin/*"]
    libexec.install Dir["dyn-libs/*"]
  end
end
