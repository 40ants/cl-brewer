class ClBrewer < Formula
  desc "Homebrew formula builder for common lisp applications"
  homepage "https://github.com/40ants/cl-brewer"
  url "https://github.com/40ants/cl-brewer/archive/v0.8.2.tar.gz"
  sha256 "9c294e9a3b837c8865653ca8a892b84487a99692a21c75129b077d027d628576"
  head "https://github.com/40ants/cl-brewer"

  depends_on "sbcl"

  resource "40ants-40ants-asdf-system" do
    url "http://dist.ultralisp.org/archive/1825/40ants-40ants-asdf-system-20230210163819.tgz"
    sha256 "3d733cc66520132ce0743ca2b363e1806c0311c8fb128e77d8af0ef0fb8638ee"
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
    url "http://dist.ultralisp.org/archive/1254/quicklisp-quicklisp-client-20210216010650.tgz"
    sha256 "d8e39e878da43e54ea69194c075da371e57c7c76f01000d541dc43e3c3ee6721"
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

  resource "Shinmera-deploy" do
    url "http://dist.ultralisp.org/archive/858/Shinmera-deploy-20230614113440.tgz"
    sha256 "c44a274d01c2f0c34369b4576bf33c500e3024f749eae7ac14b6deef4b12a43f"
  end

  resource "Shinmera-documentation-utils" do
    url "http://dist.ultralisp.org/ultralisp/archive/S/Shinmera-documentation-utils-20190627101653.tgz"
    sha256 "f2a238459c2a91032af093487e8237b38c321eae2c656c23558b711f6bc90815"
  end

  resource "Shinmera-trivial-indent" do
    url "http://dist.ultralisp.org/archive/193/Shinmera-trivial-indent-20230221065556.tgz"
    sha256 "5e66aa163143828c8b2a89bb0bb4f4c00003e7a09b9a291c86eeb9a4c2b957c4"
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
    url "http://beta.quicklisp.org/archive/uiop/2023-06-18/uiop-3.3.6.tgz"
    sha256 "302acb92b985b4b44a2ae2bdcc0d385084138c17acaf2cdc7ed2dc155172ec70"
  end

  resource "usocket" do
    url "http://beta.quicklisp.org/archive/usocket/2023-06-18/usocket-0.8.6.tgz"
    sha256 "33b94a7d3b2258bf6f06fbc52560dfbc3d2d8dd87ef407bfb18f10c40b17eb6e"
  end

  def install
    resources.each do |resource|
      resource.stage buildpath/"lib"/resource.name
    end

    ENV["CL_SOURCE_REGISTRY"] = "#{buildpath}/lib//:#{buildpath}//"
    ENV["ASDF_OUTPUT_TRANSLATIONS"] = "/:/"

    system "sbcl", "--eval", "(require :asdf)", "--eval", "(push :deploy-console *features*)", "--eval", "(asdf:load-system :deploy)", "--eval", "(handler-case (asdf:load-system :quicklisp-starter) (error () (uiop:quit 1)))", "--eval", "(handler-case (asdf:load-system :cl-plus-ssl-osx-fix) (error () (uiop:quit 1)))", "--eval", "(handler-case (asdf:make :cl-brewer) (error () (uiop:quit 1)))"
    bin.install Dir["bin/*"]
  end
end
