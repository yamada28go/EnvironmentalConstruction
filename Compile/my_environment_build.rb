# -*- coding: utf-8 -*-

require 'pp'

#-------------------
#環境構築スクリプト
#-------------------

#共通処理
module Common

  #プログラム展開ディレクトリを取得する
  module_function 
  def get_exploded_directory_name( name )
    name.freeze
    
    file_base_name =  File::basename(name).split(/\./)
    
    #以下に関係する拡張子は消去する
    file_base_name.delete_if do |t|

      #消去対象の拡張子
      del_t = %w(bz2 gz xz gz tar).freeze 
      del_t.include?(t)

    end
    
    return file_base_name.join('.')

  end

  #プログラムを実行する
  module_function 
  def system_cmd( cmd )

    puts cmd
    puts Dir::pwd
    puts `#{cmd}`
    if 0 != $?
      raise "コマンド実行に失敗しました。"
    end

  end

  #アーカイブを展開する
  module_function 
  def archive_expand( archive_name )

    case archive_name
    when /tar\.gz/ then
      
      system_cmd( "tar xvfz #{archive_name}" )

    when /tar\.xz/ then

      #xz形式は標準tarでは対応出来ないため、一回展開してからtarにかける
      system_cmd( "xz -d #{archive_name}" )
      t = File::basename(archive_name).split(/\./)
      t.pop()
      system_cmd( "tar xvf #{t.join(".")}" )

    when /tar\.bz2/ then

      system_cmd( "tar xvfj #{archive_name}" )

    else

      raise "未対応の圧縮形式です"

    end

  end

  #アーカイブファイルをダウンロードする
  module_function 
  def archive_download( conf )

    #現在のパスを取得する
    base_path = Dir::pwd.freeze
    
    begin
      #ファイルが存在するか判定する
      Dir::chdir( conf[ :archive_path ] )
      if false == File.exist?( File::basename( conf[ :archive ] ) )
        #ファイルをダウンロードする
        system_cmd( "wget #{conf[ :archive ]}" ) 
      end
      system_cmd( "cp #{File::basename( File::basename( conf[ :archive ] ) )} #{ conf[ :work_path ] }" ) 
    rescue
      #処理失敗
      raise
    ensure
      # 例外の発生有無に関わらず最後に必ず実行する処理
      #カレントディレクトリを元に戻す
      Dir::chdir( base_path )
    end

  end
end


#コンパイル方法定義モジュール
module Compile_protocol

  #通常コンパイル
  module_function 
  def basic( conf )

    p conf

    #ファイルを展開
    Common::archive_expand( conf[:archive_path] )
    
    #コンパイル用文字列を作成
    cmd = <<EOS
cd #{Common::get_exploded_directory_name( conf[:archive_path]  )} && 
./configure --prefix=#{conf[:path]} && 
make -j #{conf[:parallel]} && 
make install 
EOS
    
    Common::system_cmd( cmd )
    
  end

  #gmpコンパイル
  module_function 
  def gmp( conf )

    puts "gmp"
    p conf

    #ファイルを展開
    Common::archive_expand( conf[:archive_path] )
    
    #コンパイル用文字列を作成
    cmd = <<EOS
cd #{Common::get_exploded_directory_name( conf[:archive_path]  )} && 
./configure --prefix=#{conf[:path]} && 
make -j #{conf[:parallel]} && 
make install &&
make check -j #{conf[:parallel]}
EOS
    Common::system_cmd( cmd )
    
  end


  
  #emacs コンパイル
  module_function 
  def emacs( conf )
    
    puts "emacs"
    p conf

    #ファイルを展開
    Common::archive_expand( conf[:archive_path] )
    
    #コンパイル用文字列を作成
    cmd = <<EOS
cd #{Common::get_exploded_directory_name( conf[:archive_path]  )} && 
./configure --prefix=#{conf[:path]} --without-x && 
make -j #{conf[:parallel]} && 
make install 
EOS
    
    Common::system_cmd( cmd )
    
  end

  #mpfr コンパイル
  module_function 
  def mpfr( conf )
    
    puts "mpfr"
    p conf

    #ファイルを展開
    Common::archive_expand( conf[:archive_path] )
    
    #コンパイル用文字列を作成
    cmd = <<EOS
cd #{Common::get_exploded_directory_name( conf[:archive_path]  )} && 
./configure --prefix=#{conf[:path]} --with-gmp=#{conf[:path]} && 
make -j #{conf[:parallel]} && 
make install 
EOS
    
    Common::system_cmd( cmd )
    
  end
  
  #mpc コンパイル
  module_function 
  def mpc( conf )
    
    puts "mpfr"
    p conf

    #ファイルを展開
    Common::archive_expand( conf[:archive_path] )
    
    #コンパイル用文字列を作成
    cmd = <<EOS
cd #{Common::get_exploded_directory_name( conf[:archive_path]  )} && 
./configure --prefix=#{conf[:path]} --with-gmp=#{conf[:path]} && 
make -j #{conf[:parallel]} && 
make install 
EOS
    
    Common::system_cmd( cmd )
    
  end

  #gcc コンパイル
  module_function 
  def gcc( conf )
    
    puts "gcc"
    p conf

    #ファイルを展開
    Common::archive_expand( conf[:archive_path] )
    
    #コンパイル用文字列を作成
    cmd = <<EOS
cd #{Common::get_exploded_directory_name( conf[:archive_path]  )} && 
./configure  --with-gmp=#{conf[:path]} --with-mpfr=#{conf[:path]} --prefix=#{conf[:path]} --program-suffix=4.8 --enable-languages=c,c++ &&
make -j #{conf[:parallel]} && 
make install 
EOS
    
    Common::system_cmd( cmd )
    
  end

  #展開されたディレクトリを探す
  def find_expand_dir( key_word )
      Dir::entries(".").each do |i|
        if File::ftype(i) == "directory"
          if /#{key_word}/ =~ i
            return i
          end
        end
      end
  end

  #llvm コンパイル
  module_function 
  def llvm( conf )
    
    puts "llvm"
    p conf

    #ファイルを展開
    Common::archive_expand( conf[:archive_path] )

    ## llvmのパスを取得
    llvm_dir = find_expand_dir( "llvm" );
    
    #clangを取得
    begin

      #Clangをダウンロードする
      begin

        ca = conf[ :archive_list ].find do |i|
          /cfe/ =~ i
        end
        c = { :archive_path => conf[:archive_save_path] ,
          :archive => ca,
          :work_path => conf[:work_path]
        }.freeze
        pp c

        Common::archive_download ( c  )
      end

      
      #clangを展開
      begin
        t = conf[ :archive_list ].find do |i|
          /cfe/ =~ i
        end
        
        pp t 
        pp conf[ :archive_list ]
        
        Common::archive_expand( File::basename(t) )
      end

      #clanのディレクトリを取得
      clang_dir = find_expand_dir( "cfe" );

      #clangをllvm上の所定位置に移動
      Common::system_cmd( " mv -v #{ clang_dir } #{ llvm_dir }/tools/clang " )
      
    end
    
    
    #コンパイル用文字列を作成
    cmd = <<EOS
cd #{ llvm_dir } && 
export CC=gcc &&
export CXX=g++ &&
./configure --prefix=#{conf[:path]} &&
make -j #{conf[:parallel]} &&
make install 
EOS
    
    Common::system_cmd( cmd )
    
  end
  
end

#-------------------
#基本設定
#-------------------

#インストールパス
install_path = `echo $HOME`.strip + "/local"
#作業ディレクトリパス
work_path = Dir::pwd + "/work/"

#基準ディレクトリ
base_path = Dir::pwd

#アーカイブディレクトリ
archive_path = Dir::pwd + "/archive/"

#作業ディレクトリを消去
Common::system_cmd( "rm -rfv #{work_path}" ) 
Common::system_cmd( "mkdir -p #{work_path}" ) 
Common::system_cmd( "mkdir -p #{archive_path}" ) 

#必要なパッケージ
archive = %w(
http://tukaani.org/xz/xz-5.0.5.tar.gz
http://mirror.jre655.com/GNU/emacs/emacs-24.3.tar.xz
https://gmplib.org/download/gmp/gmp-5.1.3.tar.bz2
http://www.mpfr.org/mpfr-current/mpfr-3.1.2.tar.xz
ftp://ftp.gnu.org/gnu/mpc/mpc-1.0.2.tar.gz
http://ftp.tsukuba.wide.ad.jp/software/gcc/releases/gcc-4.9.1/gcc-4.9.1.tar.bz2
http://www.python.org/ftp/python/2.7.6/Python-2.7.6.tar.xz
http://llvm.org/releases/3.4.2/llvm-3.4.2.src.tar.gz
http://llvm.org/releases/3.4.2/cfe-3.4.2.src.tar.gz
http://ftp.gnu.org/gnu/gdb/gdb-7.8.tar.xz
http://cache.ruby-lang.org/pub/ruby/2.1/ruby-2.1.2.tar.gz
http://ftp.gnu.org/gnu/screen/screen-4.2.1.tar.gz
http://samba.org/ftp/ccache/ccache-3.1.9.tar.bz2
)





#直接インストールしないアーカイブ
dont_use_direct = %( cfe ) 


#-------------------
#プログラムMain部
#-------------------

#対象のファイルアーカイブ
archive.each do |i|

  #必要なアーカイブをダウンロードする
  begin
    conf = { :archive_path => archive_path ,
      :archive => i,
      :work_path => work_path
    }.freeze
    Common::archive_download( conf )
  end
  
  #コンパイル用引数を作成する
  #conf = Hash.new
  conf = { :path => install_path ,
    :archive_web => i,
    :archive_path => File::basename(i),
    :parallel => 3,
    :work_path => work_path,
    :archive_list => archive,
    :archive_save_path => archive_path,
  }.freeze

  #作業ディレクトリに移動
  Dir::chdir(work_path)
  p Dir::pwd
  #プログラム独自で定義された関数が存在するか判定する
  t = File::basename(i).split(/\.|\-/).shift
  
  #直接インストールするプログラムの場合
  if false == dont_use_direct.include?( t )

    if true == Compile_protocol.methods.include?(t.to_sym)
      p Compile_protocol.send("#{t}", conf)
      
    else
      Compile_protocol::basic( conf )
    end
    
  end

end
