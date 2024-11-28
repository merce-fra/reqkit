set -ex
cd ../contrib/
wget http://www.lrde.epita.fr/dload/spot/spot-2.12.1.tar.gz
tar -xzvf spot-2.12.1.tar.gz
cd spot-2.12.1
./configure --prefix ~/local/
make
make install
echo "export PATH=$PATH:~/local/bin/" >> ~/.bashrc