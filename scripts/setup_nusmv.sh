set -ex
cd ../contrib/
wget https://nusmv.fbk.eu/distrib/NuSMV-2.6.0-linux64.tar.gz
tar -xzvf NuSMV-2.6.0-linux64.tar.gz
export PATH=$PATH:`pwd`/NuSMV-2.6.0-linux64/bin/
echo "export PATH=$PATH:`pwd`/NuSMV-2.6.0-linux64/bin/" >> ~/.bashrc