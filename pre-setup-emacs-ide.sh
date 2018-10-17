### Install git
sudo apt-get install git
# config git
git config --global user.name "roaman"
git config --global user.email liguojun518@gmail.com
git config --global core.editor 'emacs -q'
# git config --global credical
git config --global rerere.enabled true

### common part
##sudo apt-get install ttf-bitstream-vera
##sudo apt-get install fonts-inconsolata
##use Source code pro font
## download from: https://github.com/adobe-fonts/source-code-pro/downloads
## unzip SourceCodePro_FontsOnly-1.013.zip to /usr/share/fonts/truetype/
fc-cache -f -v

sudo apt-get install dbus-x11

### aspell
sudo apt-get install aspell

### install ggtags  for ubuntu
# 1. download the Global
wget http://tamacom.com/global/global-6.5.7.tar.gz
# 2. install dependency package
sudo apt-get install gcc
sudo apt-get install libncurses5-dev
## 3. install on OS
# tar xvf global-6.5.7.tar.gz
# cd global-6.5.7
# ./configure [--with-exuberant-ctags=/usr/local/bin/ctags]
##If you have ctags installed, add the option --with-exuberant-ctags and supply the installed path:
## 4. compile and install
# make
# sudo make install
## 5.install emacs frontend in emacs
## refer to init.el

### org-mode
## support the image capture and display
# sudo apt-get install scrot

### python
sudo apt-get install python-pip
sudo apt-get install ipython

pip install jedi flake8 importmagic autopep8
## if not use autopep8, you can use yapf
# pip install yapf
