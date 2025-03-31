# GF-WordNet server installation

1. First make sure your compiler is installed with a flag server: 

```bash
cd gf-core/src/compiler/           
runghc Setup.hs configure -f servef
runghc Setup.hs build
sudo runghc Setup.hs install
```

1. You can test it now by running: 

```bash
gf -server
```

It will also show the root directory (`ROOT_DIR`)

2. Now make sure GF-WordNet is set up

```bash
cd ~/gf-wordnet # your path to gf-wordnet
make
```

3. Copy source files to your server root directory:

```bash
cp -r www/js/* $ROOT_DIR/js
cp -r www/* $ROOT_DIR
cp -r gf $ROOT_DIR
cp Parse.ngf $ROOT_DIR
```

4. Now you can run the server:

```bash
sudo ./build/www-services/WordNetServer $ROOT_DIR 22323 # the number is random
```
