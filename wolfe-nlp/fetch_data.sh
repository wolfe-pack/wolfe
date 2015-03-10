#!/bin/bash

mkdir data

# Number and gender data
rm data/gender.data
letters=( a b c d e f )
for letter in "${letters[@]}"; do
  wget http://www.clsp.jhu.edu/~sbergsma/Gender/Data/gender.a${letter}.gz
  gunzip gender.a${letter}.gz
  cat gender.a${letter} >> data/gender.data
  rm gender.a${letter}
done

# Brown clusters
# wget http://people.csail.mit.edu/maestro/papers/bllip-clusters.gz
# gunzip bllip-clusters.gz
# mv bllip-clusters data/

# Berkeley models
wget http://nlp.cs.berkeley.edu/downloads/berkeley-entity-models.tgz
tar -xzf berkeley-entity-models.tgz
rm berkeley-entity-models.tgz

