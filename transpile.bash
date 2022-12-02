#!/bin/bash
# usage: ./transpile.bash <name>

# synonyms
name=$1

#scripts
python3 repl_connection.py <${name}.u0d >${name}.0dA
echo ${name}.0dA
bred/bred-transpile.bash downdirection.bred bred <${name}.0dA >${name}.0dB
echo ${name}.0dB
bred/bred-transpile.bash updirection.bred bred <${name}.0dB >${name}.0dC
echo ${name}.0dC
bred/bred-transpile.bash passthrough.bred bred <${name}.0dC >${name}.0dD1
echo ${name}.0dD1
bred/bred-transpile.bash across.bred bred <${name}.0dD1 >${name}.0dD
echo ${name}.0dD
bred/bred-transpile.bash shortmessage.bred bred <${name}.0dD >${name}.0dE
echo ${name}.0dE
bred/bred-transpile.bash message.bred bred <${name}.0dE >${name}.0dF
echo ${name}.0dF
bred/bred-transpile.bash outputport.bred bred <${name}.0dF >${name}.0dG
echo ${name}.0dG
bred/bred-transpile.bash inputport.bred bred <${name}.0dG >${name}.0dH
echo ${name}.0dH
cp ${name}.0dH ${name}.0d
echo ${name}.0d
