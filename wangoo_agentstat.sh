#!/bin/bash

Rscript wangoo_agentstat.R $1 $2 $3
cat tmp.txt | awk '{gsub(/\\/,"")}1' | awk '{gsub(/{"code":"0","message":"","returnValues":"/,"")}1' | awk '{gsub(/}]"}/,"}]")}1' > tmp.json
python wangoo_agentstat.py
