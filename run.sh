#!/bin/bash
#!/usr/bin/env bash
echo "0$(cabal -v0 new-run | pigz -z | base64 -w0)"