#!/bin/bash
#!/usr/bin/env bash
echo "0$(runghc Main.hs | pigz -z | base64 -w0)"