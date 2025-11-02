#!/bin/bash

APP_PORT=8090

cabal run -- app -d "postgresql://localhost/quyon?user=postgres&password=postgres" \
  -m -p $APP_PORT 

