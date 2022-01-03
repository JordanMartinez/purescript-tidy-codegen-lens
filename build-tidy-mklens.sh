#!/usr/bin/env bash

# Simple script for regenerating the `tidy-mklens` executable script.

set -x

spago bundle-app -m Main -t out

echo '#!/usr/bin/env node' > tidy-mklens
cat out >> tidy-mklens
chmod +x tidy-mklens
rm out
