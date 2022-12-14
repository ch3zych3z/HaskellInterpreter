#!/bin/bash 

cd demos || exit 1
tests=$(find . -type f -name "*.hs")
cd ..
stack build

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

for test in $tests
do
    stack runhaskell "demos/$test" > expected
    stack run "demos/$test" > actual
    dif=$(diff expected actual)
    if [ $? -eq 0 ] ; then
        echo -e "${GREEN}[Test $test] done${NC}"
    else
        echo -e "${RED}[Test $test] failed ${NC}"
        echo -e "${YELLOW}Expected:${NC}"
        cat expected
        echo -e "${YELLOW}Actual:${NC}"
        cat actual
        exit 1
    fi

done

rm expected actual
exit 0