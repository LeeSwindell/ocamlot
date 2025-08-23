#!/bin/bash

# Test runner with detailed statistics and output

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}===========================================${NC}"
echo -e "${BLUE}       OCamlot Test Suite Runner          ${NC}"
echo -e "${BLUE}===========================================${NC}"
echo

# Start timing
START_TIME=$(date +%s)

# Run tests with output capture
echo -e "${YELLOW}Running all tests...${NC}"
echo

if [ "$1" = "--filter" ] && [ -n "$2" ]; then
    echo -e "${YELLOW}Filter: $2${NC}"
    TEST_OUTPUT=$(dune runtest "$2" --force 2>&1)
else
    TEST_OUTPUT=$(dune runtest --force 2>&1)
fi

EXIT_CODE=$?

# Display the output
echo "$TEST_OUTPUT"
echo

# Parse test results
TOTAL_TESTS=$(echo "$TEST_OUTPUT" | grep -c "Test Successful" || true)
TOTAL_RUN=$(echo "$TEST_OUTPUT" | grep -oE "[0-9]+ tests run" | awk '{sum+=$1} END {print sum}')
TOTAL_TIME=$(echo "$TEST_OUTPUT" | grep -oE "in [0-9]+\.[0-9]+s" | awk '{sum+=$2} END {printf "%.3f", sum}')

# End timing
END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo -e "${BLUE}===========================================${NC}"
echo -e "${BLUE}              Test Summary                ${NC}"
echo -e "${BLUE}===========================================${NC}"
echo

if [ $EXIT_CODE -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
else
    echo -e "${RED}✗ Some tests failed${NC}"
fi

echo
echo -e "Test Suites Run:     ${TOTAL_TESTS:-0}"
echo -e "Total Tests Run:     ${TOTAL_RUN:-0}"
echo -e "Total Test Time:     ${TOTAL_TIME:-0}s"
echo -e "Wall Clock Time:     ${ELAPSED}s"
echo

# List test suites
echo -e "${YELLOW}Test Suites:${NC}"
echo "$TEST_OUTPUT" | grep "Testing \`" | sed 's/Testing `/  - /' | sed "s/'.//"

echo
echo -e "${BLUE}===========================================${NC}"

# If verbose flag is set, show assertions
if [ "$1" = "--verbose" ] || [ "$3" = "--verbose" ]; then
    echo
    echo -e "${YELLOW}Assertions:${NC}"
    echo "$TEST_OUTPUT" | grep "^ASSERT" | sort | uniq -c | sort -rn
    echo
fi

exit $EXIT_CODE