#!/bin/bash
RED='\033[0;31m'
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo -e "${NC}Starting...${NC}"

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo -e "${RED}Please run this script using the source command: \n\tsource build.sh\nExiting...${NC}"
    exit 1
fi

echo -e "${BLUE}Clearing previous build${NC}"
rm -rf out
echo -e "${BLUE}Restoring the project...${NC}"
dotnet restore
echo -e "${BLUE}Building the project...${NC}"
dotnet publish ml-core/ml-core.fsproj -c Release -o out

export PATH="$PATH:out"

echo -e "${GREEN}Sucess, type ml-core <flags> to run the project !${YELLOW}\nIN THIS TERMINAL SESSION AND THIS FOLDER ONLY.${GREEN}\nType ml-core --help to get some help !${NC}"
