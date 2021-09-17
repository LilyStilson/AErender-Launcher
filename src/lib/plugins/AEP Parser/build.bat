@ECHO off
echo "Building aeparser_win.exe..."
set GOOS=windows
set GOARCH=amd64
go build -o "aeparser_win.exe"

echo "Building aeparser_mac..."
set GOOS=darwin
set GOARCH=amd64
go build -o "aeparser_mac"


