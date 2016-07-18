echo Clean up everything
rem @call cleanup.bat

echo Build everything
@call build.bat > build.log

echo Build setup
@call buildsetup.bat

