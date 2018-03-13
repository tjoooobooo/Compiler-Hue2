java -jar puckasm.jar TestDateien/Test.a
java -jar puckln.jar Test.o -op ./TestDateien -out ./out -proc main
cd out
puck.exe Test.main.x
