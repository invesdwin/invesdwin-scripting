putStrLn ( show ( "getDouble" ) )
getDouble = putDouble
putStrLn ( show ( getDouble ) )
:{
if not ( isNaN(getDouble) ) 
then error "getDouble not NaN!"
else pure ()
:}

putStrLn ( show ( "getDoubleVector" ) )
getDoubleVector = putDoubleVector
putStrLn ( show ( getDoubleVector ) )
:{
if not ( isNaN(getDoubleVector!!1) ) 
then error "getDoubleVector[1] not NaN!"
else pure ()
:}

putStrLn ( show ( "getDoubleVectorAsList" ) )
getDoubleVectorAsList = putDoubleVectorAsList
putStrLn ( show ( getDoubleVectorAsList ) )
:{
if not ( isNaN(getDoubleVectorAsList!!1) ) 
then error "getDoubleVectorAsList[1] not NaN!"
else pure ()
:}

putStrLn ( show ( "getDoubleMatrix" ) )
getDoubleMatrix = putDoubleMatrix
putStrLn ( show ( getDoubleMatrix ) )
:{
if not ( isNaN(getDoubleMatrix!!0!!0) ) 
then error "getDoubleMatrix[0][0] not NaN!"
else pure ()
:}
:{
if not ( isNaN(getDoubleMatrix!!1!!1) ) 
then error "getDoubleMatrix[1][1] not NaN!"
else pure ()
:}
:{
if not ( isNaN(getDoubleMatrix!!2!!2) ) 
then error "getDoubleMatrix[2][2] not NaN!"
else pure ()
:}

putStrLn ( show ( "getDoubleMatrixAsList" ) )
getDoubleMatrixAsList = putDoubleMatrixAsList
putStrLn ( show ( getDoubleMatrixAsList ) )
:{
if not ( isNaN(getDoubleMatrixAsList!!0!!0) ) 
then error "getDoubleMatrixAsList[0][0] not NaN!"
else pure ()
:}
:{
if not ( isNaN(getDoubleMatrixAsList!!1!!1) ) 
then error "getDoubleMatrixAsList[1][1] not NaN!"
else pure ()
:}
:{
if not ( isNaN(getDoubleMatrixAsList!!2!!2) ) 
then error "getDoubleMatrixAsList[2][2] not NaN!"
else pure ()
:}
