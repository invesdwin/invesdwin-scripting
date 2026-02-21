println!("getCharacter");
let getCharacter: char = callback("getCharacter", &[]);
println!("getCharacter: {}", getCharacter);
callback_void("setCharacter", &[param(getCharacter)]);

println!("getCharacterVector");
let getCharacterVector: Vec<char> = callback("getCharacterVector", &[]);
println!("getCharacterVector: {:?}", getCharacterVector);
callback_void("setCharacterVector", &[param(getCharacterVector)]);

println!("getCharacterVectorAsList");
let getCharacterVectorAsList: Vec<char> = callback("getCharacterVectorAsList", &[]);
println!("getCharacterVectorAsList: {:?}", getCharacterVectorAsList);
callback_void("setCharacterVectorAsList", &[param(getCharacterVectorAsList)]);

println!("getCharacterMatrix");
let getCharacterMatrix: Vec<Vec<char>> = callback("getCharacterMatrix", &[]);
println!("getCharacterMatrix: {:?}", getCharacterMatrix);
callback_void("setCharacterMatrix", &[param(getCharacterMatrix)]);

println!("getCharacterMatrixAsList");
let getCharacterMatrixAsList: Vec<Vec<char>> = callback("getCharacterMatrixAsList", &[]);
println!("getCharacterMatrixAsList: {:?}", getCharacterMatrixAsList);
callback_void("setCharacterMatrixAsList", &[param(getCharacterMatrixAsList)]);
