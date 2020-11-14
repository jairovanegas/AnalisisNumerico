const lineByLine = require('n-readlines');
const fs = require("fs");
const liner = new lineByLine("parse.txt");
let expression = String(liner.next());
let argument = Number(liner.next());
let oStream = fs.createWriteStream(`${process.argv[2]}.txt`);
let indexes = expression.split("$");
console.log(indexes);
for (let i = 0; i < argument; i++) {
    let reconstructed = ""
    for (let j = 0; j < indexes.length-1; j++) {
        reconstructed = reconstructed + indexes[j] + String(i);
    }
    reconstructed = reconstructed + indexes[indexes.length-1];
    console.log(reconstructed);
    oStream.write(reconstructed);
}
oStream.close();