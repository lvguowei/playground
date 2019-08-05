const letters = ["a", "b", "c", "d", "e", "f", "g", "h"];
for (let [index, letter] of letters.entries()) {
  if (index % 3 === 0) {
    console.log(letter);
  }
}

const numbers = [1, 2, 3];
console.log(Object.getOwnPropertySymbols(Object.getPrototypeOf(numbers)));

class Message {
  constructor(text) {
    this.text = text;
  }

  [Symbol.replace](value, newValue) {
    return this.text.replace(value, newValue);
  }
}

const message = new Message("There are no stupid questions");
console.log("stupid".replace(message, "s****"));
console.log("".replace(message, "Yes, "));

const fib = function* () {
  let a = 0;
  let b = 1;
  let index = 0;
  yield [index++, a];
  yield [index++, b];

  while (true) {
    const sum = a + b;
    a = b;
    b = sum;
    yield [index++, b];
  }
};

for (const [index, value] of fib()) {
  if (index > 8) {
    break;
  }
  console.log(value);
}
