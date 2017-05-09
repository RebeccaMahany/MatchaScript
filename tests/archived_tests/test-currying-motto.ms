fun motto = function fun (string statement) {
	return function fun (string name) {
		return function void (string punctuation) {
			print(statement);
			print(", ");
			print(name);
			print(punctuation);
		};
	};
};

fun pokemonMotto = motto("Gotta catch 'em all");
pokemonMotto("Ash")("!"); /* prints "Gotta catch 'em all, Ash!" */