class BunnyRabbit extends Animal {
	constructor(string name, int weight, string ears) {
		super(name, weight);
		this.ears = ears;
	}
	function int getWeight() {
		return this.weight;
	}
	function void setWeight(int newWeight) {
		this.weight = newWeight;
	}
	function string getName() {
		return this.name;
	}
	function string getEars() {
		return this.ears;
	}
	function void setEars(string newEars) {
		this.ears = newEars;
	}
}
BunnyRabbit bugsBunny = new BunnyRabbit("Bugs Bunny", 15, "Floppy");
