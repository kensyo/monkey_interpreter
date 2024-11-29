use monkey_interpreter::repl::start;
use whoami;

fn main() {
    let username = whoami::username();

    println!("Hello {}! This is the Monkey programming language!", username);
    println!("Feel free to tyep in commands");
    start();
}
