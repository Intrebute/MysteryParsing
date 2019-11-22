use mystery_dialogue_parsing::*;

fn main() {
    use std::fs;
    match fs::read_to_string("res/test_dialogue.txt") {
        Ok(contents) => match forestify(&contents) {
            Ok(parsed_forest) => {
                println!(
                    "Success!\nContents: \n{}\nParsed tree:\n{:#?}",
                    contents, parsed_forest
                );
            }
            Err(e) => {
                println!("Error parsing file: {:?}", e);
            }
        },
        Err(e) => {
            println!("Error loading file: {:?}", e);
        }
    }
    println!("Done!");
}
