use mystery_dialogue_parsing::*;

fn main() {
    use std::fs;
    match fs::read_to_string("res/test_dialogue.txt") {
        Ok(contents) => match forestify(&contents) {
            Ok(parsed_forest) => {
                match dialogue_action::parsing::repeat_until_empty(dialogue_action::parsing::parse_dialogue_action)(&parsed_forest) {
                    Ok(v) => {
                        println!("Success!\nContents:\n----------------\n{}\n----------------\nParsed Result:\n\n{:#?}", contents, v);
                    },
                    Err(e) => {
                        println!("Error parsing commands: {:?}", e);
                    }
                }
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
