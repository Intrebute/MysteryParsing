use std::collections::HashMap;

pub type DialogueMap<Msg, Stl, Tg> = HashMap<Tg, Vec<DialogueAction<Msg, Stl, Tg>>>;

#[derive(PartialEq, Eq, Debug)]
pub enum DialogueAction<Msg, Stl, Tg> {
    Say(Msg),
    Ask(Question<Msg, Stl, Tg>),
    Still(Stl),
    Tag(Tg),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Answer<Msg, Stl, Tg> {
    pub option_text: Msg,
    pub next_action: Vec<DialogueAction<Msg, Stl, Tg>>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Question<Msg, Stl, Tg> {
    pub question: Msg,
    pub answers: Vec<Answer<Msg, Stl, Tg>>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct TaggedBlock<Msg, Stl, Tg> {
    pub tag: Tg,
    pub block: Vec<DialogueAction<Msg, Stl, Tg>>,
}

pub fn end_slice<T>(i: &[T]) -> &[T] {
    &i[i.len()..]
}

pub fn end_str(i: &str) -> &str {
    &i[i.len()..]
}

pub mod parsing {
    use std::{iter::Enumerate, slice::Iter};

    use indenty::RoseTree;
    use nom::{
        bytes::complete::{tag, take, take_until},
        combinator::{map, rest},
        InputIter,
        InputTake, IResult, sequence::{delimited, preceded, separated_pair},
    };
    use nom::character::complete::none_of;
    use nom::combinator::all_consuming;
    use nom::error::ErrorKind;
    use nom::sequence::tuple;

    use crate::tree_parsing::TResult;

    use super::Answer;
    use super::DialogueAction;
    use super::Question;
    use super::TaggedBlock;

    type Forest<T> = [RoseTree<T>];

    /// StringSay's parse the pattern:
    /// Say:
    ///     Line 1
    ///     Line 2
    ///     Line 3
    #[derive(PartialOrd, PartialEq, Debug)]
    pub struct StringSay(pub Vec<String>);

    pub fn parse_string_say<'f, 's>(
        input: &'f [RoseTree<&'s str>],
    ) -> Result<(&'f Forest<&'s str>, StringSay), String> {
        let (head, rest) = match input.first() {
            Some(f) => (f, &input[1..]),
            None => { return Err("Empty sequence.".to_string()); }
        };
        tag("Say:")(head.value).map_err(|e: nom::Err<(&'s str, ErrorKind)>| format!("Command is not \"Say:\"\nError: {:?}", e))?;
        let result: Result<Vec<String>, String> = head.children.iter().map(|t| {
            let l = node(&t)?;
            Ok(l.to_string())
        }).collect();
        let lines = result?;
        Ok((rest, StringSay(lines)))
    }

    /// A CharPoseStringStill parses the following pattern:
    /// [character|pose]
    #[derive(PartialOrd, PartialEq, Debug)]
    pub struct CharPoseStringStill {
        pub character: String,
        pub pose: String,
    }

    pub fn parse_char_pose_string_still<'f, 's>(
        input: &'f Forest<&'s str>,
    ) -> Result<(&'f Forest<&'s str>, CharPoseStringStill), String> {
        let (head, rest) = take_one(input)?;
        let s = node(head).map(|&s| s)?;
        let (_, still) =
            map(all_consuming(tuple((tag("["), take_until("|"), tag("|"), take_until("]"), tag("]")))),
                |(_, c, _, p, _): (&str, &str, &str, &str, &str)| CharPoseStringStill { character: c.to_string(), pose: p.to_string() })(s)
                .map_err(|e: nom::Err<(&str, ErrorKind)>| format!("Text: \"{:?}\" Does not follow [character|pose] format.", e))?;
        Ok((rest, still))
    }

    /// A HashTag parses the following pattern (can include spaces):
    /// #tag
    /// Everything past the pound sign is included. Might(?) trim whitespace at the very end though
    #[derive(PartialOrd, PartialEq, Debug)]
    pub struct HashTag(pub String);

    pub fn parse_hash_tag<'f, 's>(
        input: &'f Forest<&'s str>,
    ) -> Result<(&'f Forest<&'s str>, HashTag), String> {
        let (head, rest) = take_one(input)?;
        let s = node(head).map(|&s| s)?;
        let (_, t) =
            map(tuple((tag("#"), nom::combinator::rest)),
                |(_, r): (&str, &str)| { HashTag(r.to_string()) })(s)
                .map_err(|e: nom::Err<(&str, ErrorKind)>| format!("Text: \"{:?}\" does not start with '#'", e))?;
        Ok((rest, t))
    }

    pub fn parse_question<'f, 's>(
        input: &'f Forest<&'s str>,
    ) -> Result<(&'f Forest<&'s str>, Question<StringSay, CharPoseStringStill, HashTag>), String> {
        let (head, rest) = take_one(input)?;
        let (question, _): (&str, &str) = tag("Ask: ")(head.value).map_err(|e: nom::Err<(&str, ErrorKind)>| {
            format!("\"{:?}\" is not \"Ask\"", e).to_string()
        })?;
        let question = StringSay(vec![question.to_string()]);
        let answers = repeat_until_empty(parse_answer)(&head.children)?;
        Ok((rest, Question { question, answers }))
    }

    pub fn parse_answer<'f, 's>(
        input: &'f Forest<&'s str>,
    ) -> Result<(&'f Forest<&'s str>, Answer<StringSay, CharPoseStringStill, HashTag>), String> {
        let (head, rest) = take_one(input)?;
        let answer = StringSay(vec![head.value.to_string()]);
        let next_actions: Vec<DialogueAction<StringSay, CharPoseStringStill, HashTag>> = repeat_until_empty(parse_dialogue_action)(&head.children)?;
        Ok((rest, Answer { option_text: answer, next_action: next_actions }))
    }

    pub fn parse_dialogue_action<'f, 's>(
        input: &'f Forest<&'s str>,
    ) -> Result<(&'f Forest<&'s str>, DialogueAction<StringSay, CharPoseStringStill, HashTag>), String> {
        if let Ok((rest, say)) = parse_string_say(input) {
            Ok((rest, DialogueAction::Say(say)))
        } else if let Ok((rest, ask)) = parse_question(input) {
            Ok((rest, DialogueAction::Ask(ask)))
        } else if let Ok((rest, pose)) = parse_char_pose_string_still(input) {
            Ok((rest, DialogueAction::Still(pose)))
        } else if let Ok((rest, tag)) = parse_hash_tag(input) {
            Ok((rest, DialogueAction::Tag(tag)))
        } else {
            Err(format!("\"{:?}\" is not a valid dialogue action", input))
        }
    }

    pub fn node<I>(input: &RoseTree<I>) -> Result<&I, String> {
        if input.children.is_empty() {
            Ok(&input.value)
        } else {
            Err("Node cannot have child values.".to_string())
        }
    }

    pub fn take_one<I>(input: &[I]) -> Result<(&I, &[I]), String> {
        Ok(match input.first() {
            Some(h) => (h, &input[1..]),
            None => { return Err("Empty sequence.".to_string()); }
        })
    }

    pub fn repeat_until_empty<I, O, E>(
        p: impl Fn(I) -> Result<(I, O), E>,
    ) -> impl Fn(I) -> Result<Vec<O>, E>
        where
            I: as_slice::AsSlice,
    {
        move |input| {
            let mut current_input: I = input;
            let mut outputs: Vec<O> = vec![];
            loop {
                if current_input.as_slice().is_empty() {
                    return Ok(outputs);
                } else {
                    let (next_input, output) = p(current_input)?;
                    current_input = next_input;
                    outputs.push(output);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use indenty::{RoseTree, tree};

    use crate::dialogue_action::parsing::{CharPoseStringStill, HashTag, parse_char_pose_string_still, parse_hash_tag, parse_string_say, StringSay};

    use super::*;

    #[test]
    fn test_string_say() {
        let source: &[RoseTree<&str>] = &[tree![
            "Say:" =>
                tree!["Hoi hoi!"],
                tree!["Hai hai!"],
                tree!["Hhahaha"],
        ]];
        let should_result: StringSay = StringSay(vec!["Hoi hoi!".to_string(), "Hai hai!".to_string(), "Hhahaha".to_string()]);
        assert_eq!(parse_string_say(&source), Ok((end_slice(source), should_result)));
    }

    #[test]
    fn test_char_pose_string_still() {
        let source: &[RoseTree<&str>] = &[tree![
            "[character|pose]"
        ]];
        let should_result: CharPoseStringStill = CharPoseStringStill {
            character: "character".to_string(),
            pose: "pose".to_string(),
        };
        assert_eq!(parse_char_pose_string_still(source), Ok((end_slice(source), should_result)));
    }

    #[test]
    fn test_hash_tag() {
        let source: &[RoseTree<&str>] = &[tree![
            "#tag"
        ]];
        let should_result: HashTag = HashTag("tag".to_string());
        assert_eq!(parse_hash_tag(source), Ok((end_slice(source), should_result)));
    }
}
