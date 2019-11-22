use std::collections::HashMap;

use indenty::{RoseTree, tree};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{line_ending, not_line_ending, space0},
    combinator::map,
    IResult,
    multi::many0,
    sequence::terminated,
};

pub mod dialogue_action;

#[derive(Debug, PartialEq)]
pub enum Error<PE> {
    NomError(nom::Err<PE>),
    IndentError(indenty::IndentationError),
    GenericError(String),
}

impl<PE> From<nom::Err<PE>> for Error<PE> {
    fn from(e: nom::Err<PE>) -> Self {
        Error::NomError(e)
    }
}

impl<PE> From<indenty::IndentationError> for Error<PE> {
    fn from(e: indenty::IndentationError) -> Self {
        Error::IndentError(e)
    }
}

impl<PE> From<&str> for Error<PE> {
    fn from(i: &str) -> Self {
        Error::GenericError(i.into())
    }
}

impl<PE> From<String> for Error<PE> {
    fn from(i: String) -> Self {
        Error::GenericError(i)
    }
}

pub type DialogueMap = HashMap<String, Vec<DialogueAction>>;

pub enum DialogueAction {
    Say(Vec<String>),
    Ask(Question),
    Still(CharacterStill),
}

pub struct CharacterStill {
    pub character: String,
    pub still: String,
}

pub struct Answer {
    pub option_text: String,
    pub next_action: TagOrBlock,
}

pub struct Question {
    pub question: String,
    pub answers: Vec<Answer>,
}

pub enum TagOrBlock {
    Tag(String),
    Block(Vec<DialogueAction>),
}

pub struct TagBlock {
    pub tag: String,
    pub block: Vec<DialogueAction>,
}

fn hash_tag(input: &str) -> IResult<&str, &str> {
    let (input, rest) = tag("#")(input)?;
    Ok((input, rest))
}

fn empty_line(input: &str) -> IResult<&str, ()> {
    let (input, _) = space0(input)?;
    let (input, _) = line_ending(input)?;

    Ok((input, ()))
}

fn indented_line(input: &str) -> IResult<&str, (&str, &str)> {
    let (input, indentation) = space0(input)?;
    let (input, line) = terminated(not_line_ending, line_ending)(input)?;
    Ok((input, (indentation, line)))
}

fn dense_lines(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    map(
        many0(alt((
            map(empty_line, |()| None),
            map(indented_line, |l| Some(l)),
        ))),
        |mut v| v.drain(..).flatten().collect(),
    )(input)
}

pub fn forestify(input: &str) -> Result<Vec<RoseTree<&str>>, Error<(&str, nom::error::ErrorKind)>> {
    let (_, dlines) = dense_lines(input)?;
    Ok(RoseTree::from_prefixables(dlines.into_iter())?)
}

pub trait Parser<I, O, E>: Fn(I) -> Result<O, E> {}

impl<I, O, E, T> Parser<I, O, E> for T where T: Fn(I) -> Result<O, E> {}

mod tree_parsing {
    use indenty::RoseTree;
    use nom::{Compare, InputTake};
    use nom::error::ParseError;

    use crate::{DialogueAction, Error, Parser};

    pub type TResult<I, O, E = String> = Result<O, Error<(I, E)>>;

    pub fn node<T, E>(tree: &RoseTree<T>) -> TResult<RoseTree<T>, &T, E> {
        if tree.children.len() == 0 {
            Ok(&tree.value)
        } else {
            Err("node should not have children".into())
        }
    }

    pub fn all_children<'t, T: 't, O, P, E>(p: P) -> impl Parser<&'t RoseTree<T>, Vec<O>, E>
        where
            P: Parser<&'t RoseTree<T>, O, E>,
    {
        move |tree: &'t RoseTree<T>| -> Result<Vec<O>, E> {
            let mut result = vec![];
            for c in &tree.children {
                match p(&c) {
                    Ok(o) => {
                        result.push(o);
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
            Ok(result)
        }
    }

    pub fn vector_tree<'t, T: 't, E>(
        tree: &RoseTree<T>,
    ) -> TResult<&RoseTree<T>, (&T, Vec<&T>), E> {
        let children: Vec<&T> =
            all_children(node)(tree).map_err(|_: Error<(RoseTree<T>, E)>| {
                Error::GenericError("all children should be nodes".into())
            })?;
        Ok((&tree.value, children))
    }

    pub fn say<'t, E: ParseError<&'t str>>(
        tree: &'t RoseTree<&'t str>,
    ) -> TResult<&'t RoseTree<&'t str>, DialogueAction, E> {
        use nom::bytes::complete::tag;
        use nom::IResult;
        let (head, contents) = vector_tree(tree)?;
        // let r: IResult<&str, &str> = tag("Say:")(*head);
        tag("Say:")(*head)
            .map(|_| {
                DialogueAction::Say(
                    contents
                        .into_iter()
                        .map(|l| l.to_string())
                        .collect::<Vec<String>>(),
                )
            })
            .map_err(|_: nom::Err<nom::error::VerboseError<&str>>| {
                Error::GenericError("SAY".to_string())
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let i = tree![0 => tree![2], tree![3], tree![5]];
        let r: tree_parsing::TResult<&RoseTree<i32>, (&i32, Vec<&i32>), String> =
            tree_parsing::vector_tree(&i);
        assert_eq!(r, Ok((&0, vec![&2, &3, &5])));
    }
}
