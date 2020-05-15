mod parser {
use std::iter::Peekable;


#[derive(Debug, Clone)]
pub enum GrammarItem {
    Product,
    Sum,
    Number(u64),
    Paren
}

#[derive(Debug, Clone)]
pub struct ParseNode {
    pub children: Vec<ParseNode>,
    pub entry: GrammarItem,
}

impl ParseNode {
    pub fn new() -> ParseNode {
        ParseNode {
            children: Vec::new(),
            entry: GrammarItem::Paren,
        }
    }
}

#[derive(Debug, Clone)]
pub enum LexItem {
    Paren(char),
    Op(char),
    Num(u64),
}

fn lex(input: &String) -> Result<Vec<LexItem>, String> {
    let mut result = Vec::new();

    let mut it = input.chars().peekable();
    while let Some(&c) = it.peek() {
        match c {
            '0'..='9' => {
                it.next();
                let n = get_number(c, &mut it);
                result.push(LexItem::Num(n));
            }
            '+' | '*' => {
                result.push(LexItem::Op(c));
                it.next();
            }
            '(' | ')' | '[' | ']' | '{' | '}' => {
                result.push(LexItem::Paren(c));
                it.next();
            }
            ' ' => {
                it.next();
            }
            _ => {
                return Err(format!("unexpected character {}", c));
            }
        }
    }
    Ok(result)
}

fn get_number<T: Iterator<Item = char>>(c: char, iter: &mut Peekable<T>) -> u64 {
    let mut number = c.to_string().parse::<u64>().expect("The caller should have passed a digit.");
    while let Some(Ok(digit)) = iter.peek().map(|c| c.to_string().parse::<u64>()) {
        number = number * 10 + digit;
        iter.next();
    }
    number
}

pub fn parse(input: &String) -> Result<ParseNode, String> {
    let tokens = lex(input)?;
    parse_expr(&tokens, 0).and_then(|(n, i)| if i == tokens.len() {
        Ok(n)
    } else {
        Err(format!("Expected end of input, found {:?} at {}", tokens[i], i))
    })
}

fn parse_expr(tokens: &Vec<LexItem>, pos: usize) -> Result<(ParseNode, usize), String> {
    let (node_summand, next_pos) = parse_summand(tokens, pos)?;
    let c = tokens.get(next_pos);
    match c {
        Some(&LexItem::Op('+')) => {
            // recurse on the expr
            let mut sum = ParseNode::new();
            sum.entry = GrammarItem::Sum;
            sum.children.push(node_summand);
            let (rhs, i) = parse_expr(tokens, next_pos + 1)?;
            sum.children.push(rhs);
            Ok((sum, i))
        }
        _ => {
            // we have just the summand production, nothing more.
            Ok((node_summand, next_pos))
        }
    }
}

fn parse_summand(tokens: &Vec<LexItem>, pos: usize) -> Result<(ParseNode, usize), String> {
    let (node_term, next_pos) = parse_term(tokens, pos)?;
    let c = tokens.get(next_pos);
    match c {
        Some(&LexItem::Op('*')) => {
            // recurse on the summand
            let mut product = ParseNode::new();
            product.entry = GrammarItem::Product;
            product.children.push(node_term);
            let (rhs, i) = parse_summand(tokens, next_pos + 1)?;
            product.children.push(rhs);
            Ok((product, i))
        }
        _ => {
            // we have just the term production, nothing more.
            Ok((node_term, next_pos))
        }
    }
}

fn parse_term(tokens: &Vec<LexItem>, pos: usize) -> Result<(ParseNode, usize), String> {
    let c: &LexItem = tokens.get(pos)
        .ok_or(String::from("Unexpected end of input, expected paren or number"))?;
    match c {
        &LexItem::Num(n) => {
            let mut node = ParseNode::new();
            node.entry = GrammarItem::Number(n);
            Ok((node, pos + 1))
        }
        &LexItem::Paren(c) => {
            match c {
                '(' | '[' | '{' => {
                    parse_expr(tokens, pos + 1).and_then(|(node, next_pos)| {
                        if let Some(&LexItem::Paren(c2)) = tokens.get(next_pos) {
                            if c2 == matching(c) {
                                // okay!
                                let mut paren = ParseNode::new();
                                paren.children.push(node);
                                Ok((paren, next_pos + 1))
                            } else {
                                Err(format!("Expected {} but found {} at {}",
                                            matching(c),
                                            c2,
                                            next_pos))
                            }
                        } else {
                            Err(format!("Expected closing paren at {} but found {:?}",
                                        next_pos,
                                        tokens.get(next_pos)))
                        }
                    })
                }
                _ => Err(format!("Expected paren at {} but found {:?}", pos, c)),
            }
        }
        _ => {
            Err(format!("Unexpected token {:?}, expected paren or number", {
                c
            }))
        }
    }
}

fn matching(c: char) -> char {
    match c {
        ')' => '(',
        ']' => '[',
        '}' => '{',
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _ => panic!("should have been a parenthesis!"),
    }
}

pub fn print(tree: &ParseNode) -> String {
    match tree.entry {
        GrammarItem::Paren => {
            format!("({})",
                    print(tree.children.get(0).expect("parens need one child")))
        }
        GrammarItem::Sum => {
            let lhs = print(tree.children.get(0).expect("sums need two children"));
            let rhs = print(tree.children.get(1).expect("sums need two children"));
            format!("{} + {}", lhs, rhs)
        }
        GrammarItem::Product => {
            let lhs = print(tree.children.get(0).expect("products need two children"));
            let rhs = print(tree.children.get(1).expect("products need two children"));
            format!("{} * {}", lhs, rhs)
        }
        GrammarItem::Number(n) => format!("{}", n),
    }
}
}

use std::env;
use parser::*;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() > 1 {
        println!("The first argument is {}", args[1]);
        println!("{:?}", parse(&args[1]));
    }
}

#[cfg(test)]
mod test {
extern crate quickcheck;
use self::quickcheck::*;
extern crate rand;
use self::rand::Rng;
use self::rand::distributions::{Standard,Distribution};
use parser::*;

impl Distribution<GrammarItem> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> GrammarItem {
        let n = rng.gen_range(0, 4);
        match n {
            0 => GrammarItem::Product,
            1 => GrammarItem::Sum,
            2 => GrammarItem::Paren,
            3 => GrammarItem::Number(rng.gen()),
            _ => panic!("unexpected number"),
        }
    }
}

fn rec_node<G: Gen>(i: u32, g: &mut G) -> ParseNode {
    let mut node = ParseNode::new();
    if i >= 1 {
        loop {
            node.entry = g.gen();
            match node.entry {
                GrammarItem::Paren => {
                    node.children.push(rec_node(i - 1, g));
                    break;
                }
                GrammarItem::Sum | GrammarItem::Product => {
                    node.children.push(rec_node(i - 1, g));
                    node.children.push(rec_node(i - 1, g));
                    break;
                }
                GrammarItem::Number(_) => {}
            }
        }
    } else {
        node.entry = GrammarItem::Number(g.gen());
    }
    node
}

impl Arbitrary for ParseNode {
        fn arbitrary<G: Gen>(g: &mut G) -> ParseNode {
           rec_node(5, g)
        }

        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            empty_shrinker()
        }
}

quickcheck! {
    fn prop(xs: ParseNode) -> bool {
        let pp = print(&xs);
        println!("instance {}", pp);
        let parsed = parse(&pp).unwrap();
        assert!(pp == print(&parsed)); 
        true
    }
}
}
