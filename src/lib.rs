use std::cmp::PartialEq;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::string::String;

use html_parser::{Dom, Element, Node};

#[derive(Debug)]
pub enum Error {
    ParseError(String),
}

enum RSXElementKind {
    Element,
    Text,
    Comment
}

pub struct RSXElement{
    name: Option<String>,
    attributes: BTreeMap<String, Option<String>>,
    children: Vec<RSXElement>,
    content: Option<String>,
    kind: RSXElementKind,
    depth: usize
}

impl RSXElement {
    fn new(name: Option<String>) -> RSXElement {
        RSXElement{
            kind: RSXElementKind::Element,
            name,
            attributes: Default::default(),
            children: vec![],
            content: None,
            depth: 0
        }
    }
}

impl RSXElement {
    fn add_child(&mut self, mut child: RSXElement) {
        child.depth = self.depth + 1;
        self.children.push(child)
    }
}

impl From<Element> for RSXElement {
    fn from(html_element: Element) -> Self {
        let mut rsx_element = RSXElement::new(Option::from(html_element.name));

        if let Some(id) = html_element.id {
            rsx_element.attributes.insert("id".to_string(), Some(id));
        }

        for (attr, val) in &html_element.attributes {
            match val {
                None => {
                    rsx_element.attributes.insert(attr.to_string(), None);
                }
                Some(val) => {
                    rsx_element.attributes.insert(attr.to_string(), Some(val.clone()));
                }
            }
        }

        if !html_element.classes.is_empty() {
            rsx_element.attributes.insert("class".to_string(), Some(html_element.classes.join(" ")));
        }

        html_element
            .children
            .into_iter()
            .for_each(|e| rsx_element.add_child(RSXElement::from(e)));

        rsx_element
    }
}

impl From<Node> for RSXElement {
    fn from(node: Node) -> Self {
        match node {
            Node::Text(t) => {
                let mut el = RSXElement::new(None);
                el.kind = RSXElementKind::Text;
                el.content = Some(t);

                el
            },
            Node::Comment(c) => {
                let mut el = RSXElement::new(None);
                el.kind = RSXElementKind::Comment;
                el.content = Some(c);

                el
            },
            Node::Element(e) => RSXElement::from(e),
        }
    }
}

impl PartialEq for RSXElementKind {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Display for RSXElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            RSXElementKind::Element => {
                let mut text : String = match &self.name {
                    None => "".to_string(),
                    Some(n) => String::from(format!("{:indent$}{} {{\n", "", n, indent=self.depth*2))
                };

                for (attribute, value) in &self.attributes {
                    let attr_str = if let Some(value) = value {
                        format!("{:indent$}  {attribute}: \"{value}\",\n", "", indent=self.depth*2)
                    } else {
                        format!("{:indent$}  {attribute},\n", "", indent=self.depth*2)
                    };

                    text.push_str(attr_str.as_str());
                }

                match &self.content {
                    None => {}
                    Some(c) => text.push_str(format!("{:indent$}  \"{c}\"\n", "", indent=self.depth*2).as_str())
                }

                for child in &self.children {
                    match child.kind {
                        RSXElementKind::Element => {
                            text.push_str(",\n")
                        }
                        RSXElementKind::Text => {}
                        RSXElementKind::Comment => {}
                    }
                    text.push_str(child.to_string().as_str());
                }

                text.push_str(format!("\n{:indent$}}}", "", indent=self.depth*2).as_str());

                write!(f, "{}", text)
            },
            RSXElementKind::Text => {
                let indent = match self.depth % 2  {
                    0 => 0,
                    _ => self.depth * 2
                };
                write!(f, "{}", match &self.content {
                    None => "".to_string(),
                    Some(c) => format!("{:indent$}  \"{}\"", "", c.to_string(), indent=indent),
                })
            },
            RSXElementKind::Comment => write!(f, "/// {}", match &self.content {
                None => "".to_string(),
                Some(c) => c.to_string()
            })
        }
    }
}

pub fn parse_to_rsx(html: &str) -> Result<Vec<RSXElement>, Error> {
    let dom = Dom::parse(html).map_err(|e| Error::ParseError(format!("{e}")))?;
    let mut res = vec![];
    for c in dom.children {
        res.push(RSXElement::from(c))
    }

    Ok(res)
}

pub fn parse(html: &str) -> Result<String, Error> {
    match parse_to_rsx(html) {
        Ok(el) => {
            let strings = el
                .into_iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("\n");

            Ok(strings)
        },
        Err(e) => Err(e)
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;

    #[test]
    fn can_transform_simple() {
        assert_eq!("div {\n\n}".to_string(), parse("<div/>").expect("should parse html"))
    }

    #[test]
    fn can_transform_with_attributes() {
        assert_eq!("div {\n  bingo: \"bongo\",\n  class: \"foo bar\",\n  disabled,\n    \"Bla\"\n}", parse("<div class=\"foo bar\" bingo=\"bongo\" disabled>Bla</div>").expect("should parse html"))
    }

    #[test]
    fn can_print_nested() {
        assert_eq!("div {\n  bingo: \"bongo\",\n  class: \"foo bar\",\n  disabled,\n    \"Bla\",\n  p {\n    foo: \"bar\",\n    \"Test\"\n  }\n}", parse("<div class=\"foo bar\" bingo=\"bongo\" disabled>Bla<p foo=\"bar\">Test</p></div>").expect("should parse html"))
    }
}