use std::mem;
use std::cmp::Ordering;

#[derive(Debug)]
pub struct Tree {
  link: Link,
}

#[derive(Debug)]
struct Node {
    elem: u64,
    lesser: Link,
    greater: Link,
}

type Link = Option<Box<Node>>;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////

fn insert_link(new_elem: u64, link: &mut Link) {
    match mem::replace(link, None) {
        None => {
            let n = Node { elem: new_elem, greater: None, lesser: None };
            *link = Some(Box::new(n));
        }
        Some(n_box) => {
            let mut n = *n_box;
            match new_elem.cmp(&n.elem) {
                Ordering::Less    => insert_link(new_elem, &mut n.lesser),
                Ordering::Greater => insert_link(new_elem, &mut n.greater),
                Ordering::Equal   => (),
            };
            *link = Some(Box::new(n))
        }
    }
}

pub fn insert(new_elem: u64, tree: &mut Tree) {
    insert_link(new_elem, &mut tree.link);
}

fn link_contains(new_elem: u64, link: &Link) -> bool {
    match *link {
        None => false,
        Some(ref node) => {
            match new_elem.cmp(&node.elem) {
                Ordering::Less    => link_contains(new_elem, &node.lesser),
                Ordering::Greater => link_contains(new_elem, &node.greater),
                Ordering::Equal   => true,
            }
        }
    }
}

pub fn contains(new_elem: u64, tree: &Tree) -> bool {
    link_contains(new_elem, &tree.link)
}

pub fn mk_singleton(x: u64) -> Tree {
    let elem = Node { elem: x, lesser: None, greater: None };
    Tree { link: Some(Box::new(elem)) }
}

pub fn mk_empty() -> Tree {
    Tree { link: None }
}

#[cfg(test)]
mod tests {
    use super::contains;
    use super::insert;
    use super::mk_empty;

    #[test]
    fn it_works() {
      let mut bst = &mut mk_empty();

      insert(1, &mut bst);
      insert(0, &mut bst);
      insert(2, &mut bst);

      assert!(contains(0, &bst));
      assert!(contains(1, &bst));
      assert!(contains(2, &bst));

      assert!(!contains(3, &bst));
    }
}
