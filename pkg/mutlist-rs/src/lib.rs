use std::mem;

pub struct List {
  link: Link,
}

struct Node {
    elem: u64,
    next: Link,
}

type Link = Option<Box<Node>>;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////

pub fn prepend(x: u64, l: &mut List) {
    let old_link = mem::replace(&mut l.link, None);
    let new_node = Node { elem: x, next: old_link };
    let new_link = Some(Box::new(new_node));
    *l = List { link: new_link };
}

pub fn head(l: &List) -> Option<u64> {
    match (*l).link {
        None        => None,
        Some(ref n) => Some(n.elem),
    }
}

pub fn pop_(l: &mut List) {
    match mem::replace(&mut l.link, None) {
        None    => (),
        Some(n) => { l.link = n.next; }
    }
}

pub fn pop(l: &mut List) -> Option<u64> {
    match mem::replace(&mut l.link, None) {
        None        => None,
        Some(n_box) => { let n=*n_box;
                         l.link=n.next;
                         Some(n.elem)
                       }
    }
}

pub fn mk_singleton(x: u64) -> List {
    let elem = Node { elem: x, next: None };
    List { link: Some(Box::new(elem)) }
}

pub fn mk_empty() -> List {
    List { link: None }
}

impl Drop for List {
    fn drop(&mut self) {
        let mut cur_link = mem::replace(&mut self.link, None);
        while let Some(mut boxed_node) = cur_link {
            cur_link = mem::replace(&mut boxed_node.next, None);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::pop;
    use super::prepend;
    use super::mk_empty;

    #[test]
    fn it_works() {
        let l = &mut mk_empty();

        prepend(0, l);
        prepend(1, l);

        let one_opt  = pop(l);
        let zero_opt = pop(l);
        let none     = pop(l);

        assert_eq!(none,     None);
        assert_eq!(zero_opt, Some(0));
        assert_eq!(one_opt,  Some(1));
    }
}
