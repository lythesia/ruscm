use std::rc::Rc;
//use std::cell::RefCell;
use std::fmt;
use std::iter;

pub struct PeekWhile<'a, I, F> where I: Iterator + 'a {
    iter: &'a mut iter::Peekable<I>,
    f: F
}

impl<'a, I, F> Iterator for PeekWhile<'a, I, F> where
    I: Iterator + 'a,
    <I as Iterator>::Item: fmt::Debug,
    F: for <'b> FnMut(&'b <I as Iterator>::Item) -> bool
{
    type Item = <I as Iterator>::Item;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let &mut PeekWhile { ref mut iter, ref mut f } = self;
//        let i = iter.peek().unwrap();
//        println!("peek: {:?}, with f: {:?}", i, f(i));
        if iter.peek().map(f).unwrap_or(false) {
            iter.next()
        } else {
            None
        }
    }
}

pub fn peek_while<'a, I, F>(iter: &'a mut iter::Peekable<I>, f: F) -> PeekWhile<'a, I, F> where
    I: Iterator + 'a,
    F: for <'b> FnMut(&'b <I as Iterator>::Item) -> bool,
{
    PeekWhile { iter, f }
}

pub enum List<T> {
    Cons(Rc<T>, Rc<List<T>>),
    Nil,
}

impl<T> List<T> {
    pub fn is_nil(&self) -> bool {
        match self {
            List::Cons(_, _) => false,
            _ => true,
        }
    }
    
    pub fn iter(&self) -> Iter<T> {
        Iter(self)
    }
}

#[macro_export]
macro_rules! list {
    () => (
        $crate::internals::List::Nil
    );
    ($head:expr $(, $tail:expr)*) => (
        $crate::internals::List::Cons(
            std::rc::Rc::new($head),
            std::rc::Rc::new(list!($($tail),*))
        )
    );
}

pub struct Iter<'a, T>(&'a List<T>);
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    
    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            List::Cons(head, tail) => {
                self.0 = tail;
                Some(head.as_ref())
            },
            _ => None,
        }
    }
}

impl<T> fmt::Debug for List<T>
where T: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: Vec<String> = self.iter().map(|v| format!("{:?}", v)).collect();
        write!(f, "({})", s.join(" "))
    }
}

impl<T> fmt::Display for List<T>
where T: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: Vec<String> = self.iter().map(|v| format!("{}", v)).collect();
        write!(f, "({})", s.join(" "))
    }
}

mod tests {
    use super::*;
    use std::mem;
    
    #[test]
    fn test_new() {
        let l: List<i32> = List::Nil;
        assert!(l.is_nil());
    }
    
    #[test]
    fn test_iter() {
        let l = Rc::new(List::Nil);
        let ll = Rc::new(List::Cons(
            Rc::new(1),
            Rc::new(List::Cons(
                Rc::new(2),
                l.clone()
            ))
        ));
        assert!(l.is_nil());
        
        let mut it = ll.iter();
        assert_eq!(it.next(), Some(&1));
        assert_eq!(it.next(), Some(&2));
        assert_eq!(it.next(), None);
        mem::drop(it);
    
        assert_eq!(Rc::strong_count(&l), 2);
        assert_eq!(Rc::strong_count(&ll), 1);
        
        mem::drop(ll);
        assert_eq!(Rc::strong_count(&l), 1);
    }
    
    #[test]
    fn test_peek_while() {
        let s = "(#\\newline) (end)";
        let mut i = s.chars().peekable();
        assert_eq!(i.peek().unwrap(), &'(');
        assert_eq!(i.next().unwrap(), '(');
        assert_eq!(i.next().unwrap(), '#');
        assert_eq!(i.next().unwrap(), '\\');
        {
            let s = peek_while(&mut i, |c| c.is_alphabetic()).collect::<String>();
            assert_eq!(s, "newline".to_string());
        }
        assert_eq!(i.next().unwrap(), ')');
    }
}