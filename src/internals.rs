use std::rc::Rc;
//use std::cell::RefCell;
use std::fmt;
use std::iter;
use std::mem;

use crate::interpreter::RuntimeError;

pub struct PeekWhile<'a, I, F>
where
    I: Iterator + 'a,
{
    iter: &'a mut iter::Peekable<I>,
    f: F,
}

impl<'a, I, F> Iterator for PeekWhile<'a, I, F>
where
    I: Iterator + 'a,
    <I as Iterator>::Item: fmt::Debug,
    F: for<'b> FnMut(&'b <I as Iterator>::Item) -> bool,
{
    type Item = <I as Iterator>::Item;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let &mut PeekWhile {
            ref mut iter,
            ref mut f,
        } = self;
        //        let i = iter.peek().unwrap();
        //        println!("peek: {:?}, with f: {:?}", i, f(i));
        if iter.peek().map(f).unwrap_or(false) {
            iter.next()
        } else {
            None
        }
    }
}

pub fn peek_while<'a, I, F>(iter: &'a mut iter::Peekable<I>, f: F) -> PeekWhile<'a, I, F>
where
    I: Iterator + 'a,
    F: for<'b> FnMut(&'b <I as Iterator>::Item) -> bool,
{
    PeekWhile { iter, f }
}

macro_rules! shift_or_error {
    ($list:expr, $($arg:tt)*) => (
        match $list.shift() {
            Some((car, cdr)) => Ok((car, cdr)),
            _ => Err(RuntimeError {msg: format!($($arg)*)})
        }?
    );
}

#[derive(Clone)]
pub enum List<T> {
    Cons(Box<T>, Box<List<T>>),
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

    pub fn len(&self) -> u32 {
        match self {
            List::Cons(_, tail) => 1 + tail.len(),
            _ => 0,
        }
    }

    pub fn shift(self) -> Option<(T, Self)> {
        match self {
            List::Cons(head, tail) => Some((*head, *tail)),
            _ => None,
        }
    }

    pub fn unshift(self, head: T) -> Self {
        List::Cons(Box::new(head), Box::new(self))
    }

    pub fn unshift_r(self, rear: T) -> Self {
        match self {
            List::Cons(head, tail) => List::Cons(Box::new(*head), Box::new(tail.unshift_r(rear))),
            _ => self.unshift(rear)
        }
    }

    pub fn unpack1(self) -> Result<T, RuntimeError> {
        let (car, cdr) = shift_or_error!(self, "expected list(# = 1), but nil got");
        if !cdr.is_nil() {
            return Err(RuntimeError { msg: format!("expected list (# = 1), but # > 1 got")})
        }
        Ok(car)
    }

    pub fn unpack2(self) -> Result<(T, T), RuntimeError> {
        let (car, cdr) = shift_or_error!(self, "expected list(# = 2), but nil got");
        let (cadr, cddr) = shift_or_error!(cdr, "expected list(# = 2), but # = 1 got");
        if !cddr.is_nil() {
            return Err(RuntimeError { msg: format!("expected list (# = 2), but # > 2 got")})
        }
        Ok((car, cadr))
    }

    pub fn unpack3(self) -> Result<(T, T, T), RuntimeError> {
        let (car, cdr) = shift_or_error!(self, "expected list(# = 3), but nil got");
        let (cadr, cddr) = shift_or_error!(cdr, "expected list(# = 3), but # = 1 got");
        let (caddr, cdddr) = shift_or_error!(cddr, "expected list(# = 3), but # = 2 got");
        if !cdddr.is_nil() {
            return Err(RuntimeError { msg: format!("expected list (# = 2), but # > 3 got")})
        }
        Ok((car, cadr, caddr))
    }
}

impl<T> IntoIterator for List<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> IntoIter<T> {
        IntoIter(self)
    }
}

#[macro_export]
macro_rules! list {
    () => (
        $crate::internals::List::Nil
    );
    ($head:expr $(, $tail:expr)*) => (
        $crate::internals::List::Cons(
            Box::new($head),
            Box::new(list!($($tail),*))
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
            }
            _ => None,
        }
    }
}
pub struct IntoIter<T>(List<T>);
impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let l = mem::replace(&mut self.0, List::Nil);
        match l.shift() {
            Some((car, cdr)) => {
                mem::replace(&mut self.0, cdr);
                Some(car)
            },
            _ => None,
        }
    }
}

impl<T> iter::FromIterator<T> for List<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut iter = iter.into_iter();
        match iter.next() {
            Some(v) => List::Cons(Box::new(v), Box::new(List::from_iter(iter))),
            _ => List::Nil,
        }
    }
}

impl<T> fmt::Debug for List<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: Vec<String> = self.iter().map(|v| format!("{:?}", v)).collect();
        write!(f, "({})", s.join(" "))
    }
}

impl<T> fmt::Display for List<T>
where
    T: fmt::Display,
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
    fn test_list_new() {
        let l: List<i32> = List::Nil;
        assert!(l.is_nil());
    }

    #[test]
    fn test_list_iter() {
        let l = Box::new(List::Nil);
        assert!(l.is_nil());
        let ll = Box::new(List::Cons(
            Box::new(1),
            Box::new(List::Cons(Box::new(2), l)),
        ));

        let mut it = ll.iter();
        assert_eq!(it.next(), Some(&1));
        assert_eq!(it.next(), Some(&2));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_list_iter_collect() {
        let ll = vec![1, 2];

//        let mut it = ll.iter().map(|i| Box::new(*i));
        let ll2 = ll.into_iter().collect::<List<i32>>();
        println!("ll2: {}", ll2);
        let mut it2 = ll2.iter();
        assert_eq!(it2.next(), Some(&1));
        assert_eq!(it2.next(), Some(&2));
        assert_eq!(it2.next(), None);
    }

    #[test]
    fn test_list_len() {
        let l: List<i32> = list!(1, 2, 3);
        assert_eq!(l.len(), 3);
    }

    #[test]
    fn test_list_shift() {
        let l = Box::new(list!(1));
        let (x, y) = l.shift().unwrap();
        assert_eq!(x, 1);
        assert!(y.is_nil());
    }

    #[test]
    fn test_unpack1() {
        let l = Box::new(list!(1));
        assert_eq!(l.unpack1().unwrap(), 1);

        let l2: Box<List<i32>> = Box::new(list!());
        assert!(l2.unpack1().is_err());

        let l3 = Box::new(list!(1, 2));
        assert!(l3.unpack1().is_err());
    }

    #[test]
    fn test_unpack2() {
        let l: Box<List<i32>> = Box::new(list!(1, 2));
        let (x1, y1) = l.unpack2().unwrap();
        assert_eq!(x1, 1);
        assert_eq!(y1, 2);
    }

    #[test]
    fn test_unpack3() {
        let l = Box::new(list!(1, 2, 3));
        let (x, y, z) = l.unpack3().unwrap();
        assert_eq!(x, 1);
        assert_eq!(y, 2);
        assert_eq!(z, 3);
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
