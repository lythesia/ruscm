use std::rc::Rc;

//use nom::Err;
//use nom::error::{VerboseError, convert_error};

//fn test_binary(i: &str) {
//    let p = binary::<VerboseError<&str>>();
//    match p(i) {
//        Ok((_, v)) => println!("ok: {}",  v),
//        Err(Err::Error(e)) | Err(Err::Failure(e)) => println!("err:\n{}", convert_error(i, e)),
//        _ => {},
//    }
//}
//
//fn test_decimal(i: &str) {
//    let p = decimal::<VerboseError<&str>>();
//    match p(i) {
//        Ok((_, v)) => println!("ok: {}",  v),
//        Err(Err::Error(e)) | Err(Err::Failure(e)) => println!("err:\n{}", convert_error(i, e)),
//        _ => {},
//    }
//}

fn main() {
    //    let l = Rc::new(List::Nil);
    //    let ll = Rc::new(List::Cons(
    //        Rc::new(1),
    //        Rc::new(List::Cons(
    //            Rc::new(2),
    //            l.clone()
    //        ))
    //    ));
    //    println!("{}", ll);
    //    println!("{:?}", ll);
    //
    //    println!("{}", Keyword::QUASIQUOTE);
    //    println!("{}", Keyword::UNQUOTESPLICING);
    //    println!("{}", Keyword::LETSTAR);
    //    println!("{}", Keyword::LETREC);
    //    println!("{}", Keyword::IF);
    //    println!("{}", Keyword::DO);

    //    test_binary("#b011");
    //    test_binary("#b2");
    //    test_binary("#b-011");
    //    test_binary("#b-012");

    //    test_decimal("#d12");
    //    test_decimal("#d-12");
    //    test_decimal("+12");
    //    test_decimal("#d+12");
}
