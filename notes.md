### on quasiquote

```
<quasiquote> = <quasiquote 1>
<qq template 0> = exp
<quasiquote D> = `<qq template D>
<qq template D> =
  <simple datum>
  <list qq template D>
;;;  <vector qq template D>
  <unquote D>
<list qq template D> =
  ( <qq template or splice D>* )
  ( <qq template or splice D>+ . <qq template D>)
  '<qq template D>
  <quasiquote D+1>
;;;<vector qq template D> = #( <qq template or splice D>* )
<unquote D> = # 下面两个完全等价
  ,<qq template D-1>
  ( unquote <qq template D-1> )
<qq template or splice D> =
  <qq template D>
  <splice unquote D>
<splice unquote D> = # 下面两个完全等价
  ,@<qq template D-1>
  ( unquote-splicing <qq template D-1> )
```

```rust
pub enum Trampoline {
    Bounce(AstNode, Rc<RefCell<Environment>>, Continuation_k), // usually reflect the value(may need to evaluate, if not atom) to k
    QuasiBounce(AstNode, Rc<RefCell<Environment>>, Continuation_k), //
    Value(Value, Continuation_k), // reflect the value to k (not evaluate)
    Land(Value), // directly return
}
```

Trampoline CPS:
类似一个Thunk, 包裹(evaluated value, env, k 也就是接下来的continuation)
而continuation做的就是把evaluated value作为参数进行调用, 那么:

根据cont的类型不同, 要有不同的调用实现, 比如special form: define, 或者普通过程调用

每个k, 也就是continuation都要包裹接下来的continuation, 这样才能链式调用下去

对于quasiquote, 需要有一个标志(或类型?)告诉continuation该怎么调用, 因为拿到symbol以后不是eval, 而直接作为字面symbol,
遇到unquote才从env lookup, 并返回给上一层(也就是quasiquote的cont)

### on cons & list

目前Values包含两种list:
DatumList
Cons/Nil

DatumList专用于表达expression, 使用internals::List, 方便处理
Cons因为set-car!,set-cdr!的存在, 需要使用interior mutability, 但如果所有代码都使用这种方式会造成pattern match hell

缺点: 处理apply和eval时候需要DatumList和Cons的频繁转换, 有没有更好的方法?
