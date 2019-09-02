quasiquote:
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

重新定义pair/list

sol.2
```rust
enum Value {
    Nil,
    Cons(Rc<RefCell<Value>>, Rc<RefCell<Value>>,),
    //...
}
```
some primitives:
pair?: Cons
null?: Nil
list?: Cons && Cons.last == Nil


```rust
pub enum Trampoline {
    Bounce(AstNode, Rc<RefCell<Environment>>, Continuation_k), // usually reflect the value(may need to evaluate, if not atom) to k
    QuasiBounce(AstNode, Rc<RefCell<Environment>>, Continuation_k), //
    Run(Value, Continuation_k), // reflect the value to k (not evaluate)
    Land(Value), // directly return
}
```
Bounce CPS:
Bounce类似一个Thunk, 包裹(evaluated value, env, k 也就是接下来的continuation)
而continuation做的就是把evaluated value作为参数进行调用, 那么:

根据cont的类型不同, 要有不同的调用实现, 比如special form: define, 或者普通过程调用

每个k, 也就是continuation都要包裹接下来的continuation, 这样才能链式调用下去

对于quasiquote, 需要有一个标志(或类型?)告诉continuation该怎么调用, 因为拿到symbol以后不是eval, 而直接作为字面symbol,
遇到unquote才从env lookup, 并返回给上一层(也就是quasiquote的cont)

----

process_top 和 process 的区别在于:
process_top的处理对象是list-of-sexp, process为sexp, 从接口上可以直接区分为&List<R.Value>, &R.Value, 当然Value也可能是个复杂sexp
process_top的每个sexp的结果应该是被discard(除非repl), 也就是说process_top有一个top continuation
process则要把sexp整体处理完, 返回Value