<div align="center" style="display:grid;place-items:center;">
<p>
    <a href="https://github.com/skycloudd/foxglove" target="_blank"><img width="80" src="img/logo.png" alt="Foxglove logo"></a>
</p>
<h1>Foxglove Programming Language</h1>

<h2>A simple, modern programming language with clean syntax and amazing error messages (wip)</h2>
</div>

***

## Code examples

**Fibonacci sequence**

```dart
fn fib|n: int|> int {
    if n == 0
        return n;
    else if n == 1
        return n;
    else
        return fib(n - 1) + fib(n - 2);
}

fn main ||> int {
    var n = 10;

    for i in 0..10
        fib(i);

    return 0;
}
```

**Sort function**

```dart
fn sort |a: [int], n: int|>
    for i in 1..n
        for j in 1..n
            if a[i] < a[j] {
                var t = a[i];

                a[i] = a[j];
                a[j] = t;
            }

fn main ||> int {
    var a = [5, 3, 9, 1, 2, 4, 6, 10, 7, 8];
    var n = 10;

    sort(a, n);

    return 0;
}
```
