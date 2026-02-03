# 🍋 Lychee

**Lychee** is a simple, dynamically typed, functional programming language.  
An experimental language designed to explore language design and interpreter implementation.

---

## 📝 Quick Syntax

### Statements

All statements end with a semicolon:

```lychee
print(x, y);
```

### Numbers
Numbers must start with a digit:

```lychee
x = 420;
y = 0.5;
```

### Strings
Strings are wrapped in double quotes:

```lychee
a = "hello world";
b = "this is a string";
```

### Objects

Objects are wrapped in curly braces:

```lychee
obj = {
  0     = "value",
  key   = "value",
  "key" = "value",
};
```

### Functions
Anonymous by default:

```lychee
() { print("hello world"); }; # does nothing
```

Assign to a variable and invoke:
```lychee
f = () { print("hello world"); };
f(); # prints "hello world"
```

Or invoke immediately:
```lychee
() { print("hello world"); } (); # prints "hello world"
```

### Arithmetic
Use built-in functions:

```lychee
add(420, 69);
sub(3.14, 1.414);
```

### Control Flow
Branching is handled via built-in functions:

```lychee
if(
  lt(x, y), 
  () { print("x is less than y"); },
  () { print("x is NOT less than y"); }
);
```