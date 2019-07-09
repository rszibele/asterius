# JavaScript FFI

## JSFFI Types

These types can be used as arguments or return values in either JSFFI imports or
exports:

* `Ptr`
* `FunPtr`
* `StablePtr`
* `Bool`
* `Int`
* `Word`
* `Char`
* `Float`
* `Double`

In addition, these types in `Asterius.Types` are supported; they represent
opaque JavaScript values:

* `JSVal`
* `JSArrayBuffer`
* `JSString`
* `JSArray`
* `JSObject`
* `JSFunction`

All `JS*` types are newtypes of `JSVal`, and there's currently no runtime
typechecking to enforce e.g. an actual array is passed via `JSArray`.

The result can be wrapped in `IO` or not. The usual caveats of C FFI regarding
`IO` also apply here.

It's worth noting that when writing JSFFI imports/exports, no type synonyms or
newtypes can be used for JSFFI types yet. The restriction rises from the hacky
way we implement JSFFI: processing the `HsSyn` AST at the parser phase, rather
than renamer/typechecker, therefore we only recognize types by strings at the
moment. This restriction will be solved in the long run; meanwhile, the common
practice of making newtype wrappers for distinction of various JavaScript types
is still possible via separating the actual interface with the underlying
import, and using `coerce` or manual wrapping/unwrapping.

When the `MagicHash`/`UnliftedFFITypes` extensions are on, these unlifted types
can also be used as JSFFI types. However, when used as return types, the result
can't be wrapped in `IO`.

* `StablePtr# a`
* `Addr#`
* `ByteArray#`
* `MutableByteArray# s`
* `Char#`
* `Int#`
* `Word#`
* `Float#`
* `Double#`

## JSFFI Imports

```haskell
import Asterius.Types

foreign import javascript "new Date()" current_time :: IO JSVal

foreign import javascript interruptible "fetch(${1})" fetch :: JSVal -> IO JSVal
```

The source text of `foreign import javascript` should be a single valid
JavaScript expression, using `${n}` to refer to the nth argument (starting from
`1`). It's possible to use IIFE(Immediately Invoked Function Expression) to
define local variables and use advanced control-flow features here, but complex
logic should really appear in standalone scripts.

In the source text, all properties of the global object can be accessed, and
additionally, the `__asterius_jsffi` identifier can be used to refer to the
current asterius instance. Say that we want to bind a JavaScript library `X` to
Haskell without polluting the global namespace, we can simply initialize the
asterius instance, assign `X` as a property of the instance object, then in the
source text, use `__asterius_jsffi.X` to refer to it.

Unlike C FFI which defaults to the `safe` safety level, when the safety level is
omitted in a `foreign import javascript` declaration, the default level is
`unsafe`. Asterius currently treats `unsafe` imports as synchronous imports, and
`safe`/`interruptible` imports as asynchronous imports.

The synchronous imports are lightweight; each call to a synchronous import maps
to a call to a WebAssembly import corresponding to the function generated using
the source text. The JavaScript code is assumed to return a value of the correct
type synchronously; failing to do so, by either returning ill-typed values or
throwing errors, will result in unrecoverable runtime errors.

The asynchronous imports are based on JavaScript `Promise`s. The JavaScript code
is assumed to return a `Promise` which resolves to the correct result type; it's
wrapped in a `Promise.resolve` call in the generated function, so normal
synchronous code works too. Upon an asynchronous call, the Haskell execution is
suspended, and resumed when the `Promise` is resolved or rejected. If the
`Promise` is rejected, the error is wrapped in a `JSException` (defined in
`Asterius.Types`) and thrown in the Haskell calling thread.

## JSFFI Exports

```haskell
foreign export javascript "mult_hs" (*) :: Int -> Int -> Int
```

In a Haskell module, one can specify the exported function name (must be
globally unique), along with its Haskell identifier and type. One can specify
`ahc-link --export-function=mult_hs` to make the linker include the relevant
bits in final WebAssembly binary, and export `mult_hs` as a regular WebAssembly
export function. After calling `hs_init` to initialize the runtime, one can call
`mult_hs` just like a regular JavaScript async function:

```javascript
i.exports.hs_init();
const r = await i.exports.mult_hs(6, 7);
```

It's also possible to use `JS*` types in JSFFI exports. In that case, when
calling the exported function in `i.exports`, we can directly pass the
JavaScript values as arguments, and receive them as results, without having to
care about the bijection between `JSVal` ids and actual values.

## Marshaling between Haskell and JavaScript types

The `Asterius.Types`/`Asterius.ByteString` modules provide some high-level
functions for converting between Haskell and JavaScript types:

```haskell
fromJSString :: JSString -> [Char]
toJSString :: [Char] -> JSString

fromJSArray :: JSArray -> [JSVal]
toJSArray :: [JSVal] -> JSArray

byteStringFromJSArrayBuffer :: JSArrayBuffer -> ByteString
byteStringToJSArrayBuffer :: ByteString -> JSArrayBuffer
```

It's possible to define them just by using the basic JSFFI mechanism, but those
functions are backed by special runtime interfaces which makes them a lot
faster. Most notably, the `fromJS*` functions directly traverse the JavaScript
value and build a fully-evaluated Haskell data structure on the heap in one
pass.

## Implementation

Content starting from here is reserved for the brave souls who seek to get their
hands dirty with the internals of asterius. (TODO: delete this line)

This subsection presents a high-level overview on the implementation of JSFFI, based on the information flow from syntactic sugar to generated WebAssembly/JavaScript code. It's not a required reading for *users* of the JSFFI feature.

### Syntactic sugar

As documented in previous sections, one can write `foreign import javascript` or `foreign export javascript` clauses in a `.hs` module. How are they processed? The logic resides in `Asterius.JSFFI`.

First, there is `addFFIProcessor`, which given a `Compiler` (defined in `ghc-toolkit`), returns a new `Compiler` and a callback to fetch a stub module. The details of `Compiler`'s implementation are not relevant here, just think of it as an abstraction layer to fetch/modify GHC IRs without dealing with all the details of GHC API.

`addFFIProcessor` adds one functionality to the input `Compiler`: rewrite parsed Haskell AST and handle the `foreign import javascript`/`foreign export javascript` syntactic sugar. After rewriting, JavaScript FFI is really turned into C FFI, so type-checking/code generation proceeds as normal.

After the parsed AST is processed, a "stub module" of type `AsteriusModule` is generated and can be later fetched given an `AsteriusModuleSymbol`. It contains JSFFI related information of type `FFIMarshalState`. Both `AsteriusModule` and `FFIMarshalState` types has `Semigroup` instance so they can be combined later at link-time.

### TODO

### Adding a JSFFI basic type

Look at the following places:

* `Asterius.JSFFI` module. All JavaScript reference types are uniformly handled as `FFI_JSVAL`, while value types are treated as `FFI_VAL`. Assuming we are adding a value type. Add logic to:
    * `marshalToFFIValueType`: Recognize the value type in parsed AST, and translate to `FFI_VAL`
* `Asterius.Builtins` module. Add the corresponding `rts_mkXX`/`rts_getXX` builtin functions. They are required for stub functions of `foreign export javascript`.
