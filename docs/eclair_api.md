# Eclair API

Eclair is meant to be embedded and controlled from inside another host language.
It exposes a tiny API that allows you to do all the high level operations in
Datalog:

1. Initializing the Eclair runtime,
2. Serializing data from host language to Eclair,
3. Running the Eclair program,
4. Deserializing results from Eclair back to the host language,
5. Cleanup / teardown of the Eclair runtime.

The types of the functions are written down in C just as an indication, it's
also completely possible to call these functions in JS if Eclair code is
compiled to WebAssembly for example.

Eclair exposes a lot of internal details in it's API, giving a developer a lot
of control over how they want to use Eclair. A higher level API that
automatically takes care of these internal details can be written on top of
this, a good example of this is
[eclair-haskell](https://github.com/luc-tielen/eclair-haskell).

## Initializing the Eclair runtime

Before you call any of the other functions in Eclair, it's important that you
first initialize the runtime. This can be done by invoking the following
function:

```c
struct program* eclair_program_init();
```

The function returns a pointer (a "handle" to the Eclair program). The rest of
the API always requires this pointer to be passed in as an argument.

## Serializing data from host language to Eclair

Now that the runtime is initialized, you can start serializing data from your
host language into Eclair. The language provides a few functions to add one or
more facts:

```c
void eclair_add_fact(struct program* program,
                     uint32_t fact_type,
                     uint32_t* fact_data);
void eclair_add_facts(struct program* program,
                      uint32_t fact_type,
                      uint32_t* fact_data,
                      uint32_t fact_count);
```

One thing might be immediately obvious: Eclair only accepts an array of
`uint32_t` data. This is done for two reasons:

1. Eclair only uses `uint32_t` internally to represent data (for both
   performance and simplicity),
2. It's better for performance when communicating data back and forth, since we
   only need to do one function call into Eclair with a single big array.

The fact array should be filled with data of a fact directly followed by
another. For example, given the following Eclair program:

```
@def edge(u32, u32).
```

If you wanted to push `edge(1, 2)` and `edge(2, 3)` into Eclair, you would need
to create an array of this shape:

```c
uint32_t fact_data[] = { 1, 2,
                         3, 4
                       };
```

If you have facts that contain data that is not a `uint32_t` (e.g. a string
value), you will need to first encode the string in Eclair. This can be done
with the following function that takes both the length of the string and a
pointer to a byte-array (Eclair assumes UTF8 encoding).

```c
uint32_t eclair_encode_string(struct program*,
                              uint32_t string_length,
                              const char* string_data);
```

As you can see, this gives you back a `uint32_t` value, that you can insert at
the right location into the fact array.

One final thing to point out about `eclair_add_fact` and `eclair_add_facts` is
the `fact_type` argument. In order to get the value for this argument, you will
need to call `eclair_encode_string` with the name of the relation, to get the
corresponding fact type value back.

## Running an Eclair program

Once you have serialized all your input facts into Eclair, you can now run your
program. This is done by invoking the following function:

```c
void eclair_program_run(struct program*);
```

This will run the Eclair Datalog program from start to end.

## Deserializing data from Eclair back to the host language

Deserializing the data back into the host language is similar to serialization
of data. Eclair also here works with a `uint32_t` array of data. For
deserialization however we need a few more helper functions to process the
returned array of data. The functions for deserialization are as follows:

```c
uint32_t eclair_fact_count(struct program*, uint32_t fact_type);
uint32_t* eclair_get_facts(struct program*, uint32_t fact_type);
void eclair_free_buffer(uint32_t* data);
```

`eclair_fact_count` returns the total amount of facts returned, while
`eclair_get_facts` returns the actual byte array. `eclair_free_buffer` is needed
since the result `uint32_t`-array is dynamically allocated and needs to be
manually freed by the host language after it is no longer needed.

These 3 functions allow you to write the following code for processing the
results in the host language:

```c
uint32_t fact_count = eclair_fact_count(program, fact_type);
uint32_t* data = eclair_get_facts(program, fact_Type);

for (uint32_t i = 0; i < fact_count; ++i) {
   // Process the data array here.
}

eclair_free_buffer(data);
```

If you have string values inside your fact data, you will also manually need to
call `eclair_decode_string` to go from the `uint32_t` value back to the
underlying string byte-array. The signature of this function is as follows:

```
const char* eclair_decode_string(struct program*, uint32_t string_index);
```

Note that this memory does _not_ need to be freed, this will happen
automatically when the Eclair program is shutdown (see next section).

## Cleanup of the Eclair runtime

Once you finished using Eclair, you need to shutdown the runtime. This is a
manual operation since Eclair has no garbage collector and performs manual
memory management.

Eclair provides a single function for this:

```c
void eclair_program_destroy(struct program*);
```

Calling this function will free up any memory still in use by Eclair, so that
the system can use it for other purposes. After this point it's no longer valid
/ safe to call other functions of the API.
