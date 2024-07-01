module FFalse.RuntimeStack

open System.Collections.Generic

type StackValue =
    | NumberValue of int 
    | ReferenceValue of string
    | HandleValue of int
    
let popAny (stack : Stack<StackValue>) : StackValue =
    let mutable value : StackValue = NumberValue(0)
    
    if stack.TryPop(&value) then
        value
    else
        failwith "Failed to pop from runtime stack."
    
let popNumber stack =
    match popAny stack with
    | NumberValue v -> v
    | _ -> failwith "Failed to pop number value from runtime stack."
    
let popTwoNumbers stack = (popNumber stack, popNumber stack)

let popThreeNumbers stack = (popNumber stack, popNumber stack, popNumber stack)

let pushNumber (stack : Stack<StackValue>) number = stack.Push(NumberValue(number))

let pushReference (stack : Stack<StackValue>) name = stack.Push(ReferenceValue(name))

let pushHandle (stack : Stack<StackValue>) handle = stack.Push(HandleValue(handle))

let popReference (stack : Stack<StackValue>) =
    match popAny stack with
    | ReferenceValue v -> v
    | _ -> failwith "Failed to pop reference value from runtime stack."
    
let popHandle (stack : Stack<StackValue>) =
    match popAny stack with
    | HandleValue h -> h
    | _ -> failwith "Failed to pop handle value from runtime stack."

let pushAny (stack : Stack<StackValue>) item = stack.Push(item)

let peekAny (stack : Stack<StackValue>) index = stack.ToArray()[index]

