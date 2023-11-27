
/*
 cl-metal.swift
 Swift Bindings to be compiled into .dylib and interoperated with Common Lisp via CFFI.
 */

import MetalKit

enum RetCode: Int {
    case Success                  = 0
    case CannotCreateDevice       = -1
    case CannotCreateCommandQueue = -2
    // Other retcode follows...
}

enum Dtype: Int {
    case Unknown = -1
    case U8      = 0
    case I8      = 1
    case U16     = 2
    case I16     = 3
    case U32     = 4
    case I32     = 5
    case U64     = 6
    case I64     = 7
    case F16     = 8
    case F32     = 9
    case F64     = 10    
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Inspired from: https://github.com/baldand/py-metal-compute/blob/main/src/metalcompute.swift, (c) Andrew Baldwin 2021
//
// Can only be used for single kernel 
// sequential synchronous (non-pipelined) execution
// Data must be copied in/out (copy overhead)
//

var device:MTLDevice?
var commandQueue:MTLCommandQueue?

var library:MTLLibrary?
var function:MTLFunction?
var inputBuffer:MTLBuffer?

var inputCount:Int = 0
var inputStride:Int = 0

var outputBuffer:MTLBuffer?
var outputCount:Int = 0
var outputStride:Int = 0

var readyToCompile = false
var readyToCompute = false

var readyToRun = false
var readyToRetrieve = false

var compileError:String = ""
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// test global variables in swift can be handled even when via cffi?

@_cdecl("clm_sw_init") public func clm_sw_init(device_index:Int64) -> Int {
    let device_index = Int(device_index)
    //let devices = MTLCopyAllDevices()
    inputCount += device_index
    return inputCount
}
