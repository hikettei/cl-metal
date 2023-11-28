
/*
 cl-metal.swift
 Swift Bindings to be compiled into .dylib and interoperated with Common Lisp via CFFI.
 */
import Metal
import MetalPerformanceShaders
import Accelerate
import Foundation
import MetalKit

enum RetCode: Int {
    case Success                    = 0
    case CannotCreateDevice         = -1
    case CannotCreateCommandQueue   = -2
    case NotReadyToCompile          = -3
    case FailedToCompile            = -4
    case FailedToFindFunctione      = -5
    case NotReadyToCompute          = -6
    case FailedToMakeInputBuffer    = -7
    case FailedToMakeOutputBuffer   = -8
    //case NotReadyToRun              = -8
    case CannotCreateCommandBuffer  = -9
    case CannotCreateCommandEncoder = -10
    case CannotCreatePipelineState  = -11
    case IncorrectOutputCount       = -12
    case NotReadyToRetrieve         = -13
    case UnsupportedInputFormat     = -14
    case UnsupportedOutputFormat    = -15

    case DeviceNotFound         = -1000
    case KernelNotFound         = -1001
    case FunctionNotFound       = -1002
    case CouldNotMakeBuffer     = -1003
    case BufferNotFound         = -1004
    case RunNotFound            = -1005
    case DeviceBuffersAllocated = -1006
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

@available(macOS 10.13, *)
var device       = MTLCreateSystemDefaultDevice()!,
    commandQueue = device.makeCommandQueue()!

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

@_cdecl("clm_set_device")
public func clm_set_device(device_index:Int) -> Int {
    let devices = MTLCopyAllDevices()
    
    guard let defaultDevice = MTLCreateSystemDefaultDevice() else {
        return RetCode.CannotCreateDevice.rawValue
    }
    
    if devices.count == 0 {
        return RetCode.CannotCreateDevice.rawValue
    }
    
    if device_index >= devices.count {
        return RetCode.CannotCreateDevice.rawValue
    }
    
    let newDevice = device_index < 0 ? defaultDevice : devices[device_index] 

    // Updating Global Variable: device
    device = newDevice
    
    guard let newCommandQueue = newDevice.makeCommandQueue() else {
        return RetCode.CannotCreateCommandQueue.rawValue
    }

    // Updating Global Variables:    
    commandQueue = newCommandQueue
    
    readyToCompile = true
    readyToCompute = false

    return RetCode.Success.rawValue
}

