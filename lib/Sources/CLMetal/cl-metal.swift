
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
    case FailedToFindFunction       = -5
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
// Following APIs are Strongly inspired from: https://github.com/baldand/py-metal-compute/blob/main/src/metalcompute.swift

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

@_cdecl("clm_get_n_device")
public func clm_get_n_device() -> Int {
    return MTLCopyAllDevices().count
}

// Initializes all buffers, all global variables as it was ignoring device/queue
@_cdecl("clm_init_device")
public func clm_init_device() -> Int {
    inputBuffer = nil
    outputBuffer = nil
    function = nil
    library = nil
    device = nil
    readyToCompile = false
    readyToCompute = false
    readyToRun = false
    readyToRetrieve = false
    return RetCode.Success.rawValue
}

@_cdecl("clm_compile_kernel")
public func clm_compile_kernel(metalRaw: UnsafePointer<CChar>,
                               fnameRaw: UnsafePointer<CChar>) -> Int {
    guard readyToCompile       else { return RetCode.NotReadyToCompile.rawValue }
    guard let lDevice = device else { return RetCode.NotReadyToCompile.rawValue }

    // CString <-> String
    let metal = String(cString: metalRaw)
    let fName = String(cString: fnameRaw)

    let options = MTLCompileOptions();
    options.fastMathEnabled = true
    //options.languageVersion = .version2_3

    do {
        let newLibrary = try lDevice.makeLibrary(source: metal, options: options)       
        guard let newFunction = newLibrary.makeFunction(name: fName) else { return RetCode.FailedToFindFunction.rawValue }

        // updating two global variables: library and function
        library = newLibrary
        function = newFunction
    } catch {
        compileError = error.localizedDescription
        return RetCode.FailedToCompile.rawValue
    }

    // Here, after confiming compiling was succeed, two gloal variables are changed:
    readyToCompute = true
    readyToRun = false
    
    return RetCode.Success.rawValue
}

// Loads pre-compiled metal kernel file as a MTLLibrary
@_cdecl("clm_load_kernel")
public func clm_load_kernel() -> Int {
    return 0
}
