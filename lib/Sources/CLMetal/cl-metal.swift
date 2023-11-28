
/*
 cl-metal.swift
 Swift Bindings to be compiled into .dylib and interoperated with Common Lisp via CFFI.
 Following codes are strongly inspired from: https://github.com/baldand/py-metal-compute/blob/main/src/metalcompute.swift
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
    case CannotCreateCommandBuffer  = -9
    case CannotCreateCommandEncoder = -10
    case CannotCreatePipelineState  = -11
    case IncorrectOutputCount       = -12
    case NotReadyToRetrieve         = -13
    case UnsupportedInputFormat     = -14
    case UnsupportedOutputFormat    = -15

    case FailedToLoadLibrary        = -16
    case NotReadyToRun              = -17

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
@_cdecl("clm_load_from_metallib")
public func clm_load_from_metallib(metallibPath: UnsafePointer<CChar>, fnameRaw: UnsafePointer<CChar>) -> Int {

    guard readyToCompile       else { return RetCode.NotReadyToCompile.rawValue }
    guard let lDevice = device else { return RetCode.NotReadyToCompile.rawValue }

    let path  = String(cString: metallibPath)
    let fName = String(cString: fnameRaw)
    
    do {
        let newLibrary = try lDevice.makeLibrary(filepath: path)
        guard let newFunction = newLibrary.makeFunction(name: fName) else { return RetCode.FailedToFindFunction.rawValue }

        // updating two global variables: library and function
        library = newLibrary
        function = newFunction
    } catch {
        return RetCode.FailedToLoadLibrary.rawValue
    }
    
    // Here, after confiming compiling was succeed, two gloal variables are changed:
    readyToCompute = true
    readyToRun = false

    return RetCode.Success.rawValue
}

@available(macOS 11.0, *)
func stride_of(fmt: Int) -> Int {
    switch fmt {
    case 10:
        return MemoryLayout<Float64>.stride
    case 9:
        return MemoryLayout<Float32>.stride
    case 8:
        return MemoryLayout<Float16>.stride
    case 0:
        return MemoryLayout<UInt8>.stride
    case 1:
        return MemoryLayout<Int8>.stride
    case 2:
        return MemoryLayout<UInt16>.stride
    case 3:
        return MemoryLayout<Int16>.stride
    case 4:
        return MemoryLayout<UInt32>.stride
    case 5:
        return MemoryLayout<Int32>.stride
    case 6:
        return MemoryLayout<UInt64>.stride
    case 7:
        return MemoryLayout<Int64>.stride
    default:
        return 0    
    }
}

@available(macOS 11.0, *)
@_cdecl("clm_alloc")
public func clm_alloc(icount: Int, input: UnsafeRawPointer, iformat: Int, ocount: Int, oformat: Int) -> Int {
    // Allocate input/output buffers for run
    // Separating this step allows python global lock to be released for the actual run which does not need any python objects
    guard readyToCompute       else { return RetCode.NotReadyToCompute.rawValue }
    guard let lDevice = device else { return RetCode.NotReadyToCompute.rawValue }

    inputStride  = stride_of(fmt: iformat)
    outputStride = stride_of(fmt: oformat)
    
    guard inputStride  != 0 else { return RetCode.UnsupportedInputFormat.rawValue }    
    guard outputStride != 0 else { return RetCode.UnsupportedOutputFormat.rawValue }

    guard let newInputBuffer  = lDevice.makeBuffer(bytes: input, length: inputStride * icount, options: .storageModeShared) else {
        return RetCode.FailedToMakeInputBuffer.rawValue
    }
    guard let newOutputBuffer = lDevice.makeBuffer(length: outputStride * ocount, options: .storageModeShared) else {
        return RetCode.FailedToMakeOutputBuffer.rawValue
    }

    // updating global variables
    inputBuffer     = newInputBuffer
    outputBuffer    = newOutputBuffer
    inputCount      = icount
    outputCount     = ocount
    readyToRun      = true
    readyToRetrieve = false

    return RetCode.Success.rawValue
}


@_cdecl("clm_run")
public func clm_run(kcount:Int) -> Int {
    // Execute the configured compute task, waiting for completion
    guard readyToRun else { return RetCode.NotReadyToRun.rawValue }
    guard let lDevice = device else { return RetCode.NotReadyToRun.rawValue }
    guard let lFunction = function else { return RetCode.NotReadyToRun.rawValue }
    guard let lCommandQueue = commandQueue else { return RetCode.NotReadyToRun.rawValue }
    guard let lInputBuffer = inputBuffer else { return RetCode.NotReadyToRun.rawValue }
    guard let lOutputBuffer = outputBuffer else { return RetCode.NotReadyToRun.rawValue }
    guard let commandBuffer = lCommandQueue.makeCommandBuffer() else { return RetCode.CannotCreateCommandBuffer.rawValue }
    guard let encoder = commandBuffer.makeComputeCommandEncoder() else { return RetCode.CannotCreateCommandEncoder.rawValue }

    do {
        let pipelineState = try lDevice.makeComputePipelineState(function:lFunction)
        encoder.setComputePipelineState(pipelineState);
        encoder.setBuffer(lInputBuffer, offset: 0, index: 0)
        encoder.setBuffer(lOutputBuffer, offset: 0, index: 1)
        let w = pipelineState.threadExecutionWidth
        let h = pipelineState.maxTotalThreadsPerThreadgroup / w
        let numThreadgroups = MTLSize(width: (kcount+(w*h-1))/(w*h), height: 1, depth: 1)
        let threadsPerThreadgroup = MTLSize(width: w*h, height: 1, depth: 1)
        encoder.dispatchThreadgroups(numThreadgroups, threadsPerThreadgroup: threadsPerThreadgroup)
        encoder.endEncoding()
        commandBuffer.commit()
        commandBuffer.waitUntilCompleted()
    } catch {
        return RetCode.CannotCreatePipelineState.rawValue
    }

    readyToRetrieve = true
    return RetCode.Success.rawValue
}

@_cdecl("clm_retrieve")
public func clm_retrieve(ocount:Int, output: UnsafeMutableRawPointer) -> Int {
    guard readyToRetrieve else { return RetCode.NotReadyToRetrieve.rawValue }
    guard ocount == outputCount else { return RetCode.IncorrectOutputCount.rawValue }
    guard let lOutputBuffer = outputBuffer else { return RetCode.NotReadyToRetrieve.rawValue }
    
    output.copyMemory(from: lOutputBuffer.contents(), byteCount: outputCount * outputStride)
    return RetCode.Success.rawValue
}

@_cdecl("clm_get_compile_error")
public func clm_get_compile_error() -> UnsafeMutablePointer<CChar> {
    return strdup(compileError)
}

