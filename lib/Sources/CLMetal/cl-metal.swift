
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
class CLMBuffer {
    public var buff:MTLBuffer
    public var count:Int
    public var stride:Int
    
    init (_ buff:MTLBuffer, _ count:Int, _ stride:Int) {
        self.buff   = buff
        self.count  = count
        self.stride = stride
    }
}
// Session = device, queue et al
//         = [inBuff1, outBuff2, ...] x N
class DeviceSession {
    var device:MTLDevice?
    var commandQueue:MTLCommandQueue?
    var library:MTLLibrary?
    var function:MTLFunction?

    public var readyToCompile:Bool = true
    public var readyToCompute:Bool = false

    public var readyToRun:Bool      = false
    public var readyToRetrieve:Bool = false

    public var buffs:[Int:CLMBuffer] = [:]
    
    init(_ device:MTLDevice, _ cmdQueue:MTLCommandQueue) {
        self.device = device
        self.commandQueue = cmdQueue
        self.readyToCompile = true
        self.readyToCompute = false        
    }

    public func append_buffer (_ nth:Int, _ inputBuffer:MTLBuffer,_ inputCount:Int,_ inputStride:Int) {
        self.buffs[nth] = CLMBuffer(inputBuffer, inputCount, inputStride)
    }
}

var globalDeviceSession:DeviceSession?
var compileError:String = ""
var globalCurrentDeviceIdx:Int = 0
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
    
    guard let newCommandQueue = newDevice.makeCommandQueue() else {
        return RetCode.CannotCreateCommandQueue.rawValue
    }

    // Updating Global Session:
    globalDeviceSession = DeviceSession(newDevice, newCommandQueue)
    globalCurrentDeviceIdx = device_index
    return RetCode.Success.rawValue
}

@available(macOS 10.15, *)
@_cdecl("clm_get_device")
public func clm_get_device(device_index:Int) -> UnsafeMutablePointer<CChar> {
    let devices = MTLCopyAllDevices()
    
    guard let defaultDevice = MTLCreateSystemDefaultDevice() else {
        return strdup("CannotCreateDevice")
    }
    
    if devices.count == 0 {
        return strdup("CannotCreateDevice (no gpus)")
    }
    
    if device_index >= devices.count {
        return strdup("CannotCreateDevice (invaild device_index)")
    }
    
    let tgtDevice = device_index < 0 ? defaultDevice : devices[device_index]
    
    var buffer = "= [device: " + String(device_index) + "]=========================================\n"
    buffer += " name                       : " + tgtDevice.name + "\n"
    buffer += " isLowPower                 : " + String(tgtDevice.isLowPower) + "\n"
    buffer += " hasUnifiedMemory           : " + String(tgtDevice.hasUnifiedMemory) + "\n"
    buffer += " currentAllocatedSize       : " + String(tgtDevice.currentAllocatedSize) + "\n"    
    buffer += " maxThreadgroupMemoryLength : " + String(tgtDevice.maxThreadgroupMemoryLength) + "\n"
    
    return strdup(buffer)
}

@_cdecl("clm_get_n_device")
public func clm_get_n_device() -> Int {
    return MTLCopyAllDevices().count
}

@_cdecl("clm_compile_kernel")
public func clm_compile_kernel(metalRaw: UnsafePointer<CChar>,
                               fnameRaw: UnsafePointer<CChar>) -> Int {
    let session = globalDeviceSession
    //guard session! != nil               else { return RetCode.NotReadyToCompile.rawValue }
    guard session!.readyToCompile       else { return RetCode.NotReadyToCompile.rawValue }
    guard let lDevice = session!.device else { return RetCode.NotReadyToCompile.rawValue }

    // CString <-> String
    let metal = String(cString: metalRaw)
    let fName = String(cString: fnameRaw)

    let options = MTLCompileOptions();
    options.fastMathEnabled = true
    //options.languageVersion = .version2_3

    do {
        let newLibrary = try lDevice.makeLibrary(source: metal, options: options)       
        guard let newFunction = newLibrary.makeFunction(name: fName) else { return RetCode.FailedToFindFunction.rawValue }

        session!.library = newLibrary
        session!.function = newFunction
    } catch {
        compileError = error.localizedDescription
        return RetCode.FailedToCompile.rawValue
    }

    // Here, after confiming compiling was succeed, two gloal variables are changed:
    session!.readyToCompute = true
    session!.readyToRun = false
    
    return RetCode.Success.rawValue
}
// Loads pre-compiled metal kernel file as a MTLLibrary
@_cdecl("clm_load_from_metallib")
public func clm_load_from_metallib(metallibPath: UnsafePointer<CChar>, fnameRaw: UnsafePointer<CChar>) -> Int {
    let session = globalDeviceSession
    guard session != nil                else { return RetCode.NotReadyToCompile.rawValue }
    guard session!.readyToCompile       else { return RetCode.NotReadyToCompile.rawValue }
    guard let lDevice = session!.device else { return RetCode.NotReadyToCompile.rawValue }

    let path  = String(cString: metallibPath)
    let fName = String(cString: fnameRaw)
    
    do {
        let newLibrary        = try lDevice.makeLibrary(filepath: path)
        guard let newFunction = newLibrary.makeFunction(name: fName) else { return RetCode.FailedToFindFunction.rawValue }
        // updating two global variables: library and function
        session!.library  = newLibrary
        session!.function = newFunction
    } catch {
        return RetCode.FailedToLoadLibrary.rawValue
    }
    // Here, after confiming compiling was succeed, two gloal variables are changed:
    session!.readyToCompute = true
    session!.readyToRun = false

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

@_cdecl("clm_reset_buffer")
public func clm_reset_buffer() -> Int {
    let session = globalDeviceSession
    guard session != nil else { return RetCode.NotReadyToCompile.rawValue }
    session!.buffs  = [:]
    session!.readyToCompile  = true
    session!.readyToCompute  = false
    session!.readyToRun      = false
    session!.readyToRetrieve = false

    //guard let newCommandQueue = session!.device!.makeCommandQueue() else {
    //    return RetCode.CannotCreateCommandQueue.rawValue
    //}

    //session!.commandQueue = newCommandQueue
    
    return 0//clm_set_device(device_index: globalCurrentDeviceIdx)
}

@available(macOS 11.0, *)
// Adds a new buffer to the current session
@_cdecl("clm_alloc")
public func clm_alloc(nth:Int, icount: Int, input: UnsafeRawPointer, iformat: Int) -> Int {
    let session = globalDeviceSession
//    guard session != nil                else { return RetCode.NotReadyToCompile.rawValue }
    guard session!.readyToCompute       else { return RetCode.NotReadyToCompile.rawValue }
    guard let lDevice = session!.device else { return RetCode.NotReadyToCompile.rawValue }

    let inputStride  = stride_of(fmt: iformat)
    
    guard inputStride  != 0 else { return RetCode.UnsupportedInputFormat.rawValue }    

    guard let newInputBuffer  = lDevice.makeBuffer(bytes: input, length: inputStride * icount, options: .storageModeShared) else {
        return RetCode.FailedToMakeInputBuffer.rawValue
    }

    session!.append_buffer(nth, newInputBuffer, icount, inputStride)
    session!.readyToRun      = true
    session!.readyToRetrieve = false
    return RetCode.Success.rawValue
}


@_cdecl("clm_run")
public func clm_run(global_width:Int, global_height:Int, global_depth:Int, local_width:Int, local_height:Int, local_depth:Int) -> Int {
    // Execute the configured compute task, waiting for completion
    let session = globalDeviceSession
    guard session != nil else { return RetCode.NotReadyToCompile.rawValue }
    
    guard session!.readyToRun else { return RetCode.NotReadyToRun.rawValue }
    guard let lDevice       = session!.device       else { return RetCode.NotReadyToRun.rawValue }
    guard let lFunction     = session!.function     else { return RetCode.NotReadyToRun.rawValue }
    guard let lCommandQueue = session!.commandQueue else { return RetCode.NotReadyToRun.rawValue }
    guard let commandBuffer = lCommandQueue.makeCommandBuffer()         else { return RetCode.CannotCreateCommandBuffer.rawValue }
    guard let encoder       = commandBuffer.makeComputeCommandEncoder() else { return RetCode.CannotCreateCommandEncoder.rawValue }
    //guard let buffs         = session.buffs else { return RetCode.NotReadyToRun.rawValue }
    
    do {
        let pipelineState = try lDevice.makeComputePipelineState(function:lFunction)
        encoder.setComputePipelineState(pipelineState);

        for (index, buff) in session!.buffs {
            encoder.setBuffer(buff.buff, offset: 0, index: index)
        }
        
        let numThreadgroups       = MTLSize(width: global_width, height: global_height, depth: global_depth)
        let threadsPerThreadgroup = MTLSize(width: local_width, height: local_height, depth: local_depth)
        
        encoder.dispatchThreadgroups(numThreadgroups, threadsPerThreadgroup: threadsPerThreadgroup)
        encoder.endEncoding()
        
        commandBuffer.commit()
        commandBuffer.waitUntilCompleted()
    } catch {
        return RetCode.CannotCreatePipelineState.rawValue
    }

    session!.readyToRetrieve = true
    return RetCode.Success.rawValue
}

@_cdecl("clm_retrieve")
public func clm_retrieve(nth:Int, output: UnsafeMutableRawPointer) -> Int {
    let session = globalDeviceSession
    guard session != nil           else { return RetCode.NotReadyToCompile.rawValue }
    guard session!.readyToRetrieve else { return RetCode.NotReadyToRetrieve.rawValue }

    let target = session!.buffs[nth]
    output.copyMemory(from: target!.buff.contents(), byteCount: target!.count * target!.stride)
    return RetCode.Success.rawValue
}

@_cdecl("clm_get_compile_error")
public func clm_get_compile_error() -> UnsafeMutablePointer<CChar> {
    return strdup(compileError)
}

