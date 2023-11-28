import XCTest
@testable import CLMetal

final class CLMetalTests: XCTestCase {
    func test_clm_set_device () {
        let ret = clm_set_device(device_index: 0)
        XCTAssertEqual(ret, 0)
    }
    
    static var allTests = [
      ("test_clm_set_device", test_clm_set_device)
    ]
}
