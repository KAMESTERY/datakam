project(cpp)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pthread")

#set(CMAKE_GENERATOR_PLATFORM x64)
set(CMAKE_BUILD_TYPE Release)

# Enable ExternalProject CMake module
include(ExternalProject)

# Set default ExternalProject root directory
set_directory_properties(PROPERTIES EP_PREFIX ${CMAKE_BINARY_DIR}/Rust)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CARGO_CMD cargo build)
else ()
    set(CARGO_CMD cargo build --release)
endif ()

# Add rusty_common as a CMake target
ExternalProject_Add(
        datakam_rust_workspace
        DOWNLOAD_COMMAND ""
        CONFIGURE_COMMAND ""
        BUILD_COMMAND cargo build
        COMMAND ${CARGO_CMD}
        TEST_COMMAND cargo test
        BINARY_DIR "${PROJECT_SOURCE_DIR}"
        INSTALL_COMMAND ""
        LOG_BUILD ON
)

#### Find VCPKG Dependencies
find_package(aws-cpp-sdk-core CONFIG REQUIRED)
#target_link_libraries(main PRIVATE aws-cpp-sdk-core)

find_package(aws-cpp-sdk-dynamodb CONFIG REQUIRED)
#target_link_libraries(main PRIVATE aws-cpp-sdk-dynamodb)

#find_package(aws-cpp-sdk-kinesis CONFIG REQUIRED)
#target_link_libraries(main PRIVATE aws-cpp-sdk-kinesis)

#find_package(aws-cpp-sdk-s3 CONFIG REQUIRED)
#target_link_libraries(main PRIVATE aws-cpp-sdk-s3)

### Build Targets

set(DALCPP_SRC ${CMAKE_CURRENT_SOURCE_DIR}/dalcpp)
add_executable(dalcpp-exe ${DALCPP_SRC}/main.cpp)

### Link VCPKG Libraries
target_link_libraries(dalcpp-exe PRIVATE aws-cpp-sdk-dynamodb)

### Create dependency of DataKam Rust Workspace on datakam_rust_workspace
add_dependencies(dalcpp-exe datakam_rust_workspace)
