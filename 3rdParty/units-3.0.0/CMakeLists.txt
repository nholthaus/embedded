if(NOT TARGET units)
    if(POLICY CMP0135)
        cmake_policy(SET CMP0135 NEW)
        set(CMAKE_POLICY_DEFAULT_CMP0135 NEW)
    endif()

    if(POLICY CMP0077)
        cmake_policy(SET CMP0077 NEW)
        set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
    endif()

    SET(UNITS_BUILD_TESTS OFF)

    include(FetchContent)
    FetchContent_Declare(
            units
            URL ${CMAKE_CURRENT_SOURCE_DIR}/units-3.x.zip
            URL_HASH MD5=7a55da6d91ccb77ea7bff4b445bec88a
    )
    FetchContent_MakeAvailable(units)
endif()