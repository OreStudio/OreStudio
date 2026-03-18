vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO getml/sqlgen
    REF 7e5e6d11bf31ec44bbc027b9c199917e2827225c
    SHA512 b7b7915e145c7154746234766998005f7aedd1d4dbb19748e0fb76b033984c3a6beb7a7b73c8dda145ccfb27701ff75b073ee3aebdb3ae1eb28419a873e31ab6
    HEAD_REF main
)

if(VCPKG_TARGET_IS_WINDOWS)
    vcpkg_check_linkage(ONLY_STATIC_LIBRARY)
endif()
string(COMPARE EQUAL "${VCPKG_LIBRARY_LINKAGE}" "dynamic" SQLGEN_BUILD_SHARED)

vcpkg_check_features(OUT_FEATURE_OPTIONS FEATURE_OPTIONS
    FEATURES
        duckdb              SQLGEN_DUCKDB
        mariadb             SQLGEN_MYSQL
        postgres            SQLGEN_POSTGRES
)

vcpkg_cmake_configure(
    SOURCE_PATH ${SOURCE_PATH}
    OPTIONS
        ${FEATURE_OPTIONS}
        -DSQLGEN_BUILD_TESTS=OFF
        -DSQLGEN_SQLITE3=ON
        -DSQLGEN_BUILD_SHARED=${SQLGEN_BUILD_SHARED}
)

vcpkg_cmake_install()

vcpkg_cmake_config_fixup(
    CONFIG_PATH "lib/cmake/${PORT}"
)

file(REMOVE_RECURSE
    "${CURRENT_PACKAGES_DIR}/debug/include"
    "${CURRENT_PACKAGES_DIR}/debug/share"
)

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE")
configure_file("${CMAKE_CURRENT_LIST_DIR}/usage" "${CURRENT_PACKAGES_DIR}/share/${PORT}/usage" COPYONLY)
