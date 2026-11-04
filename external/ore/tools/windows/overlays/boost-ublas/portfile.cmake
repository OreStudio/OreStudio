# Same source and build as the pinned vcpkg port, with the MSVC macro fix
# discussed at https://github.com/boostorg/ublas/issues/77.
vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO boostorg/ublas
    REF boost-${VERSION}
    SHA512 9ab089724699c7bc8f8c188412a025cdf33b3afb4c9d70affcbb14034af46f1c4997eb84bb0bdbcafb82d34920d0e04ebf717b724c292dd32eac3c6a9289fec1
    HEAD_REF master
    PATCHES msvc-iterator-debug-level.patch
)
boost_configure_and_install(SOURCE_PATH "${SOURCE_PATH}")
