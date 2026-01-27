This facet provides instructions on how to update CMakeLists.txt for a domain type.

The CMakeLists.txt in `projects/COMPONENT/src/CMakeLists.txt` will use `GLOB_RECURSE` to pick up all `*.cpp` files automatically so do not add new files.

Ensure the following dependencies are linked:

-   `libfort::fort` for table I/O
-   `reflectcpp::reflectcpp` for JSON I/O
-   `faker-cxx::faker-cxx` for generators
-   `sqlgen::sqlgen` for repository
-   `Boost::boost` for various Boost libraries
