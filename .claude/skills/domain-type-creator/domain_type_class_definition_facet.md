This facet provides instructions on how to generate a domain type class definition.

Create the header file under:

-   `projects/COMPONENT/include/COMPONENT/domain/TYPE_NAME.hpp`

The domain class should:

-   Use a `struct` with all fields public.
-   Include comprehensive doxygen comments for the struct and all fields.
-   Use appropriate C++ types:
    -   `boost::uuids::uuid` for unique identifiers
    -   `std::string` for text fields
    -   `bool` for flags
    -   `int` or appropriate numeric types for counters
    -   `std::chrono::system_clock::time_point` for timestamps
    -   `boost::asio::ip::address` for IP addresses
-   Follow the pattern established in [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.iam/include/ores.iam/domain/account.hppprojects/ores.iam/include/ores.iam/domain/account.hpp).

Example structure:

```cpp
namespace ores::COMPONENT::domain {

/**
 * @brief Brief description of the domain type.
 */
struct TYPE_NAME final {
    /**
     * @brief Description of field.
     */
    TYPE field_name;

    // ... more fields
};

}
```
