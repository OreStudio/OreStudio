This facet provides instructions on how to generate table I/O support for a domain type.

Create two files for table output:

1.  Header: `projects/COMPONENT/include/COMPONENT/domain/TYPE_NAME_table_io.hpp`
2.  Implementation: `projects/COMPONENT/src/domain/TYPE_NAME_table_io.cpp`

The header should declare:

```cpp
#include <iosfwd>
#include <vector>
#include "COMPONENT/domain/TYPE_NAME.hpp"

namespace ores::COMPONENT::domain {

/**
 * @brief Dumps the TYPE_NAME object to a stream in table format.
 */
std::ostream& operator<<(std::ostream& s, const std::vector<TYPE_NAME>& v);

}
```

The implementation should:

-   Include `<fort.hpp>` for table formatting
-   Include `<boost/uuid/uuid_io.hpp>` for UUID string conversion
-   Use `fort::char_table` with `FT_BASIC_STYLE`
-   Create appropriate column headers using `fort::header`
-   Format special types appropriately:
    -   UUIDs: use `boost::uuids::to_string()`
    -   Booleans: convert to "Y"/"N"
    -   Timestamps: format using `std::put_time()` as "YYYY-MM-DD HH:MM:SS"
    -   IP addresses: use `.to_string()`
-   End each row with `fort::endr`
-   Follow the pattern in `projects/ores.accounts/src/domain/account_table_io.cpp`
