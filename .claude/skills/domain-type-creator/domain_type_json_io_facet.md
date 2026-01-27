This facet provides instructions on how to generate JSON I/O support for a domain type.

Create two files for JSON serialization:

1.  Header: `projects/COMPONENT/include/COMPONENT/domain/TYPE_NAME_json_io.hpp`
2.  Implementation: `projects/COMPONENT/src/domain/TYPE_NAME_json_io.cpp`

The header should declare:

```cpp
#include <iosfwd>
#include "COMPONENT/domain/TYPE_NAME.hpp"

namespace ores::COMPONENT::domain {

/**
 * @brief Dumps the TYPE_NAME object to a stream in JSON format.
 */
std::ostream& operator<<(std::ostream& s, const TYPE_NAME& v);

}
```

The implementation should:

-   Include `<rfl.hpp>` and `<rfl/json.hpp>`
-   Include `ores.utility/rfl/reflectors.hpp` for custom type support
-   Use `rfl::json::write()` for serialization
-   Follow the pattern in `projects/ores.accounts/src/domain/account_json_io.cpp`
