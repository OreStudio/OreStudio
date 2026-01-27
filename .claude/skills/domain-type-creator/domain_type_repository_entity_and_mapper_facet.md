This facet provides instructions on how to create repository entity and mapper for a domain type.

Create the database entity and mapper for ORM support:

1.  Entity header: `projects/COMPONENT/include/COMPONENT/repository/TYPE_NAME_entity.hpp`
2.  Entity implementation: `projects/COMPONENT/src/repository/TYPE_NAME_entity.cpp`
3.  Mapper header: `projects/COMPONENT/include/COMPONENT/repository/TYPE_NAME_mapper.hpp`
4.  Mapper implementation: `projects/COMPONENT/src/repository/TYPE_NAME_mapper.cpp`

One domain class does not always map to a single entity. Entities reflect database tables. Clarify with the user how domain classes should be mapped to database entities.

The entity should:

-   Use sqlgen annotations for table and column mapping
-   Include all fields from the domain type.
-   Follow the pattern in `projects/ores.accounts/include/ores.accounts/repository/account_entity.hpp`

The mapper should:

-   Provide `to_domain()` method to convert entity to domain type
-   Provide `from_domain()` method to convert domain type to entity
-   Handle type conversions (e.g., UUID to string, timestamp conversions)
-   Follow the pattern in `projects/ores.accounts/src/repository/account_mapper.cpp`
