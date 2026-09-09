#ifndef ORES_SYNTHETIC_API_DOMAIN_SCOPE_HPP
#define ORES_SYNTHETIC_API_DOMAIN_SCOPE_HPP

#include <optional>
#include <ostream>
#include <stdexcept>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief Sharing radius of a market_data_generation_config: who consumes
 * the same generated data.
 *
 * Orthogonal to binding_mode, which decides whether the generated data
 * is authoritative. party_id/tenant_id are null/set per level: system
 * (both null), tenant (tenant_id only), party (both set).
 */
enum class scope {
    system, ///< Shared across every tenant. Not exercised end-to-end yet.
    tenant, ///< Shared across every party under one tenant.
    party   ///< Owned by, and visible only to, a single party.
};

/**
 * @brief Convert a scope to its lowercase string representation.
 *
 * The three values match the SQL @c check constraint values and the JSON
 * wire format. Throws @c std::invalid_argument on an out-of-range value.
 */
[[nodiscard]] inline std::string_view to_string(scope s) {
    switch (s) {
        case scope::system:
            return "system";
        case scope::tenant:
            return "tenant";
        case scope::party:
            return "party";
    }
    throw std::invalid_argument("Out-of-range scope");
}

/**
 * @brief Stream a scope using its string representation.
 *
 * Generated table code streams entity members directly; without this
 * operator a domain-enum member in a table display does not compile.
 */
inline std::ostream& operator<<(std::ostream& s, scope s2) {
    return s << to_string(s2);
}

/**
 * @brief Parse a scope from its lowercase string representation.
 *
 * Returns @c std::nullopt for an unrecognised value so the caller has to
 * decide whether the input is genuinely a parsing error or just absent.
 */
[[nodiscard]] inline std::optional<scope> scope_from_string(std::string_view sv) {
    if (sv == "system")
        return scope::system;
    if (sv == "tenant")
        return scope::tenant;
    if (sv == "party")
        return scope::party;
    return std::nullopt;
}

}

#endif
