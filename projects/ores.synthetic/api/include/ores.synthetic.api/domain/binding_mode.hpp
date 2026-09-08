#ifndef ORES_SYNTHETIC_API_DOMAIN_BINDING_MODE_HPP
#define ORES_SYNTHETIC_API_DOMAIN_BINDING_MODE_HPP

#include <ostream>
#include <optional>
#include <stdexcept>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief Whether the generated data is authoritative or exploratory.
 *
 * Orthogonal to scope, which decides who consumes the generated data.
 * bound feeds publish on the subjects the marketdata ingest loop
 * subscribes to; sandboxed feeds publish on distinct subjects nothing
 * ingests.
 */
enum class binding_mode {
    bound,     ///< Authoritative: publishes on the ingested subjects.
    sandboxed  ///< Exploratory: publishes on non-ingested subjects.
};

/**
 * @brief Convert a binding_mode to its lowercase string representation.
 *
 * The two values match the SQL @c check constraint values and the JSON
 * wire format. Throws @c std::invalid_argument on an out-of-range value.
 */
[[nodiscard]] inline std::string_view to_string(binding_mode m) {
    switch (m) {
        case binding_mode::bound:
            return "bound";
        case binding_mode::sandboxed:
            return "sandboxed";
    }
    throw std::invalid_argument("Out-of-range binding_mode");
}

/**
 * @brief Stream a binding_mode using its string representation.
 *
 * Generated table code streams entity members directly; without this
 * operator a domain-enum member in a table display does not compile.
 */
inline std::ostream& operator<<(std::ostream& s, binding_mode m) {
    return s << to_string(m);
}

/**
 * @brief Parse a binding_mode from its lowercase string representation.
 *
 * Returns @c std::nullopt for an unrecognised value so the caller has to
 * decide whether the input is genuinely a parsing error or just absent.
 */
[[nodiscard]] inline std::optional<binding_mode> binding_mode_from_string(std::string_view sv) {
    if (sv == "bound")
        return binding_mode::bound;
    if (sv == "sandboxed")
        return binding_mode::sandboxed;
    return std::nullopt;
}

}

#endif
