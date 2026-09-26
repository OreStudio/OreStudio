/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#pragma once

#include "ores.scheduler.api/export.hpp"
#include <chrono>
#include <expected>
#include <ostream>
#include <rfl.hpp>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::scheduler::domain {

/**
 * @brief Strongly-typed, validated cron expression.
 *
 * Wraps a cron expression string, validating it on construction via croncpp.
 * Provides C++23 chrono-based next-occurrence computation and serialisation
 * to the plain string required by pg_cron's cron.schedule() function.
 *
 * Use the factory method from_string() to construct:
 * @code
 *   auto expr = cron_expression::from_string("0 0 * * *");
 *   if (expr) {
 *       auto next = expr->next_occurrence();
 *   }
 * @endcode
 */
class ORES_SCHEDULER_API_EXPORT cron_expression final {
public:
    /**
     * @brief Default constructor. Creates an expression that runs every minute.
     *
     * Provides a valid default state so that types containing cron_expression
     * (e.g. job_definition) can be default-constructed for UI purposes.
     * Equivalent to "* * * * *".
     */
    cron_expression();

    /**
     * @brief Parse and validate a cron expression string.
     *
     * Accepts standard 5-field cron syntax (minute hour day month weekday).
     * Returns an error string if the expression is malformed.
     */
    [[nodiscard]] static std::expected<cron_expression, std::string>
    from_string(std::string_view expr);

    /**
     * @brief The validated cron string, suitable for pg_cron's cron.schedule().
     */
    [[nodiscard]] const std::string& to_string() const noexcept;

    /**
     * @brief Compute the next occurrence after the given time point.
     *
     * Defaults to the current wall-clock time when no argument is supplied.
     */
    [[nodiscard]] std::chrono::system_clock::time_point next_occurrence(
        std::chrono::system_clock::time_point after = std::chrono::system_clock::now()) const;

    bool operator==(const cron_expression& other) const noexcept = default;

private:
    explicit cron_expression(std::string validated_expr);

    std::string expr_;
};

} // namespace ores::scheduler::domain

/**
 * @brief Writes the expression's validated string.
 *
 * A cron expression is read as the string it wraps, which is what a table,
 * a log line or a message shows.
 */
inline std::ostream& operator<<(std::ostream& s,
                                const ores::scheduler::domain::cron_expression& v) {
    return s << v.to_string();
}

namespace rfl {

/**
 * @brief Custom reflector for ores::scheduler::domain::cron_expression.
 *
 * Serialises the expression as its validated string and parses it back
 * through the factory, so a round trip cannot produce an invalid expression.
 * The reflector sits beside the type it reflects, which is what lets the
 * generated JSON I/O for an entity that carries one see it.
 */
template <>
struct Reflector<ores::scheduler::domain::cron_expression> {
    using ReflType = std::string;

    static ores::scheduler::domain::cron_expression to(const ReflType& str) {
        auto result = ores::scheduler::domain::cron_expression::from_string(str);
        if (!result)
            throw std::runtime_error("Invalid cron expression: " + result.error());
        return *result;
    }

    static ReflType from(const ores::scheduler::domain::cron_expression& v) {
        return v.to_string();
    }
};

}
