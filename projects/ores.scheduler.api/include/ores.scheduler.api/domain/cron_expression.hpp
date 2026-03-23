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

#include <chrono>
#include <expected>
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
class cron_expression final {
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
    [[nodiscard]] std::chrono::system_clock::time_point
    next_occurrence(std::chrono::system_clock::time_point after
                    = std::chrono::system_clock::now()) const;

    bool operator==(const cron_expression& other) const noexcept = default;

private:
    explicit cron_expression(std::string validated_expr);

    std::string expr_;
};

} // namespace ores::scheduler::domain
