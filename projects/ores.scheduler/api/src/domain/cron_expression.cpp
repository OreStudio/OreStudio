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
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include <algorithm>
#include <array>
#include <croncpp.h>
#include <string>

namespace ores::scheduler::domain {

namespace {

/**
 * @brief Convert a 5-field cron expression to croncpp's 6-field format.
 *
 * The job definition uses standard Unix cron: minute hour day month weekday
 * croncpp requires:               second minute hour day month weekday
 *
 * We prepend "0 " (seconds = 0) to bridge the two formats.
 */
std::string to_croncpp_expr(std::string_view expr) {
    return "0 " + std::string(expr);
}

/**
 * @brief Count the fields the way croncpp splits them.
 *
 * croncpp splits on the space character alone and drops the empty parts, so a
 * run of spaces separates once and a tab does not separate at all. Counting
 * the same way keeps this check and the library from disagreeing about how
 * many fields an expression holds.
 */
std::size_t count_fields(std::string_view expr) {
    std::size_t fields = 0;
    bool in_field = false;
    for (const char c : expr) {
        if (c == ' ')
            in_field = false;
        else if (!in_field) {
            ++fields;
            in_field = true;
        }
    }
    return fields;
}

/**
 * @brief The five fields of an expression, split the way croncpp splits them.
 *
 * A validated expression always holds five fields, so the array is filled.
 */
std::array<std::string_view, 5> fields_of(std::string_view expr) {
    std::array<std::string_view, 5> fields{};
    std::size_t index = 0;
    std::size_t start = std::string_view::npos;
    for (std::size_t i = 0; i <= expr.size() && index < fields.size(); ++i) {
        const bool separator = i == expr.size() || expr[i] == ' ';
        if (!separator) {
            if (start == std::string_view::npos)
                start = i;
            continue;
        }
        if (start != std::string_view::npos) {
            fields[index++] = expr.substr(start, i - start);
            start = std::string_view::npos;
        }
    }
    return fields;
}

/**
 * @brief The expression with one field replaced.
 *
 * Used to read one of the two day fields without the other, which is how the
 * POSIX "either field matches" rule is evaluated.
 */
std::string with_field(std::string_view expr, std::size_t replaced, std::string_view value) {
    const auto fields = fields_of(expr);
    std::string result;
    for (std::size_t i = 0; i < fields.size(); ++i) {
        if (i > 0)
            result += ' ';
        result += i == replaced ? value : fields[i];
    }
    return result;
}

/**
 * @brief Whether a day field narrows the days that match.
 *
 * POSIX reads a star as every day, and applies its "either field matches"
 * rule to the two day fields only when both narrow.
 */
bool narrows_days(std::string_view field) {
    return field != "*";
}

// Field positions in a 5-field expression: minute hour day month weekday.
constexpr std::size_t day_of_month_field = 2;
constexpr std::size_t day_of_week_field = 4;

} // anonymous namespace

cron_expression::cron_expression()
    : expr_("* * * * *") {}

cron_expression::cron_expression(std::string validated_expr)
    : expr_(std::move(validated_expr)) {}

std::expected<cron_expression, std::string> cron_expression::from_string(std::string_view expr) {
    // croncpp also accepts a trailing year field, so a six-field input would
    // pass its validation with every field shifted one place. The field count
    // is checked here rather than left to the library.
    if (const auto fields = count_fields(expr); fields != 5)
        return std::unexpected(std::string("Invalid cron expression '") + std::string(expr) +
                               "': expected 5 fields (minute hour day month weekday), found " +
                               std::to_string(fields) + ".");
    try {
        cron::make_cron(to_croncpp_expr(expr));
        return cron_expression(std::string(expr));
    } catch (const cron::bad_cronexpr& e) {
        return std::unexpected(std::string("Invalid cron expression '") + std::string(expr) +
                               "': " + e.what());
    }
}

const std::string& cron_expression::to_string() const noexcept {
    return expr_;
}

std::chrono::system_clock::time_point
cron_expression::next_occurrence(std::chrono::system_clock::time_point after) const {
    const auto from = std::chrono::system_clock::to_time_t(after);
    const auto next_for = [from](const std::string& expr) {
        const auto cex = cron::make_cron(to_croncpp_expr(expr));
        return std::chrono::system_clock::from_time_t(cron::cron_next(cex, from));
    };

    const auto fields = fields_of(expr_);
    if (!narrows_days(fields[day_of_month_field]) || !narrows_days(fields[day_of_week_field]))
        return next_for(expr_);

    // POSIX fires when either restricted day field matches, and the library
    // fires only when both do. Asking each field on its own and taking the
    // earlier answer is the POSIX rule.
    return std::min(next_for(with_field(expr_, day_of_week_field, "*")),
                    next_for(with_field(expr_, day_of_month_field, "*")));
}

} // namespace ores::scheduler::domain
