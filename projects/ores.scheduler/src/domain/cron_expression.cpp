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
#include <croncpp.h>
#include "ores.scheduler/domain/cron_expression.hpp"

namespace ores::scheduler::domain {

namespace {

/**
 * @brief Convert a 5-field pg_cron expression to croncpp's 6-field format.
 *
 * pg_cron uses standard Unix cron: minute hour day month weekday
 * croncpp requires:               second minute hour day month weekday
 *
 * We prepend "0 " (seconds = 0) to bridge the two formats.
 */
std::string to_croncpp_expr(std::string_view pg_cron_expr) {
    return "0 " + std::string(pg_cron_expr);
}

} // anonymous namespace

cron_expression::cron_expression(std::string validated_expr)
    : expr_(std::move(validated_expr)) {}

std::expected<cron_expression, std::string>
cron_expression::from_string(std::string_view expr) {
    // pg_cron uses 5-field cron (minute hour day month weekday).
    // croncpp requires 6 fields (second minute hour day month weekday).
    // We validate by prepending "0 " for the seconds field.
    try {
        cron::make_cron(to_croncpp_expr(expr));
        return cron_expression(std::string(expr));
    } catch (const cron::bad_cronexpr& e) {
        return std::unexpected(std::string("Invalid cron expression '")
                               + std::string(expr) + "': " + e.what());
    }
}

const std::string& cron_expression::to_string() const noexcept {
    return expr_;
}

std::chrono::system_clock::time_point
cron_expression::next_occurrence(std::chrono::system_clock::time_point after) const {
    const auto cex = cron::make_cron(to_croncpp_expr(expr_));
    const auto t = std::chrono::system_clock::to_time_t(after);
    return std::chrono::system_clock::from_time_t(cron::cron_next(cex, t));
}

} // namespace ores::scheduler::domain
