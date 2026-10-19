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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_convention_family_unit.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "convention_family_units.hpp"
#include "conventions_row_helpers.hpp"
#include "ores.refdata.core/repository/cross_currency_fix_float_convention_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <rfl/json.hpp>
#include <stdexcept>
#include <utility>
#include <vector>

namespace ores::refdata::service::convention_family {

void apply_cross_currency_fix_float(const ores::database::context& ctx,
                                    const std::string& payload,
                                    [[maybe_unused]] conventions_apply_result& result) {
    const auto decoded =
        rfl::json::read<ores::refdata::domain::cross_currency_fix_float_convention>(payload);
    if (!decoded)
        throw std::runtime_error("Staged conventions payload for family "
                                 "\"cross_currency_fix_float\" does not decode.");
    auto rows = std::vector<ores::refdata::domain::cross_currency_fix_float_convention>{
        std::move(*decoded)};
    ores::service::messaging::stamp(rows.front(), ctx);
    replace_party_rows(ctx,
                       ores::refdata::repository::cross_currency_fix_float_convention_repository(),
                       std::move(rows));
}

std::vector<std::string>
read_cross_currency_fix_float_payloads(const ores::database::context& ctx) {
    std::vector<std::string> payloads;
    for (const auto& row :
         ores::refdata::repository::cross_currency_fix_float_convention_repository().read_latest(
             ctx))
        payloads.push_back(rfl::json::write(row));
    return payloads;
}

}
