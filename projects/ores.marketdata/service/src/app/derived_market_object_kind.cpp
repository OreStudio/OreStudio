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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include "ores.marketdata.service/app/derived_market_object_kind.hpp"
#include <algorithm>

namespace ores::marketdata::service::app {

std::optional<kind_declaration> declaration_of_publish_kind(std::string_view publish_kind_code) {
    const auto found = std::ranges::find_if(kind_declarations, [publish_kind_code](const auto& d) {
        return d.publish_kind_code == publish_kind_code;
    });
    if (found == kind_declarations.end())
        return std::nullopt;
    return *found;
}

const kind_declaration& declaration_of(derived_kind kind) {
    const auto found =
        std::ranges::find_if(kind_declarations, [kind](const auto& d) { return d.kind == kind; });
    return *found;
}

bool can_withdraw(const kind_declaration& declaration) {
    return !declaration.withdrawal_kind_code.empty();
}

std::optional<std::string> withdrawal_refusal_of_publish_kind(std::string_view publish_kind_code) {
    const auto declaration = declaration_of_publish_kind(publish_kind_code);
    if (!declaration)
        return "No kind of derived market object has the publish kind " +
               std::string(publish_kind_code) + ".";
    if (can_withdraw(*declaration))
        return std::nullopt;
    return "A " + std::string(declaration->publish_kind_code) +
           " set cannot be withdrawn: its kind declares no withdrawal kind, so a published set of "
           "it stays in use.";
}

}
