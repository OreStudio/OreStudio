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
#include "ores.ore.service/messaging/family_row_party.hpp"

namespace ores::ore::service::messaging {

namespace {

const rfl::Generic* member_named(const rfl::Generic& value, const std::string& key) {
    const auto* object = std::get_if<rfl::Generic::Object>(&value.get());
    if (object == nullptr)
        return nullptr;
    for (const auto& [name, member] : *object)
        if (name == key)
            return &member;
    return nullptr;
}

std::string text_of(const rfl::Generic* value) {
    if (value == nullptr)
        return {};
    const auto* text = std::get_if<std::string>(&value->get());
    return text == nullptr ? std::string{} : *text;
}

}

std::string party_of_family_row(const rfl::Generic& row) {
    if (auto own = text_of(member_named(row, "party_id")); !own.empty())
        return own;
    if (const auto* identity = member_named(row, "identity"); identity != nullptr)
        return text_of(member_named(*identity, "party_id"));
    return {};
}

}
