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
#include "ores.marketdata.api/datum/market_index.hpp"
#include <algorithm>
#include <array>
#include <format>
#include <utility>

namespace ores::marketdata::datum {

namespace {

constexpr std::array<std::string_view, index_family_count> family_names{"ibor",
                                                                        "swap",
                                                                        "inflation",
                                                                        "fx",
                                                                        "equity",
                                                                        "commodity",
                                                                        "power",
                                                                        "bond",
                                                                        "bond_future",
                                                                        "cmb",
                                                                        "generic"};

std::unexpected<std::string> refuse(index_family f, std::string_view what) {
    return std::unexpected(std::format("{} index: {}", name_of(f), what));
}

}

std::string_view name_of(index_family f) {
    return family_names[static_cast<std::size_t>(f)];
}

std::optional<index_family> index_family_named(std::string_view name) {
    for (std::size_t i = 0; i < family_names.size(); ++i) {
        if (family_names[i] == name)
            return static_cast<index_family>(i);
    }
    return std::nullopt;
}

market_index::market_index(index_family family, std::string subject, std::vector<field_text> fields)
    : family_(family)
    , subject_(std::move(subject))
    , fields_(std::move(fields)) {}

std::expected<market_index, std::string>
market_index::make(index_family family, std::string subject, std::vector<field_text> fields) {
    const auto& row = index_row_of(family);
    if (subject.empty())
        return refuse(family, std::format("{} cannot be empty", row.subject));

    for (const auto& f : fields) {
        const bool known = std::ranges::any_of(
            row.fields, [&](const index_field_spec& s) { return s.name == f.name; });
        if (!known)
            return refuse(family, std::format("{} is not a field of this family", f.name));
    }

    std::vector<field_text> ordered;
    for (const auto& spec : row.fields) {
        const auto count =
            std::ranges::count_if(fields, [&](const field_text& f) { return f.name == spec.name; });
        if (count > 1)
            return refuse(family, std::format("{} is given twice", spec.name));
        const auto it =
            std::ranges::find_if(fields, [&](const field_text& f) { return f.name == spec.name; });
        if (it == fields.end()) {
            if (!spec.optional)
                return refuse(family, std::format("{} is missing", spec.name));
            continue;
        }
        if (it->text.empty())
            return refuse(family, std::format("{} cannot be empty", spec.name));
        ordered.push_back(std::move(*it));
    }
    return market_index(family, std::move(subject), std::move(ordered));
}

const std::string* market_index::get(std::string_view name) const {
    const auto it =
        std::ranges::find_if(fields_, [&](const field_text& f) { return f.name == name; });
    return it == fields_.end() ? nullptr : &it->text;
}

}
