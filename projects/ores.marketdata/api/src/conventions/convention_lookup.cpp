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
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include <format>
#include <optional>
#include <utility>

namespace ores::marketdata::conventions {

namespace {

using datum::market_reference;

std::unexpected<lookup_error> refuse(const market_reference& reference, std::string reason) {
    return std::unexpected(lookup_error{reference.uri(), std::move(reason)});
}

/*
 * The address of the index's family: the index with its tenor dropped. An
 * index that states no tenor is its own family.
 */
std::optional<std::string> family_address_of(const market_reference& reference) {
    const auto* index = reference.index();
    if (index == nullptr)
        return std::nullopt;
    std::vector<datum::market_index::field_text> fields;
    for (const auto& field : index->fields())
        if (field.name != "tenor")
            fields.push_back(field);
    auto family = datum::market_index::make(index->family(), index->subject(), std::move(fields));
    if (!family)
        return std::nullopt;
    auto reference_of_family = market_reference::of(std::move(*family));
    if (!reference_of_family)
        return std::nullopt;
    return reference_of_family->uri();
}

/*
 * The one convention of @p conventions whose address is @p address. Two that
 * state the same address are a conflict the caller must see, not a choice the
 * lookup makes.
 */
template <typename Convention>
std::expected<Convention, lookup_error> the_one_at(const std::vector<Convention>& conventions,
                                                    const std::string& address,
                                                    const market_reference& reference,
                                                    std::string_view what) {
    const Convention* found = nullptr;
    for (const auto& convention : conventions) {
        if (!convention.oresmd_uri || *convention.oresmd_uri != address)
            continue;
        if (found != nullptr)
            return refuse(reference,
                          std::format("more than one {} states {}: {} and {}",
                                      what,
                                      address,
                                      found->id,
                                      convention.id));
        found = &convention;
    }
    if (found == nullptr)
        return refuse(reference, std::format("no {} states {}", what, address));
    return *found;
}

}

convention_lookup::convention_lookup(convention_set conventions)
    : conventions_(std::move(conventions)) {}

std::expected<index_definition, lookup_error>
convention_lookup::index_definition_of(const market_reference& index) const {
    const auto family = family_address_of(index);
    if (!family)
        return refuse(index, "it addresses no index");

    auto ibor = the_one_at(conventions_.ibor_indices, *family, index, "Ibor index convention");
    auto overnight =
        the_one_at(conventions_.overnight_indices, *family, index, "overnight index convention");
    if (ibor && overnight)
        return refuse(index,
                      std::format("both the Ibor index convention {} and the overnight index "
                                  "convention {} state {}",
                                  ibor->id,
                                  overnight->id,
                                  *family));
    if (ibor)
        return index_definition{std::move(*ibor)};
    if (overnight)
        return index_definition{std::move(*overnight)};
    return refuse(index, std::format("no Ibor or overnight index convention states {}", *family));
}

std::expected<refdata::domain::swap_convention, lookup_error>
convention_lookup::swap_convention_of(const market_reference& index) const {
    if (index.index() == nullptr)
        return refuse(index, "it addresses no index");
    return the_one_at(conventions_.swaps, index.uri(), index, "swap convention");
}

std::expected<refdata::domain::ois_convention, lookup_error>
convention_lookup::ois_convention_of(const market_reference& index) const {
    if (index.index() == nullptr)
        return refuse(index, "it addresses no index");
    return the_one_at(conventions_.ois_swaps, index.uri(), index, "OIS convention");
}

}
