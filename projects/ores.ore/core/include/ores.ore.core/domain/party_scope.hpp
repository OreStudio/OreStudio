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
#ifndef ORES_ORE_CORE_DOMAIN_PARTY_SCOPE_HPP
#define ORES_ORE_CORE_DOMAIN_PARTY_SCOPE_HPP

#include <boost/uuid/uuid.hpp>
#include <optional>
#include <rfl.hpp>
#include <type_traits>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief Stamps the owning party on every row of a mapped document.
 *
 * A configuration document belongs to one party, and every row it maps to
 * carries that party's id. The mappers know nothing of parties, as they know
 * nothing of tenants, so whoever persists a mapped document stamps it first.
 *
 * Walks the value: a row with a party_id is stamped; a vector, an optional or
 * a struct of rows is walked into; anything else is left alone.
 */
template <typename T>
void assign_party(T& v, const boost::uuids::uuid& party) {
    if constexpr (requires { v.party_id = party; }) {
        v.party_id = party;
    } else if constexpr (requires {
                             v.begin();
                             v.end();
                             typename T::value_type;
                         }) {
        if constexpr (!std::is_same_v<typename T::value_type, char>)
            for (auto& e : v)
                assign_party(e, party);
    } else if constexpr (requires {
                             v.has_value();
                             *v;
                         }) {
        if (v)
            assign_party(*v, party);
    } else if constexpr (std::is_class_v<T> && std::is_aggregate_v<T>) {
        rfl::to_view(v).apply([&](const auto& field) { assign_party(*field.value(), party); });
    }
}

}

#endif
