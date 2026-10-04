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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_API_DOMAIN_COUNTERPARTY_ALIAS_HPP
#define ORES_DQ_API_DOMAIN_COUNTERPARTY_ALIAS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief A name a source system uses for a counterparty, keyed to the counterparty's LEI.
 *
 * A name a source system uses for a counterparty, such as the CPTY_A an ORE
 * document puts in its envelope, staged with the identifier scheme it belongs
 * to and the LEI of the counterparty it names. A counterparty's id differs from
 * tenant to tenant, so the alias names it by LEI; publishing resolves the LEI to
 * the tenant's counterparty and writes the alias as a counterparty identifier.
 */
struct counterparty_alias final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The name the source system uses.
     */
    std::string id_value;

    /**
     * @brief The party identifier scheme the name belongs to, such as ORE.
     */
    std::string id_scheme;

    /**
     * @brief The LEI of the counterparty the name stands for.
     */
    std::string lei;

    /**
     * @brief Optional note on where the name comes from.
     */
    std::string description;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const counterparty_alias&, const counterparty_alias&) = default;
};

/**
 * @brief Dispatch-key identifier for counterparty_alias, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const counterparty_alias&) {
    return "ores.dq.counterparty_alias";
}

}

#endif
