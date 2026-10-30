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
 * Template: cpp_field_group.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_DOMAIN_CONVENTION_IDENTITY_HPP
#define ORES_REFDATA_API_DOMAIN_CONVENTION_IDENTITY_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <tuple>

namespace ores::refdata::domain {

/**
 * @brief The key, the version and the owning party of a convention.
 *
 * What makes a convention the one a caller means: its surrogate key, the party
 * that holds it and the kind it is, with the scaffold's version and tenant. Every
 * read of a convention starts here, so it is one struct rather than five members
 * scattered through the row.
 *
 * Extracted as a plain nested sub-struct so that
 * rfl::internal::no_duplicate_field_names never instantiates a Literal
 * wider than 5 fields for this block. See the decomposition section of
 * doc/knowledge/architecture/data_oriented_design.org.
 */
struct convention_identity {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The surrogate key of this convention.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the convention.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The convention kind this row is, such as TENOR_BASIS_SWAP.
     */
    std::string kind_code;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const convention_identity&, const convention_identity&) = default;
};
/**
 * @brief The members of convention_identity, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(const convention_identity&) {
    return std::tuple{
        utility::codec::field{"version", &convention_identity::version},
        utility::codec::field{"tenant_id", &convention_identity::tenant_id},
        utility::codec::field{"id", &convention_identity::id},
        utility::codec::field{"party_id", &convention_identity::party_id},
        utility::codec::field{"kind_code", &convention_identity::kind_code},
    };
}

}

#endif
