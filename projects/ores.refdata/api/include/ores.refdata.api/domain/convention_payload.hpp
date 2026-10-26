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
#ifndef ORES_REFDATA_API_DOMAIN_CONVENTION_PAYLOAD_HPP
#define ORES_REFDATA_API_DOMAIN_CONVENTION_PAYLOAD_HPP

#include <optional>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief What a convention carries besides its identity.
 *
 * The name the convention carries in the source document and the address of the
 * market data it needs, when its kind names one index. Everything kind-specific
 * lives in a part table, so this is all a central row holds beyond its identity.
 *
 * Extracted as a plain nested sub-struct so that
 * rfl::internal::no_duplicate_field_names never instantiates a Literal
 * wider than 5 fields for this block. See the decomposition section of
 * doc/knowledge/architecture/data_oriented_design.org.
 */
struct convention_payload {
    /**
     * @brief The name the convention carries in the source document, its ORE Id element.
     */
    std::string name;

    /**
     * @brief The oresmd URI of the market data this convention needs, when the kind names one
     * index.
     */
    std::optional<std::string> oresmd_uri;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const convention_payload&, const convention_payload&) = default;
};

}

#endif
