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
#ifndef ORES_MARKETDATA_API_DOMAIN_SERIES_AXIS_HPP
#define ORES_MARKETDATA_API_DOMAIN_SERIES_AXIS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One coordinate field a composite market series varies over, in the order the object's key
 * writes it.
 *
 * The axes a composite market series varies over. A term structure varies over one
 * axis and a surface over two. Each row names one field the series' instrument type
 * marks as a coordinate, so the object's shape is a declared thing rather than an
 * inference from whatever points happen to be stored.
 *
 * The rows are ordered. The sequence column holds the position of each axis among
 * the object's coordinates, so a reader walks the axes in the order the type's key
 * writes them and a hole in a grid is a hole rather than an absent row.
 *
 * The table is a current state, not a history: the series row is already temporal
 * and the shape of a series is written again whenever it is built. A series with no
 * rows here declares no shape, and a caller that reads one gets nothing rather than
 * a fabricated default.
 */
struct series_axis final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The series this axis belongs to. A soft reference: the series table is temporal and
     * its primary key carries the validity window, so no foreign key can name a single current row
     * of it.
     */
    boost::uuids::uuid series_id;

    /**
     * @brief The oresmd field code the axis is, which is one of the coordinate fields the series'
     * instrument type declares. A type that declares no coordinate has no axis row.
     */
    std::string axis_field;

    /**
     * @brief The owning party, copied so a query filters without joining the series.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The axis's position among the object's coordinates. The order the schema row declares
     * is the order the values are read.
     */
    int sequence = 0;

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
    friend bool operator==(const series_axis&, const series_axis&) = default;
};

/**
 * @brief Dispatch-key identifier for series_axis, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const series_axis&) {
    return "ores.marketdata.series_axis";
}

}

#endif
