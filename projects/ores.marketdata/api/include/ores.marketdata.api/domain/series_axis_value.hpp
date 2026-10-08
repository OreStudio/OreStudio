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
#ifndef ORES_MARKETDATA_API_DOMAIN_SERIES_AXIS_VALUE_HPP
#define ORES_MARKETDATA_API_DOMAIN_SERIES_AXIS_VALUE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One value an axis of a composite market series holds, in the order the axis declared it.
 *
 * One value an axis of a composite market series holds. The rows of one axis are
 * its ordered values, so a grid can be read as coordinates rather than as the set
 * of points that survived a build.
 *
 * The sequence column holds the position of each value among its axis's values.
 * A value that the axis declares and no point uses is still a declared value, and
 * a reader can tell it apart from a value the build never saw.
 *
 * The table is a current state, not a history: a rebuilt series writes its shape
 * again. A value is text because the codec keeps every value as the key spelled
 * it, and a projection that reinterpreted it would be a second spelling of the
 * same coordinate.
 */
struct series_axis_value final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The series this axis value belongs to. A soft reference: the series table is temporal
     * and its primary key carries the validity window, so no foreign key can name a single current
     * row of it.
     */
    boost::uuids::uuid series_id;

    /**
     * @brief The oresmd field code of the axis this value belongs to, matching the axis row the
     * series declared.
     */
    std::string axis_field;

    /**
     * @brief The axis value as the coordinate spells it. The writer refuses a value the axis
     * field's vocabulary does not hold.
     */
    std::string value;

    /**
     * @brief The owning party, copied so a query filters without joining the series.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The value's position among its axis's values. The order the writer found them in is
     * the order they are read.
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
    friend bool operator==(const series_axis_value&, const series_axis_value&) = default;
};

/**
 * @brief Dispatch-key identifier for series_axis_value, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const series_axis_value&) {
    return "ores.marketdata.series_axis_value";
}

}

#endif
