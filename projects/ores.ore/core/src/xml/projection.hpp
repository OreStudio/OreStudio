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
#ifndef ORES_ORE_CORE_XML_PROJECTION_HPP
#define ORES_ORE_CORE_XML_PROJECTION_HPP

#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/generated/calendar_adjustments.hpp"
#include "ores.ore.core/xml/generated/currency_config.hpp"
#include <format>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::ore::xml::projection {

/**
 * @brief States the row a projection fills, or refuses to write silently.
 *
 * A rename in the schema reaches the projection as a name the table no longer
 * has, and a writer that skipped it would emit a document missing a field.
 */
inline void state(const record_type& type, void* record, std::string_view name) {
    if (!mark_present(type, record, name))
        throw std::runtime_error(std::format("the {} table has no '{}' field", type.name, name));
}

/**
 * @brief The table the Currency element of the currency config is described by.
 */
inline auto currency_type() -> const record_type& {
    const auto* found =
        find_child_type(*generated::currency_config::currency_config_document().root, "Currency");
    if (found == nullptr)
        throw std::runtime_error("the currency config table has no Currency element");
    return *found;
}

/**
 * @brief The table the Calendar element of the calendar adjustments is
 * described by.
 */
inline auto calendar_type() -> const record_type& {
    const auto* found = find_child_type(
        *generated::calendar_adjustments::calendar_adjustments_document().root, "Calendar");
    if (found == nullptr)
        throw std::runtime_error("the calendar adjustments table has no Calendar element");
    return *found;
}

}

#endif
