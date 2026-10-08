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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FLOAT_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FLOAT_DATA_HPP

#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief A number the document states with an optional start date.
 *
 * The fixed rates, the spreads, the caps, the floors and the gearings all carry
 * this shape, and so does a notional. The value is required in the schema, so an
 * unengaged start date is the only presence question here.
 */
struct bond_float_data {
    /**
     * @brief The number the document states.
     */
    double value = {};

    /**
     * @brief The date the number applies from, when the document states one.
     */
    std::optional<std::string> start_date;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_float_data&, const bond_float_data&) = default;
};

}

#endif
