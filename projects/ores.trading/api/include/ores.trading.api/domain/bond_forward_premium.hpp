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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FORWARD_PREMIUM_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FORWARD_PREMIUM_HPP

#include <string>

namespace ores::trading::domain {

/**
 * @brief A forward bond's premium: the amount and the date it pays.
 *
 * Both members are required strings, and the schema spells the amount as text
 * rather than as a number.
 */
struct bond_forward_premium {
    /**
     * @brief The premium amount, as the document spells it.
     */
    std::string amount;

    /**
     * @brief The date the premium pays on, as ISO 8601 text.
     */
    std::string date;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_forward_premium&, const bond_forward_premium&) = default;
};

}

#endif
