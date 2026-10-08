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
#ifndef ORES_TRADING_API_DOMAIN_BOND_OPTION_SETTLEMENT_HPP
#define ORES_TRADING_API_DOMAIN_BOND_OPTION_SETTLEMENT_HPP

#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief The settlement terms an option or a premium pays under.
 *
 * The pay currency and the FX index are required and the fixing date is
 * optional. The schema spells the same three members twice, once under the
 * option and once under each premium, so the container holds one type for both.
 */
struct bond_option_settlement {
    /**
     * @brief The currency the option or premium pays in.
     */
    std::string pay_currency;

    /**
     * @brief The FX index the payment settles against.
     */
    std::string fx_index;

    /**
     * @brief The date the index fixes on, when the document states one.
     */
    std::optional<std::string> fixing_date;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_option_settlement&, const bond_option_settlement&) = default;
};

}

#endif
