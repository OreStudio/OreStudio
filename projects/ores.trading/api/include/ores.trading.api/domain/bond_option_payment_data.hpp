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
#ifndef ORES_TRADING_API_DOMAIN_BOND_OPTION_PAYMENT_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_OPTION_PAYMENT_DATA_HPP

#include "ores.trading.api/domain/bond_option_payment_rules.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief An option's payment dates, stated either as a list or as a rule.
 *
 */
struct bond_option_payment_data {
    /**
     * @brief The payment dates the document states outright, in document order.
     */
    std::vector<std::string> dates;

    /**
     * @brief The rule the payment dates are derived from, when the document states one.
     */
    std::optional<bond_option_payment_rules> rules;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_option_payment_data&,
                           const bond_option_payment_data&) = default;
};

}

#endif
