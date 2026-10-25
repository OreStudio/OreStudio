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
#ifndef ORES_TRADING_API_DOMAIN_BOND_OPTION_PAYMENT_RULES_HPP
#define ORES_TRADING_API_DOMAIN_BOND_OPTION_PAYMENT_RULES_HPP

#include <cstdint>
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief The rule an option's payment dates are derived from.
 *
 */
struct bond_option_payment_rules {
    /**
     * @brief How long after each period end the payment falls.
     */
    std::uint64_t lag = 0;

    /**
     * @brief Business day calendar the payment dates are adjusted against.
     */
    std::string calendar;

    /**
     * @brief Business day convention the payment dates are adjusted under.
     */
    std::string convention;

    /**
     * @brief What the payment dates are stated relative to, when the document states it.
     */
    std::optional<std::string> relative_to;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_option_payment_rules&,
                           const bond_option_payment_rules&) = default;
};

}

#endif
