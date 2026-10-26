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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_PAY_RECEIVE_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_PAY_RECEIVE_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_part_identity.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_leg.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive_coupon.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_receive_leg.hpp"
#include <chrono>
#include <span>
#include <string_view>
#include <type_traits>

namespace ores::refdata::domain {

/**
 * @brief The paying and receiving leg form of a tenor basis swap convention.
 *
 * The paying and receiving leg form of a tenor basis swap. ORE writes a tenor
 * basis swap as PayIndex and ReceiveIndex with an optional frequency each,
 * plus the averaging and sub-periods fields. A row of this part holds that form.
 * The long and short form lives in tenor_basis_swap_long_short, and a
 * convention fills exactly one of the two.
 */
struct tenor_basis_swap_pay_receive final {
    tenor_basis_swap_part_identity identity;
    tenor_basis_swap_pay_leg pay_leg;
    tenor_basis_swap_receive_leg rec_leg;
    tenor_basis_swap_pay_receive_coupon coupon;
    ores::dq::domain::audit_record audit;
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
    friend bool operator==(const tenor_basis_swap_pay_receive&,
                           const tenor_basis_swap_pay_receive&) = default;
};

/**
 * @brief Names the members of tenor_basis_swap_pay_receive that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<tenor_basis_swap_pay_receive>) {
    return {};
}

/**
 * @brief Dispatch-key identifier for tenor_basis_swap_pay_receive, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_basis_swap_pay_receive&) {
    return "ores.refdata.tenor_basis_swap_pay_receive";
}

}

#endif
