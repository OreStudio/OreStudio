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
#ifndef ORES_DQ_API_DOMAIN_CSA_HPP
#define ORES_DQ_API_DOMAIN_CSA_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief A credit support annex staged with the code of the netting set it collateralises.
 *
 * The credit support annex of a netting set, with the terms ORE reads from a
 * netting set definition. The set is named by code; publishing resolves it in
 * the target tenant. The eligible collateral currencies are a comma separated
 * list in posting order.
 */
struct csa final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The code of the netting set the CSA collateralises.
     */
    std::string netting_set_code;

    /**
     * @brief ORE's ActiveCSAFlag.
     */
    bool is_active = false;

    /**
     * @brief Bilateral, CallOnly or PostOnly.
     */
    std::optional<std::string> bilateral;

    /**
     * @brief The currency the CSA is held in.
     */
    std::optional<std::string> csa_currency;

    /**
     * @brief The index collateral accrues at.
     */
    std::optional<std::string> index_name;

    /**
     * @brief The exposure the firm may owe before it must post.
     */
    std::optional<double> threshold_pay;

    /**
     * @brief The exposure the counterparty may owe before it must post.
     */
    std::optional<double> threshold_receive;

    /**
     * @brief The smallest amount the firm transfers.
     */
    std::optional<ores::utility::decimal::decimal> minimum_transfer_amount_pay;

    /**
     * @brief The smallest amount the counterparty transfers.
     */
    std::optional<ores::utility::decimal::decimal> minimum_transfer_amount_receive;

    /**
     * @brief The independent amount held.
     */
    std::optional<ores::utility::decimal::decimal> independent_amount_held;

    /**
     * @brief FIXED, as ORE names it.
     */
    std::optional<std::string> independent_amount_type;

    /**
     * @brief How often the firm calls for collateral.
     */
    std::optional<std::string> call_frequency;

    /**
     * @brief How often the firm posts collateral.
     */
    std::optional<std::string> post_frequency;

    /**
     * @brief ORE's margin period of risk.
     */
    std::optional<std::string> margin_period_of_risk;

    /**
     * @brief The spread on collateral received.
     */
    std::optional<ores::utility::decimal::decimal> collateral_compounding_spread_receive;

    /**
     * @brief The spread on collateral posted.
     */
    std::optional<ores::utility::decimal::decimal> collateral_compounding_spread_pay;

    /**
     * @brief ORE's ApplyInitialMargin.
     */
    std::optional<bool> apply_initial_margin;

    /**
     * @brief Bilateral, CallOnly or PostOnly.
     */
    std::optional<std::string> initial_margin_type;

    /**
     * @brief ORE's CalculateIMAmount.
     */
    std::optional<bool> calculate_im_amount;

    /**
     * @brief ORE's CalculateVMAmount.
     */
    std::optional<bool> calculate_vm_amount;

    /**
     * @brief ORE's NonExemptIMRegulations.
     */
    std::optional<std::string> non_exempt_im_regulations;

    /**
     * @brief The eligible collateral currencies, comma separated, in posting order.
     */
    std::optional<std::string> eligible_currencies;

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
    friend bool operator==(const csa&, const csa&) = default;
};

/**
 * @brief Dispatch-key identifier for csa, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const csa&) {
    return "ores.dq.csa";
}

}

#endif
