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
#ifndef ORES_REFDATA_API_DOMAIN_CSA_HPP
#define ORES_REFDATA_API_DOMAIN_CSA_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The collateral terms attached to a netting set.
 *
 * A Credit Support Annex holds the collateral terms of a
 * [[id:83C9697A-0D6D-42A2-9917-6C68268B5404][netting set]]: thresholds, minimum transfer amounts,
 * margining frequencies, the margin period of risk and the eligible collateral. Netting reduces
 * exposure, and collateral mitigates what remains (see
 * [[id:5F12A37F-9F26-4B49-BAAF-1BAA7B2BB94F][Netting sets]]).
 *
 * A CSA has a lifecycle of its own: it is added, changed or switched off
 * without touching the trades in its set. A set has at most one active CSA;
 * an inactive one keeps its terms, as ORE keeps the details of a set whose
 * CSA flag is off. The columns follow ORE's CSADetails one for one, and
 * the eligible collateral currencies are [[id:25090994-3093-470E-BB3F-7832EDCA26A4][rows of their
 * own]].
 */
struct csa final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this CSA.
     *
     * Surrogate key for the CSA record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The netting set these collateral terms govern.
     *
     * References the netting sets table.
     */
    boost::uuids::uuid netting_set_id;

    /**
     * @brief Whether the CSA is in force.
     *
     * ORE's ActiveCSAFlag. A set has at most one active CSA; an inactive one keeps its terms.
     */
    bool is_active = false;

    /**
     * @brief Who posts collateral.
     *
     * Bilateral, CallOnly or PostOnly, as ORE names them.
     */
    std::optional<std::string> bilateral;

    /**
     * @brief The currency the CSA is denominated in.
     *
     * An ISO currency code. Not checked against the currencies table: like the currency columns of
     * the instrument models, it holds what the ORE document states.
     */
    std::optional<std::string> csa_currency;

    /**
     * @brief The index that accrues interest on collateral.
     *
     * ORE's Index, such as EUR-EONIA.
     */
    std::optional<std::string> index_name;

    /**
     * @brief The exposure the firm may owe before it must post.
     *
     * Not negative.
     */
    std::optional<double> threshold_pay;

    /**
     * @brief The exposure the counterparty may owe before it must post.
     *
     * Not negative.
     */
    std::optional<double> threshold_receive;

    /**
     * @brief The smallest transfer the firm makes.
     *
     * Not negative.
     */
    std::optional<ores::utility::decimal::decimal> minimum_transfer_amount_pay;

    /**
     * @brief The smallest transfer the counterparty makes.
     *
     * Not negative.
     */
    std::optional<ores::utility::decimal::decimal> minimum_transfer_amount_receive;

    /**
     * @brief The independent amount held.
     *
     * ORE's IndependentAmountHeld.
     */
    std::optional<ores::utility::decimal::decimal> independent_amount_held;

    /**
     * @brief How the independent amount is expressed.
     *
     * ORE's IndependentAmountType; FIXED is the only value ORE defines.
     */
    std::optional<std::string> independent_amount_type;

    /**
     * @brief How often the firm calls for collateral.
     *
     * A period, such as 1D.
     */
    std::optional<std::string> call_frequency;

    /**
     * @brief How often the firm posts collateral.
     *
     * A period, such as 1D.
     */
    std::optional<std::string> post_frequency;

    /**
     * @brief The time to close out after a default.
     *
     * A period, such as 2W.
     */
    std::optional<std::string> margin_period_of_risk;

    /**
     * @brief The spread on collateral the firm receives.
     *
     * ORE's CollateralCompoundingSpreadReceive.
     */
    std::optional<ores::utility::decimal::decimal> collateral_compounding_spread_receive;

    /**
     * @brief The spread on collateral the firm posts.
     *
     * ORE's CollateralCompoundingSpreadPay.
     */
    std::optional<ores::utility::decimal::decimal> collateral_compounding_spread_pay;

    /**
     * @brief Whether initial margin applies.
     *
     * ORE's ApplyInitialMargin.
     */
    std::optional<bool> apply_initial_margin;

    /**
     * @brief Who posts initial margin.
     *
     * Bilateral, CallOnly or PostOnly, as ORE names them.
     */
    std::optional<std::string> initial_margin_type;

    /**
     * @brief Whether ORE calculates the initial margin amount.
     *
     * ORE's CalculateIMAmount.
     */
    std::optional<bool> calculate_im_amount;

    /**
     * @brief Whether ORE calculates the variation margin amount.
     *
     * ORE's CalculateVMAmount.
     */
    std::optional<bool> calculate_vm_amount;

    /**
     * @brief The regulations under which initial margin is not exempt.
     *
     * ORE's NonExemptIMRegulations, as written.
     */
    std::optional<std::string> non_exempt_im_regulations;

    /**
     * @brief Username of the person who last modified this CSA.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

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
    return "ores.refdata.csa";
}

}

#endif
