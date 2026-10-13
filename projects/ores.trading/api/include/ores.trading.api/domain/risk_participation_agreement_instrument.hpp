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
#ifndef ORES_TRADING_API_DOMAIN_RISK_PARTICIPATION_AGREEMENT_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_RISK_PARTICIPATION_AGREEMENT_INSTRUMENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief Risk participation agreement facts.
 *
 * Represents the product facts of a RiskParticipationAgreement trade: the participation rate, the
 * credit curve, the issuer, the fixed recovery rate and the two flags. The protection start and end
 * are the header's start and maturity dates. The protection fee is a list of legs in the
 * protection_fee group of swap_legs, and the underlying's legs are in the underlying group. The
 * underlying's option block is the shared option rows, which the agreement owns because it has no
 * option of its own.
 *
 * This row is the family's fact table, not its identity. The header,
 * ores.trading.credit_instrument, holds the trade type code, the party, the
 * instrument's start and maturity dates and its description; this table holds
 * only the product's own fields, and joins the header by trade_id.
 */
struct risk_participation_agreement_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this instrument belongs to, and the instrument's own key.
     *
     * The trade id identifies both the trade and its instrument, so the instrument carries no
     * identity of its own: this column is the key, and the trade it names gives the instrument its
     * scope.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The credit curve of the agreement, from the required CreditCurveId element.
     */
    std::string credit_curve_id;

    /**
     * @brief The issuer the agreement names, from the IssuerId element. Null when the document
     * states none.
     */
    std::optional<std::string> issuer_id;

    /**
     * @brief The fixed recovery rate as a fraction. Null when the document states none.
     */
    std::optional<double> recovery_rate;

    /**
     * @brief The share of the underlying's loss the agreement participates in, from the required
     * ParticipationRate element, as a fraction. Continuous, so a binary float.
     */
    double participation_rate = 0.0;

    /**
     * @brief Whether the accrued premium settles on default, from SettlesAccrual. Null when the
     * document states none.
     */
    std::optional<bool> settles_accrual;

    /**
     * @brief Whether the underlying's option is naked, from the underlying's NakedOption. Null when
     * the document states none.
     */
    std::optional<bool> naked_option;

    /**
     * @brief Username of the person who last modified this risk participation agreement instrument.
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
    friend bool operator==(const risk_participation_agreement_instrument&,
                           const risk_participation_agreement_instrument&) = default;
};

/**
 * @brief The members of risk_participation_agreement_instrument the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view risk_participation_agreement_instrument_economic_fields[] = {
    "credit_curve_id",
    "issuer_id",
    "recovery_rate",
    "participation_rate",
    "settles_accrual",
    "naked_option",
};

/**
 * @brief Names the members of risk_participation_agreement_instrument that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<risk_participation_agreement_instrument>) {
    return risk_participation_agreement_instrument_economic_fields;
}

/**
 * @brief Dispatch-key identifier for risk_participation_agreement_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const risk_participation_agreement_instrument&) {
    return "ores.trading.risk_participation_agreement_instrument";
}

}

#endif
