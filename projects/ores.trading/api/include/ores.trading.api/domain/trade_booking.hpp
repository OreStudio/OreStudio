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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_BOOKING_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_BOOKING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Where a trade is booked and the netting set it is filed in.
 *
 * A trade's booking: the book that holds it, the netting set it is filed
 * in, the date it was agreed and the time it was executed. It is a
 * component of the [[id:4304A441-E532-45FB-837A-378F13693CAE][trade anchor]], keyed by the trade
 * id, and versions on its own timeline: a book move or a re-papering into another netting set cuts
 * a new version of the booking and leaves the anchor alone.
 *
 * The booking copies the anchor's party and counterparty. The copies are
 * pinned to the anchor, so they cannot drift, and they are what the other
 * pins and row-level security read: the book must belong to the trade's
 * party, and the netting set to its counterparty and party.
 *
 * A booking names a virtual book, one inside a sandbox, only while its
 * trade is a pre-agreement draft, a test or a hypothetical. An actual trade
 * going live needs a real book: the booking is written before the state
 * that takes the trade live, in the same transaction.
 */
struct trade_booking final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this booking belongs to.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The book that holds the trade.
     */
    boost::uuids::uuid book_id;

    /**
     * @brief The netting set the trade is filed in.
     */
    std::optional<boost::uuids::uuid> netting_set_id;

    /**
     * @brief The identifier the trade's source named its counterparty by.
     *
     * An ORE document names a counterparty by an alias, and two aliases may name one counterparty,
     * as CP and CPTY do in the ORE samples. The import keeps the identifier it resolved, so an
     * export writes back the name the document used.
     */
    std::optional<boost::uuids::uuid> counterparty_identifier_id;

    /**
     * @brief The identifier the trade's source named its netting set by, kept for the same reason
     * as the counterparty's.
     */
    std::optional<boost::uuids::uuid> netting_set_identifier_id;

    /**
     * @brief The date the trade was agreed. Absent when the source does not state it, as an ORE
     * document does not.
     */
    std::optional<std::chrono::year_month_day> trade_date;

    /**
     * @brief The time the trade was executed.
     */
    std::optional<std::chrono::system_clock::time_point> execution_timestamp;

    /**
     * @brief Username of the person who last modified this trade booking.
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
    friend bool operator==(const trade_booking&, const trade_booking&) = default;
};

/**
 * @brief Dispatch-key identifier for trade_booking, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const trade_booking&) {
    return "ores.trading.trade_booking";
}

}

#endif
