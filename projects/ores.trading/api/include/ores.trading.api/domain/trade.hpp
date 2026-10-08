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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_HPP

#include "ores.trading.api/domain/booking_nature.hpp"
#include "ores.trading.api/domain/counterparty_scope.hpp"
#include "ores.trading.api/domain/entry_channel.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The immutable anchor of a trade.
 *
 * The immutable anchor of a trade. It holds only the facts that are fixed
 * for the trade's life: the firm's legal entity (party), the counterparty,
 * the trade type, and the three closed classifications — counterparty
 * scope, booking nature and entry channel. Everything that changes lives in
 * component tables keyed by the trade, each on its own timeline.
 *
 * A row is written once and never changed. A change of party or
 * counterparty is a cancel and rebook: a new trade, linked to the old one.
 * Because the row never changes, the component tables reference it with
 * database foreign keys instead of trigger checks against temporal rows.
 *
 * The anchor is built beside the wide trade table, which it replaces in
 * the last task of the story; until then it is named trade. It has
 * no audit tail: the audit record of the act that booked it carries that.
 */
struct trade final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade id: the firm's own identifier of the trade.
     */
    boost::uuids::uuid id;

    /**
     * @brief The firm's legal entity that is party to the trade.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The party the firm faces. Absent only on an intra-entity trade, which moves risk
     * between two books of one legal entity and faces nobody.
     */
    std::optional<boost::uuids::uuid> counterparty_id;

    /**
     * @brief ORE trade type code (e.g. Swap, FxForward, CapFloor).
     */
    std::string trade_type;

    /**
     * @brief Who the firm faces: external, inter_entity or intra_entity.
     */
    ores::trading::domain::counterparty_scope counterparty_scope =
        ores::trading::domain::counterparty_scope::external;

    /**
     * @brief Whether anything happened to cause the booking: actual, test or hypothetical.
     */
    ores::trading::domain::booking_nature booking_nature =
        ores::trading::domain::booking_nature::actual;

    /**
     * @brief How the trade reached the firm's books: manual, stp, ecn or allocation.
     */
    ores::trading::domain::entry_channel entry_channel =
        ores::trading::domain::entry_channel::manual;

    /**
     * @brief Username of the person who last modified this trade.
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
    friend bool operator==(const trade&, const trade&) = default;
};

/**
 * @brief Dispatch-key identifier for trade, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const trade&) {
    return "ores.trading.trade";
}

}

#endif
