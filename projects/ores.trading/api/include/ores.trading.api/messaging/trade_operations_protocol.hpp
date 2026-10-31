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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_MESSAGING_TRADE_OPERATIONS_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_OPERATIONS_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.trading.api/domain/bond_issue_call_date.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
#include "ores.trading.api/domain/bond_issue_leg.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amortization.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amount.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_date.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

/**
 * @brief Books a trade: its anchor, its booking, its first state, its
 * identifiers and its instrument.
 *
 * The rows are written in one transaction. The booking's trade id,
 * party and counterparty are taken from the anchor, so the caller states
 * them once.
 */
struct book_trade_request {
    using response_type = struct book_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.book_trade";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The trade's immutable facts.
     */
    ores::trading::domain::trade anchor;
    /**
     * @brief Where the trade is booked. Its trade id, party and counterparty
     * are replaced by the anchor's.
     */
    ores::trading::domain::trade_booking booking;
    /**
     * @brief The activity that books the trade, which names the transition
     * that starts its state: new_booking for a live trade, draft_capture for a
     * draft.
     */
    std::string activity_type_code;
    /**
     * @brief The trade's identifiers. The service states the trade id and the
     * activity id on each one, so the caller states the scheme and the value.
     */
    std::vector<ores::trading::domain::trade_identifier> identifiers;
    /**
     * @brief The trade's instrument, as one array for each instrument table. The
     * service states the trade id, the trade type and the activity id on each
     * row. A row for a kind with no writer yet refuses the booking by name.
     */
    ores::trading::domain::instrument_batch instruments;
};

/**
 * @brief The outcome of booking a trade.
 */
struct book_trade_response {
    /**
     * @brief Outcome of the operation: conflict with code already_exists when
     * the trade id is already booked.
     */
    ores::utility::domain::result result;
    /**
     * @brief The activity that booked the trade; absent when the booking was
     * refused.
     */
    std::optional<boost::uuids::uuid> activity_id;
};

/**
 * @brief Creates a bond issue with its legs, in one transaction.
 *
 * The issue is the security master. It is whole or absent: a refused child
 * write leaves no issue row. The issue is create-only. An issue that already
 * exists is answered with a conflict and the stored issue stays as it is:
 * already_exists when its terms equal the request, content_differs when they
 * do not. Each child row names its issue and leg, and the caller states them
 * once on the row.
 */
struct put_bond_issue_composite_request {
    using response_type = struct put_bond_issue_composite_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.put_bond_issue_composite";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The security's terms.
     */
    ores::trading::domain::bond_issue issue;
    /**
     * @brief The issue's legs.
     */
    std::vector<ores::trading::domain::bond_issue_leg> legs;
    /**
     * @brief The amounts of the legs: notionals, coupons and the like.
     */
    std::vector<ores::trading::domain::bond_issue_leg_amount> amounts;
    /**
     * @brief The rate terms of the legs.
     */
    std::vector<ores::trading::domain::bond_issue_leg_rate> rates;
    /**
     * @brief The schedules of the legs.
     */
    std::vector<ores::trading::domain::bond_issue_leg_schedule> schedules;
    /**
     * @brief The dates of the schedules that state a date list.
     */
    std::vector<ores::trading::domain::bond_issue_leg_schedule_date> schedule_dates;
    /**
     * @brief The amortisation terms of the legs.
     */
    std::vector<ores::trading::domain::bond_issue_leg_amortization> amortizations;
    /**
     * @brief The dates on which the issuer may call the issue.
     */
    std::vector<ores::trading::domain::bond_issue_call_date> call_dates;
    /**
     * @brief The securities a convertible issue converts into.
     */
    std::vector<ores::trading::domain::bond_issue_conversion_target> conversion_targets;
};

/**
 * @brief The outcome of creating a bond issue.
 */
struct put_bond_issue_composite_response {
    /**
     * @brief Outcome of the operation: conflict with code already_exists when
     * the same issue is stored, and conflict with code content_differs when an
     * issue with this id or security id is stored with other terms.
     */
    ores::utility::domain::result result;
};

/**
 * @brief One trade's anchor, its ORE identifier and its envelope, as an
 * export writes it.
 *
 * The trade's instrument is not here. The export carries it in the batch's
 * typed array for the table the trade type routes to, and every element of
 * that array names the trade id this item states, so a reader joins the two
 * without an element tag.
 */
struct trade_export_item {
    /**
     * @brief The trade's immutable facts.
     */
    ores::trading::domain::trade anchor;
    /**
     * @brief The id the package names the trade by, which is the trade's own UUID.
     */
    std::string ore_id;
    /**
     * @brief The ids of the trade's counterparty, netting set and portfolios, and the additional
     * fields: the names and codes of those entities as context, beside any field the trade's source
     * recorded. Absent when the trade has none.
     */
    std::optional<ores::trading::domain::trade_envelope_data> envelope;
};

/**
 * @brief Exports the trades under a taxonomy node.
 *
 * The node is a book, a portfolio or a business unit, resolved to its books on
 * the server; the trades are those booked in them.
 */
struct export_portfolio_request {
    using response_type = struct export_portfolio_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.export_portfolio";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The book, portfolio or business unit to export.
     */
    std::string node_id;
    /**
     * @brief The first trade to export, in trade id order.
     */
    int offset = 0;
    /**
     * @brief The most trades to export.
     */
    int limit = 10000;
};

/**
 * @brief The exported trades.
 */
struct export_portfolio_response {
    /**
     * @brief Whether the export ran.
     */
    bool success = false;
    /**
     * @brief Why the export failed, when it did.
     */
    std::string message;
    /**
     * @brief The trades, in trade id order.
     */
    std::vector<trade_export_item> items;
    /**
     * @brief The trades' instruments, one typed array per instrument entity and
     * per child table, every element keyed by its trade id.
     *
     * An instrument is not a field of the item above: a std::variant cannot cross
     * the wire, because reflect-cpp names no alternative, and an encoded payload
     * would be an untyped field. So the tag sits on the container: a reader takes
     * the trade's type from the anchor, routes it with instrument_table_for, and
     * joins the array that table names by the trade id.
     */
    ores::trading::domain::instrument_batch instruments;
};

/**
 * @brief Exports the trades booked in a set of books to object storage.
 *
 * The handler serialises the export items to MsgPack, compresses them with
 * gzip and uploads them, so a report run passes a storage key rather than the
 * trades through NATS.
 */
struct export_trades_to_storage_request {
    using response_type = struct export_trades_to_storage_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.export_trades_to_storage";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The books whose trades to export.
     */
    std::vector<std::string> book_ids;
    /**
     * @brief The target bucket: the platform bucket, ores.
     */
    std::string storage_bucket;
    /**
     * @brief The target key, such as reporting/runs/{instance_id}/trades.msgpack.
     */
    std::string storage_key;
};

/**
 * @brief The outcome of an export to storage.
 */
struct export_trades_to_storage_response {
    /**
     * @brief Whether the export ran.
     */
    bool success = false;
    /**
     * @brief Why the export failed, when it did.
     */
    std::string message;
    /**
     * @brief How many trades were written.
     */
    int trade_count = 0;
    /**
     * @brief The key written, echoed from the request.
     */
    std::string storage_key;
    /**
     * @brief The version of each trade written, keyed by trade id.
     *
     * The blob holds the trades, so a reader of it needs no manifest to read them.
     * This is for the caller that holds a trade id and has to say which version of
     * that trade the export read: a report run writes the errors its engine raises,
     * and an error about a trade names the version the engine was given.
     */
    std::map<std::string, int> trade_versions;
};

}

#endif
