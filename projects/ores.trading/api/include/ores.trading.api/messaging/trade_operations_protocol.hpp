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
#include "ores.trading.api/domain/structure.hpp"
#include "ores.trading.api/domain/structure_member.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <cstddef>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
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
     * @brief Why the trade is a draft, when the activity books one. Empty for a
     * booking that goes live, because a concluded deal waits on nothing.
     */
    std::string draft_reason_code;
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
 * @brief Takes a draft trade live: the activity that executes it, and the
 * state that activity moves it to.
 *
 * The activity, the state and the authorisation request join one
 * transaction, so an actual trade is never live without a request.
 */
struct execute_trade_request {
    using response_type = struct execute_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.execute_trade";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The draft to execute.
     */
    boost::uuids::uuid trade_id;
    /**
     * @brief The version the caller read. A draft that moved since is refused
     * rather than executed on terms nobody read.
     */
    int expected_version;
    /**
     * @brief When the deal executed; the clock when absent.
     */
    std::optional<std::chrono::system_clock::time_point> occurred_at;
    /**
     * @brief Why the draft was executed.
     */
    std::string change_reason_code;
    /**
     * @brief The note that goes with the reason.
     */
    std::string change_commentary;
};

/**
 * @brief The outcome of executing a draft.
 */
struct execute_trade_response {
    /**
     * @brief Outcome of the operation: conflict when the trade is not a draft or
     * has moved since the caller read it, not found when no such trade exists.
     */
    ores::utility::domain::result result;
    /**
     * @brief The activity that executed the trade; absent when it was refused.
     */
    std::optional<boost::uuids::uuid> activity_id;
};

/**
 * @brief Completes a structure: its anchor, its legs, and the execution of
 * every leg, in one transaction.
 *
 * The legs are drafts when the request arrives. A completion that is refused
 * leaves every one of them a draft and writes no member, which is why the
 * structure, the members and the executions are one unit of work rather than
 * a sequence a caller drives.
 */
struct complete_structure_request {
    using response_type = struct complete_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.complete_structure";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The structure to write. The service names it, so the caller states
     * the party, the kind and the template, and no reference.
     */
    ores::trading::domain::structure structure;
    /**
     * @brief The legs the structure holds, in the order they are given. The
     * service states the structure id and the sequence number on each one.
     */
    std::vector<ores::trading::domain::structure_member> legs;
    /**
     * @brief How many legs the caller means to complete. A request whose list is
     * not this long is refused, so a truncated list never completes a structure
     * that is missing a leg.
     */
    int leg_count;
    /**
     * @brief Why the structure is being completed.
     */
    std::string change_reason_code;
    /**
     * @brief The note that goes with the reason.
     */
    std::string change_commentary;
};

/**
 * @brief The outcome of completing a structure.
 */
struct complete_structure_response {
    /**
     * @brief Outcome of the operation: conflict with code already_exists when the
     * structure is already made, conflict with code not_a_draft when a leg is not
     * a draft, not found when a leg is not booked.
     */
    ores::utility::domain::result result;
    /**
     * @brief The structure written; absent when the completion was refused.
     */
    std::optional<boost::uuids::uuid> structure_id;
    /**
     * @brief The version the structure is at after the write, so a caller can
     * address the structure it just made.
     */
    int structure_version;
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

/**
 * @brief The members of book_trade_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_trade_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"anchor", &book_trade_request::anchor},
        ::ores::utility::codec::field{"booking", &book_trade_request::booking},
        ::ores::utility::codec::field{"activity_type_code",
                                      &book_trade_request::activity_type_code},
        ::ores::utility::codec::field{"draft_reason_code", &book_trade_request::draft_reason_code},
        ::ores::utility::codec::field{"identifiers", &book_trade_request::identifiers},
        ::ores::utility::codec::field{"instruments", &book_trade_request::instruments},
    };
}

/**
 * @brief The members of book_trade_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_trade_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &book_trade_response::result},
        ::ores::utility::codec::field{"activity_id", &book_trade_response::activity_id},
    };
}

/**
 * @brief The members of execute_trade_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const execute_trade_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"trade_id", &execute_trade_request::trade_id},
        ::ores::utility::codec::field{"expected_version", &execute_trade_request::expected_version},
        ::ores::utility::codec::field{"occurred_at", &execute_trade_request::occurred_at},
        ::ores::utility::codec::field{"change_reason_code",
                                      &execute_trade_request::change_reason_code},
        ::ores::utility::codec::field{"change_commentary",
                                      &execute_trade_request::change_commentary},
    };
}

/**
 * @brief The members of execute_trade_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const execute_trade_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &execute_trade_response::result},
        ::ores::utility::codec::field{"activity_id", &execute_trade_response::activity_id},
    };
}

/**
 * @brief The members of complete_structure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const complete_structure_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"structure", &complete_structure_request::structure},
        ::ores::utility::codec::field{"legs", &complete_structure_request::legs},
        ::ores::utility::codec::field{"leg_count", &complete_structure_request::leg_count},
        ::ores::utility::codec::field{"change_reason_code",
                                      &complete_structure_request::change_reason_code},
        ::ores::utility::codec::field{"change_commentary",
                                      &complete_structure_request::change_commentary},
    };
}

/**
 * @brief The members of complete_structure_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const complete_structure_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &complete_structure_response::result},
        ::ores::utility::codec::field{"structure_id", &complete_structure_response::structure_id},
        ::ores::utility::codec::field{"structure_version",
                                      &complete_structure_response::structure_version},
    };
}

/**
 * @brief The members of put_bond_issue_composite_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_issue_composite_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"issue", &put_bond_issue_composite_request::issue},
        ::ores::utility::codec::field{"legs", &put_bond_issue_composite_request::legs},
        ::ores::utility::codec::field{"amounts", &put_bond_issue_composite_request::amounts},
        ::ores::utility::codec::field{"rates", &put_bond_issue_composite_request::rates},
        ::ores::utility::codec::field{"schedules", &put_bond_issue_composite_request::schedules},
        ::ores::utility::codec::field{"schedule_dates",
                                      &put_bond_issue_composite_request::schedule_dates},
        ::ores::utility::codec::field{"amortizations",
                                      &put_bond_issue_composite_request::amortizations},
        ::ores::utility::codec::field{"call_dates", &put_bond_issue_composite_request::call_dates},
        ::ores::utility::codec::field{"conversion_targets",
                                      &put_bond_issue_composite_request::conversion_targets},
    };
}

/**
 * @brief The members of put_bond_issue_composite_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_issue_composite_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &put_bond_issue_composite_response::result},
    };
}

/**
 * @brief The members of trade_export_item, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_export_item&) {
    return std::tuple{
        ::ores::utility::codec::field{"anchor", &trade_export_item::anchor},
        ::ores::utility::codec::field{"ore_id", &trade_export_item::ore_id},
        ::ores::utility::codec::field{"envelope", &trade_export_item::envelope},
    };
}

/**
 * @brief The members of export_portfolio_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const export_portfolio_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"node_id", &export_portfolio_request::node_id},
        ::ores::utility::codec::field{"offset", &export_portfolio_request::offset},
        ::ores::utility::codec::field{"limit", &export_portfolio_request::limit},
    };
}

/**
 * @brief The members of export_portfolio_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const export_portfolio_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &export_portfolio_response::success},
        ::ores::utility::codec::field{"message", &export_portfolio_response::message},
        ::ores::utility::codec::field{"items", &export_portfolio_response::items},
        ::ores::utility::codec::field{"instruments", &export_portfolio_response::instruments},
    };
}

/**
 * @brief The members of export_trades_to_storage_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const export_trades_to_storage_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"book_ids", &export_trades_to_storage_request::book_ids},
        ::ores::utility::codec::field{"storage_bucket",
                                      &export_trades_to_storage_request::storage_bucket},
        ::ores::utility::codec::field{"storage_key",
                                      &export_trades_to_storage_request::storage_key},
    };
}

/**
 * @brief The members of export_trades_to_storage_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const export_trades_to_storage_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &export_trades_to_storage_response::success},
        ::ores::utility::codec::field{"message", &export_trades_to_storage_response::message},
        ::ores::utility::codec::field{"trade_count",
                                      &export_trades_to_storage_response::trade_count},
        ::ores::utility::codec::field{"storage_key",
                                      &export_trades_to_storage_response::storage_key},
        ::ores::utility::codec::field{"trade_versions",
                                      &export_trades_to_storage_response::trade_versions},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::book_trade_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::book_trade_request& v);
    static ores::trading::messaging::book_trade_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::book_trade_request& v);
    static ores::trading::messaging::book_trade_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::book_trade_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::book_trade_response& v);
    static ores::trading::messaging::book_trade_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::book_trade_response& v);
    static ores::trading::messaging::book_trade_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::execute_trade_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::execute_trade_request& v);
    static ores::trading::messaging::execute_trade_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::execute_trade_request& v);
    static ores::trading::messaging::execute_trade_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::execute_trade_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::execute_trade_response& v);
    static ores::trading::messaging::execute_trade_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::execute_trade_response& v);
    static ores::trading::messaging::execute_trade_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::complete_structure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::complete_structure_request& v);
    static ores::trading::messaging::complete_structure_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::complete_structure_request& v);
    static ores::trading::messaging::complete_structure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::complete_structure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::complete_structure_response& v);
    static ores::trading::messaging::complete_structure_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::complete_structure_response& v);
    static ores::trading::messaging::complete_structure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_bond_issue_composite_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_bond_issue_composite_request& v);
    static ores::trading::messaging::put_bond_issue_composite_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_bond_issue_composite_request& v);
    static ores::trading::messaging::put_bond_issue_composite_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_bond_issue_composite_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_bond_issue_composite_response& v);
    static ores::trading::messaging::put_bond_issue_composite_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_bond_issue_composite_response& v);
    static ores::trading::messaging::put_bond_issue_composite_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_export_item> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_export_item& v);
    static ores::trading::messaging::trade_export_item read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_export_item& v);
    static ores::trading::messaging::trade_export_item
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::export_portfolio_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::export_portfolio_request& v);
    static ores::trading::messaging::export_portfolio_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::export_portfolio_request& v);
    static ores::trading::messaging::export_portfolio_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::export_portfolio_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::export_portfolio_response& v);
    static ores::trading::messaging::export_portfolio_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::export_portfolio_response& v);
    static ores::trading::messaging::export_portfolio_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::export_trades_to_storage_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::export_trades_to_storage_request& v);
    static ores::trading::messaging::export_trades_to_storage_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::export_trades_to_storage_request& v);
    static ores::trading::messaging::export_trades_to_storage_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::export_trades_to_storage_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::export_trades_to_storage_response& v);
    static ores::trading::messaging::export_trades_to_storage_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::export_trades_to_storage_response& v);
    static ores::trading::messaging::export_trades_to_storage_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
