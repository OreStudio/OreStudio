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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_payload.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.api/domain/trade_instrument.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct trade_key {
    std::string external_id;
};

struct trade_write {
    boost::uuids::uuid id;
    std::string external_id;
    boost::uuids::uuid book_id;
    boost::uuids::uuid portfolio_id;
    std::optional<boost::uuids::uuid> successor_trade_id;
    std::string trade_type;
    std::optional<boost::uuids::uuid> counterparty_id;
    std::string product_type;
    std::optional<std::string> asset_class;
    std::string netting_set_id;
    std::string activity_type_code;
    boost::uuids::uuid status_id;
    std::string trade_date;
    std::string execution_timestamp;
    std::string effective_date;
    std::string termination_date;
};

struct trade_change {
    trade_write write;
    ores::utility::domain::precondition precondition;
};

struct trade_removal {
    trade_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct trade_lookup {
    trade_key key;
    std::optional<ores::trading::domain::trade> trade;
};

struct trades_filter {
    std::optional<std::string> node_id;
};

struct trade_event {
    boost::uuids::uuid event_id;
    trade_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct trade_version_key {
    trade_key trade;
    std::uint32_t version;
};

struct trade_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_trades_request {
    using response_type = struct list_trades_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trades_filter> filter;
};

struct list_trades_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade> trades;
    std::uint64_t total;
};

struct get_trade_request {
    using response_type = struct get_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_key key;
};

struct get_trade_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade> trade;
};

struct get_many_trades_request {
    using response_type = struct get_many_trades_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_key> keys;
};

struct get_many_trades_response {
    ores::utility::domain::result result;
    std::vector<trade_lookup> entries;
};

struct put_trade_request {
    using response_type = struct put_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_change change;
    ores::utility::domain::change_intent intent;
};

struct put_trade_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade> trade;
};

struct put_many_trades_request {
    using response_type = struct put_many_trades_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_trades_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade> trades;
};

struct delete_trade_request {
    using response_type = struct delete_trade_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_trade_response {
    ores::utility::domain::result result;
};

struct delete_many_trades_request {
    using response_type = struct delete_many_trades_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_trades_response {
    ores::utility::domain::result result;
};

struct list_trade_versions_request {
    using response_type = struct list_trade_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_versions_filter> filter;
};

struct list_trade_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade> versions;
    std::uint64_t total;
};

struct get_trade_version_request {
    using response_type = struct get_trade_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_version_key key;
};

struct get_trade_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_event_subjects {
inline constexpr std::string_view created = "trading.v1.trades_events.created";
inline constexpr std::string_view updated = "trading.v1.trades_events.updated";
inline constexpr std::string_view deleted = "trading.v1.trades_events.deleted";
}

/**
 * @brief One trade plus its resolved instrument data.
 *
 * The instrument is carried as a payload rather than as a trade_instrument
 * variant. reflect-cpp cannot name the active alternative of that variant:
 * untagged, the first alternative std::monostate parses from any payload and
 * wins; tagged, rfl::AddTagsToVariants exceeds the fold limits on macOS and
 * MSVC. See instrument_payload. The payload's type is empty when the trade has
 * no linked instrument or the product_type is unrecognised.
 *
 * The envelope holds the trade-level data the product tables do not: the
 * counterparty name, the netting set id, the portfolio id labels and the
 * document's additional fields. It is absent when the trade has none.
 */
struct trade_export_item {
    ores::trading::domain::trade trade;
    ores::trading::domain::instrument_payload instrument;
    std::optional<ores::trading::domain::trade_envelope_data> envelope;
};

struct get_trade_instrument_request {
    using response_type = struct get_trade_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.instrument";
    std::string trade_id;
};

struct get_trade_instrument_response {
    bool success = false;
    std::string message;
    ores::trading::domain::trade trade;
    ores::trading::domain::trade_instrument instrument;
};

/**
 * @brief Request to export all trades (and instruments) under a taxonomy node.
 *
 * @p node_id is resolved by the server to the book-id set just like
 * get_trades_request; typical callers supply a portfolio id (to export the
 * whole portfolio subtree) or a book id (to export a single book).
 */
struct export_portfolio_request {
    using response_type = struct export_portfolio_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.portfolio.export";
    std::string node_id;
    int offset = 0;
    int limit = 10000;
};

struct export_portfolio_response {
    bool success = false;
    std::string message;
    std::vector<trade_export_item> items;
};

/**
 * @brief Exports trades for the given book IDs to object storage.
 *
 * The handler resolves trade IDs via ores_trading_get_trade_ids_by_books_fn,
 * loads full trade_export_items, serialises to MsgPack, compresses with gzip,
 * and uploads to storage. Returns the storage key and trade count.
 *
 * Used by the report execution workflow to offload large trade data sets
 * to storage instead of passing them through NATS.
 */
struct export_trades_to_storage_request {
    using response_type = struct export_trades_to_storage_response;
    static constexpr std::string_view nats_subject = "trading.v1.trades.export-to-storage";

    std::vector<std::string> book_ids;
    // Target bucket: the platform bucket, "ores".
    std::string storage_bucket;
    // Target key, such as "reporting/runs/{instance_id}/trades.msgpack".
    std::string storage_key;
};

struct export_trades_to_storage_response {
    bool success = false;
    std::string message;
    int trade_count = 0;
    // Echoed back from the request so the caller can confirm the target.
    std::string storage_key;
};
}

#endif
