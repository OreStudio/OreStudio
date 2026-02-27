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
#include "ores.trading/messaging/trade_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::trading::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Trade helpers
// ============================================================================

void write_trade(std::vector<std::byte>& buffer,
    const domain::trade& tr) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(tr.version));
    writer::write_uuid(buffer, tr.id);
    writer::write_uuid(buffer, tr.party_id);
    writer::write_string(buffer, tr.external_id);
    writer::write_uuid(buffer, tr.book_id);
    writer::write_uuid(buffer, tr.portfolio_id);
    writer::write_bool(buffer, tr.successor_trade_id.has_value());
    if (tr.successor_trade_id.has_value()) {
        writer::write_uuid(buffer, *tr.successor_trade_id);
    }
    writer::write_bool(buffer, tr.counterparty_id.has_value());
    if (tr.counterparty_id.has_value()) {
        writer::write_uuid(buffer, *tr.counterparty_id);
    }
    writer::write_string(buffer, tr.trade_type);
    writer::write_string(buffer, tr.netting_set_id);
    writer::write_string(buffer, tr.lifecycle_event);
    writer::write_string(buffer, tr.trade_date);
    writer::write_string(buffer, tr.execution_timestamp);
    writer::write_string(buffer, tr.effective_date);
    writer::write_string(buffer, tr.termination_date);
    writer::write_string(buffer, tr.modified_by);
    writer::write_string(buffer, tr.performed_by);
    writer::write_string(buffer, tr.change_reason_code);
    writer::write_string(buffer, tr.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(tr.recorded_at));
}

std::expected<domain::trade, error_code>
read_trade(std::span<const std::byte>& data) {
    domain::trade tr;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    tr.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    tr.id = *id_result;

    auto party_id_result = reader::read_uuid(data);
    if (!party_id_result) return std::unexpected(party_id_result.error());
    tr.party_id = *party_id_result;

    auto external_id_result = reader::read_string(data);
    if (!external_id_result) return std::unexpected(external_id_result.error());
    tr.external_id = *external_id_result;

    auto book_id_result = reader::read_uuid(data);
    if (!book_id_result) return std::unexpected(book_id_result.error());
    tr.book_id = *book_id_result;

    auto portfolio_id_result = reader::read_uuid(data);
    if (!portfolio_id_result) return std::unexpected(portfolio_id_result.error());
    tr.portfolio_id = *portfolio_id_result;

    auto successor_trade_id_present_result = reader::read_bool(data);
    if (!successor_trade_id_present_result) return std::unexpected(successor_trade_id_present_result.error());
    if (*successor_trade_id_present_result) {
        auto successor_trade_id_result = reader::read_uuid(data);
        if (!successor_trade_id_result) return std::unexpected(successor_trade_id_result.error());
        tr.successor_trade_id = *successor_trade_id_result;
    }

    auto counterparty_id_present_result = reader::read_bool(data);
    if (!counterparty_id_present_result) return std::unexpected(counterparty_id_present_result.error());
    if (*counterparty_id_present_result) {
        auto counterparty_id_result = reader::read_uuid(data);
        if (!counterparty_id_result) return std::unexpected(counterparty_id_result.error());
        tr.counterparty_id = *counterparty_id_result;
    }

    auto trade_type_result = reader::read_string(data);
    if (!trade_type_result) return std::unexpected(trade_type_result.error());
    tr.trade_type = *trade_type_result;

    auto netting_set_id_result = reader::read_string(data);
    if (!netting_set_id_result) return std::unexpected(netting_set_id_result.error());
    tr.netting_set_id = *netting_set_id_result;

    auto lifecycle_event_result = reader::read_string(data);
    if (!lifecycle_event_result) return std::unexpected(lifecycle_event_result.error());
    tr.lifecycle_event = *lifecycle_event_result;

    auto trade_date_result = reader::read_string(data);
    if (!trade_date_result) return std::unexpected(trade_date_result.error());
    tr.trade_date = *trade_date_result;

    auto execution_timestamp_result = reader::read_string(data);
    if (!execution_timestamp_result) return std::unexpected(execution_timestamp_result.error());
    tr.execution_timestamp = *execution_timestamp_result;

    auto effective_date_result = reader::read_string(data);
    if (!effective_date_result) return std::unexpected(effective_date_result.error());
    tr.effective_date = *effective_date_result;

    auto termination_date_result = reader::read_string(data);
    if (!termination_date_result) return std::unexpected(termination_date_result.error());
    tr.termination_date = *termination_date_result;

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    tr.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    tr.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    tr.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    tr.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        tr.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return tr;
}

} // anonymous namespace

// ============================================================================
// Trade Messages Implementation
// ============================================================================

std::vector<std::byte> get_trades_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    writer::write_bool(buffer, book_id.has_value());
    if (book_id) writer::write_uuid(buffer, *book_id);
    writer::write_bool(buffer, portfolio_id.has_value());
    if (portfolio_id) writer::write_uuid(buffer, *portfolio_id);
    writer::write_bool(buffer, business_unit_id.has_value());
    if (business_unit_id) writer::write_uuid(buffer, *business_unit_id);
    return buffer;
}

std::expected<get_trades_request, error_code>
get_trades_request::deserialize(std::span<const std::byte> data) {
    get_trades_request request;

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    if (!data.empty()) {
        auto has_book = reader::read_bool(data);
        if (!has_book) return std::unexpected(has_book.error());
        if (*has_book) {
            auto bid = reader::read_uuid(data);
            if (!bid) return std::unexpected(bid.error());
            request.book_id = *bid;
        }

        if (!data.empty()) {
            auto has_port = reader::read_bool(data);
            if (!has_port) return std::unexpected(has_port.error());
            if (*has_port) {
                auto pid = reader::read_uuid(data);
                if (!pid) return std::unexpected(pid.error());
                request.portfolio_id = *pid;
            }
        }

        if (!data.empty()) {
            auto has_bu = reader::read_bool(data);
            if (!has_bu) return std::unexpected(has_bu.error());
            if (*has_bu) {
                auto buid = reader::read_uuid(data);
                if (!buid) return std::unexpected(buid.error());
                request.business_unit_id = *buid;
            }
        }
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_trades_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_trades_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, total_available_count);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(trades.size()));
    for (const auto& tr : trades) {
        write_trade(buffer, tr);
    }
    return buffer;
}

std::expected<get_trades_response, error_code>
get_trades_response::deserialize(std::span<const std::byte> data) {
    get_trades_response response;

    auto total_result = reader::read_uint32(data);
    if (!total_result) return std::unexpected(total_result.error());
    response.total_available_count = *total_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.trades.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_trade(data);
        if (!result) return std::unexpected(result.error());
        response.trades.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_trades_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_trade_request::serialize() const {
    std::vector<std::byte> buffer;
    write_trade(buffer, trade);
    return buffer;
}

std::expected<save_trade_request, error_code>
save_trade_request::deserialize(std::span<const std::byte> data) {
    save_trade_request request;

    auto result = read_trade(data);
    if (!result) return std::unexpected(result.error());
    request.trade = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_trade_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_trade_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_trade_response, error_code>
save_trade_response::deserialize(std::span<const std::byte> data) {
    save_trade_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_trade_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_trade_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_trade_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_trade_request, error_code>
delete_trade_request::deserialize(std::span<const std::byte> data) {
    delete_trade_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_trade_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_trade_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_trade_response, error_code>
delete_trade_response::deserialize(std::span<const std::byte> data) {
    delete_trade_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_trade_result r;

        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        r.id = *id_result;

        auto success_result = reader::read_bool(data);
        if (!success_result) return std::unexpected(success_result.error());
        r.success = *success_result;

        auto message_result = reader::read_string(data);
        if (!message_result) return std::unexpected(message_result.error());
        r.message = *message_result;

        response.results.push_back(std::move(r));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_trade_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_trade_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_trade_history_request, error_code>
get_trade_history_request::deserialize(std::span<const std::byte> data) {
    get_trade_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_trade_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_trade_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_trade(buffer, v);
    }
    return buffer;
}

std::expected<get_trade_history_response, error_code>
get_trade_history_response::deserialize(std::span<const std::byte> data) {
    get_trade_history_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.versions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_trade(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_trade_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
