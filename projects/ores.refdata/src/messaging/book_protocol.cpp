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
#include "ores.refdata/messaging/book_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::refdata::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Book helpers
// ============================================================================

void write_book(std::vector<std::byte>& buffer,
    const domain::book& bk) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(bk.version));
    writer::write_uuid(buffer, bk.id);
    writer::write_uuid(buffer, bk.party_id);
    writer::write_string(buffer, bk.name);
    writer::write_string(buffer, bk.description);
    writer::write_uuid(buffer, bk.parent_portfolio_id);
    writer::write_string(buffer, bk.ledger_ccy);
    writer::write_string(buffer, bk.gl_account_ref);
    writer::write_string(buffer, bk.cost_center);
    writer::write_string(buffer, bk.book_status);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(bk.is_trading_book));
    writer::write_string(buffer, bk.modified_by);
    writer::write_string(buffer, bk.performed_by);
    writer::write_string(buffer, bk.change_reason_code);
    writer::write_string(buffer, bk.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(bk.recorded_at));
}

std::expected<domain::book, error_code>
read_book(std::span<const std::byte>& data) {
    domain::book bk;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    bk.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    bk.id = *id_result;

    auto party_id_result = reader::read_uuid(data);
    if (!party_id_result) return std::unexpected(party_id_result.error());
    bk.party_id = *party_id_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    bk.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    bk.description = *description_result;

    auto parent_portfolio_id_result = reader::read_uuid(data);
    if (!parent_portfolio_id_result) return std::unexpected(parent_portfolio_id_result.error());
    bk.parent_portfolio_id = *parent_portfolio_id_result;

    auto ledger_ccy_result = reader::read_string(data);
    if (!ledger_ccy_result) return std::unexpected(ledger_ccy_result.error());
    bk.ledger_ccy = *ledger_ccy_result;

    auto gl_account_ref_result = reader::read_string(data);
    if (!gl_account_ref_result) return std::unexpected(gl_account_ref_result.error());
    bk.gl_account_ref = *gl_account_ref_result;

    auto cost_center_result = reader::read_string(data);
    if (!cost_center_result) return std::unexpected(cost_center_result.error());
    bk.cost_center = *cost_center_result;

    auto book_status_result = reader::read_string(data);
    if (!book_status_result) return std::unexpected(book_status_result.error());
    bk.book_status = *book_status_result;

    auto is_trading_book_result = reader::read_uint32(data);
    if (!is_trading_book_result) return std::unexpected(is_trading_book_result.error());
    bk.is_trading_book = static_cast<int>(*is_trading_book_result);

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    bk.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    bk.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    bk.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    bk.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        bk.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return bk;
}

} // anonymous namespace

// ============================================================================
// Book Messages Implementation
// ============================================================================

std::vector<std::byte> get_books_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_books_request, error_code>
get_books_request::deserialize(std::span<const std::byte> data) {
    get_books_request request;

    // Backward compatibility: empty payload from old clients uses defaults
    if (data.empty()) {
        return request;
    }

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_books_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_books_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write total available count
    writer::write_uint32(buffer, total_available_count);

    // Write book count in this response
    writer::write_uint32(buffer, static_cast<std::uint32_t>(books.size()));
    for (const auto& bk : books) {
        write_book(buffer, bk);
    }
    return buffer;
}

std::expected<get_books_response, error_code>
get_books_response::deserialize(std::span<const std::byte> data) {
    get_books_response response;

    // Read total available count
    auto total_result = reader::read_uint32(data);
    if (!total_result) return std::unexpected(total_result.error());
    response.total_available_count = *total_result;

    // Read book count in this response
    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.books.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_book(data);
        if (!result) return std::unexpected(result.error());
        response.books.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_books_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_book_request::serialize() const {
    std::vector<std::byte> buffer;
    write_book(buffer, book);
    return buffer;
}

std::expected<save_book_request, error_code>
save_book_request::deserialize(std::span<const std::byte> data) {
    save_book_request request;

    auto result = read_book(data);
    if (!result) return std::unexpected(result.error());
    request.book = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_book_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_book_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_book_response, error_code>
save_book_response::deserialize(std::span<const std::byte> data) {
    save_book_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_book_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_book_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_book_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_book_request, error_code>
delete_book_request::deserialize(std::span<const std::byte> data) {
    delete_book_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_book_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_book_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_book_response, error_code>
delete_book_response::deserialize(std::span<const std::byte> data) {
    delete_book_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_book_result r;

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

std::ostream& operator<<(std::ostream& s, const delete_book_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_book_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_book_history_request, error_code>
get_book_history_request::deserialize(std::span<const std::byte> data) {
    get_book_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_book_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_book_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_book(buffer, v);
    }
    return buffer;
}

std::expected<get_book_history_response, error_code>
get_book_history_response::deserialize(std::span<const std::byte> data) {
    get_book_history_response response;

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
        auto result = read_book(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_book_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
