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
#include "ores.refdata/messaging/currency_market_tier_protocol.hpp"

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
// Currency Market Tier helpers
// ============================================================================

void write_currency_market_tier(std::vector<std::byte>& buffer,
    const domain::currency_market_tier& rt) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(rt.version));
    writer::write_string(buffer, rt.code);
    writer::write_string(buffer, rt.name);
    writer::write_string(buffer, rt.description);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(rt.display_order));
    writer::write_string(buffer, rt.modified_by);
    writer::write_string(buffer, rt.performed_by);
    writer::write_string(buffer, rt.change_reason_code);
    writer::write_string(buffer, rt.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(rt.recorded_at));
}

std::expected<domain::currency_market_tier, error_code>
read_currency_market_tier(std::span<const std::byte>& data) {
    domain::currency_market_tier rt;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    rt.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    rt.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    rt.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    rt.description = *description_result;

    auto display_order_result = reader::read_uint32(data);
    if (!display_order_result) return std::unexpected(display_order_result.error());
    rt.display_order = static_cast<int>(*display_order_result);

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    rt.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    rt.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    rt.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    rt.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        rt.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return rt;
}

} // anonymous namespace

// ============================================================================
// Currency Market Tier Messages Implementation
// ============================================================================

std::vector<std::byte> get_currency_market_tiers_request::serialize() const {
    return {};
}

std::expected<get_currency_market_tiers_request, error_code>
get_currency_market_tiers_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_currency_market_tiers_request{};
}

std::ostream& operator<<(std::ostream& s, const get_currency_market_tiers_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_currency_market_tiers_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(types.size()));
    for (const auto& rt : types) {
        write_currency_market_tier(buffer, rt);
    }
    return buffer;
}

std::expected<get_currency_market_tiers_response, error_code>
get_currency_market_tiers_response::deserialize(std::span<const std::byte> data) {
    get_currency_market_tiers_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.types.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_currency_market_tier(data);
        if (!result) return std::unexpected(result.error());
        response.types.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_currency_market_tiers_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_currency_market_tier_request
save_currency_market_tier_request::from(domain::currency_market_tier type) {
    return save_currency_market_tier_request{std::vector<domain::currency_market_tier>{std::move(type)}};
}

save_currency_market_tier_request
save_currency_market_tier_request::from(std::vector<domain::currency_market_tier> types) {
    return save_currency_market_tier_request{std::move(types)};
}

std::vector<std::byte> save_currency_market_tier_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(types.size()));
    for (const auto& e : types)
        write_currency_market_tier(buffer, e);
    return buffer;
}

std::expected<save_currency_market_tier_request, error_code>
save_currency_market_tier_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result)
        return std::unexpected(count_result.error());

    save_currency_market_tier_request request;
    request.types.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_currency_market_tier(data);
        if (!e)
            return std::unexpected(e.error());
        request.types.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_currency_market_tier_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_currency_market_tier_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_currency_market_tier_response, error_code>
save_currency_market_tier_response::deserialize(std::span<const std::byte> data) {
    save_currency_market_tier_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_currency_market_tier_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_currency_market_tier_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_currency_market_tier_request, error_code>
delete_currency_market_tier_request::deserialize(std::span<const std::byte> data) {
    delete_currency_market_tier_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.codes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        request.codes.push_back(*code_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_currency_market_tier_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_currency_market_tier_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_currency_market_tier_response, error_code>
delete_currency_market_tier_response::deserialize(std::span<const std::byte> data) {
    delete_currency_market_tier_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_currency_market_tier_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
