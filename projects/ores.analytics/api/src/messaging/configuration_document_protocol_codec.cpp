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
 * Template: cpp_protocol_codec.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.analytics.api/domain/pricing_model_config_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_product_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter_codec.hpp"
#include "ores.analytics.api/domain/todays_market_collection_codec.hpp"
#include "ores.analytics.api/domain/todays_market_config_codec.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_binding_codec.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_codec.hpp"
#include "ores.analytics.api/domain/todays_market_entry_codec.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::analytics::messaging::pricing_engines_document>::write_json(
    const ores::analytics::messaging::pricing_engines_document& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_engines_document
wire_traits<ores::analytics::messaging::pricing_engines_document>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_engines_document>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_engines_document>::write_msgpack(
    const ores::analytics::messaging::pricing_engines_document& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_engines_document
wire_traits<ores::analytics::messaging::pricing_engines_document>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_engines_document>(data);
}

std::string wire_traits<ores::analytics::messaging::todays_market_document>::write_json(
    const ores::analytics::messaging::todays_market_document& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_document
wire_traits<ores::analytics::messaging::todays_market_document>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::todays_market_document>(text);
}

std::string wire_traits<ores::analytics::messaging::todays_market_document>::write_msgpack(
    const ores::analytics::messaging::todays_market_document& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_document
wire_traits<ores::analytics::messaging::todays_market_document>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::todays_market_document>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_engines_document_problem>::write_json(
    const ores::analytics::messaging::pricing_engines_document_problem& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_engines_document_problem
wire_traits<ores::analytics::messaging::pricing_engines_document_problem>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_engines_document_problem>(text);
}

std::string
wire_traits<ores::analytics::messaging::pricing_engines_document_problem>::write_msgpack(
    const ores::analytics::messaging::pricing_engines_document_problem& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_engines_document_problem
wire_traits<ores::analytics::messaging::pricing_engines_document_problem>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_engines_document_problem>(data);
}

std::string
wire_traits<ores::analytics::messaging::save_pricing_engines_document_request>::write_json(
    const ores::analytics::messaging::save_pricing_engines_document_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::save_pricing_engines_document_request
wire_traits<ores::analytics::messaging::save_pricing_engines_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::save_pricing_engines_document_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::save_pricing_engines_document_request>::write_msgpack(
    const ores::analytics::messaging::save_pricing_engines_document_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::save_pricing_engines_document_request
wire_traits<ores::analytics::messaging::save_pricing_engines_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::save_pricing_engines_document_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::save_pricing_engines_document_response>::write_json(
    const ores::analytics::messaging::save_pricing_engines_document_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::save_pricing_engines_document_response
wire_traits<ores::analytics::messaging::save_pricing_engines_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::save_pricing_engines_document_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::save_pricing_engines_document_response>::write_msgpack(
    const ores::analytics::messaging::save_pricing_engines_document_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::save_pricing_engines_document_response
wire_traits<ores::analytics::messaging::save_pricing_engines_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::save_pricing_engines_document_response>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::get_pricing_engines_document_request>::write_json(
    const ores::analytics::messaging::get_pricing_engines_document_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_pricing_engines_document_request
wire_traits<ores::analytics::messaging::get_pricing_engines_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_pricing_engines_document_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::get_pricing_engines_document_request>::write_msgpack(
    const ores::analytics::messaging::get_pricing_engines_document_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_pricing_engines_document_request
wire_traits<ores::analytics::messaging::get_pricing_engines_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_pricing_engines_document_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::get_pricing_engines_document_response>::write_json(
    const ores::analytics::messaging::get_pricing_engines_document_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_pricing_engines_document_response
wire_traits<ores::analytics::messaging::get_pricing_engines_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_pricing_engines_document_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::get_pricing_engines_document_response>::write_msgpack(
    const ores::analytics::messaging::get_pricing_engines_document_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_pricing_engines_document_response
wire_traits<ores::analytics::messaging::get_pricing_engines_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_pricing_engines_document_response>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::delete_pricing_engines_document_request>::write_json(
    const ores::analytics::messaging::delete_pricing_engines_document_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_pricing_engines_document_request
wire_traits<ores::analytics::messaging::delete_pricing_engines_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_pricing_engines_document_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::delete_pricing_engines_document_request>::write_msgpack(
    const ores::analytics::messaging::delete_pricing_engines_document_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_pricing_engines_document_request
wire_traits<ores::analytics::messaging::delete_pricing_engines_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::delete_pricing_engines_document_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::delete_pricing_engines_document_response>::write_json(
    const ores::analytics::messaging::delete_pricing_engines_document_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_pricing_engines_document_response
wire_traits<ores::analytics::messaging::delete_pricing_engines_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_pricing_engines_document_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::delete_pricing_engines_document_response>::write_msgpack(
    const ores::analytics::messaging::delete_pricing_engines_document_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_pricing_engines_document_response
wire_traits<ores::analytics::messaging::delete_pricing_engines_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::delete_pricing_engines_document_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::save_todays_market_document_request>::write_json(
    const ores::analytics::messaging::save_todays_market_document_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::save_todays_market_document_request
wire_traits<ores::analytics::messaging::save_todays_market_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::save_todays_market_document_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::save_todays_market_document_request>::write_msgpack(
    const ores::analytics::messaging::save_todays_market_document_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::save_todays_market_document_request
wire_traits<ores::analytics::messaging::save_todays_market_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::save_todays_market_document_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::save_todays_market_document_response>::write_json(
    const ores::analytics::messaging::save_todays_market_document_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::save_todays_market_document_response
wire_traits<ores::analytics::messaging::save_todays_market_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::save_todays_market_document_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::save_todays_market_document_response>::write_msgpack(
    const ores::analytics::messaging::save_todays_market_document_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::save_todays_market_document_response
wire_traits<ores::analytics::messaging::save_todays_market_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::save_todays_market_document_response>(
        data);
}

std::string wire_traits<ores::analytics::messaging::get_todays_market_document_request>::write_json(
    const ores::analytics::messaging::get_todays_market_document_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_todays_market_document_request
wire_traits<ores::analytics::messaging::get_todays_market_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_todays_market_document_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_document_request>::write_msgpack(
    const ores::analytics::messaging::get_todays_market_document_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_todays_market_document_request
wire_traits<ores::analytics::messaging::get_todays_market_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_todays_market_document_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_document_response>::write_json(
    const ores::analytics::messaging::get_todays_market_document_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_todays_market_document_response
wire_traits<ores::analytics::messaging::get_todays_market_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_todays_market_document_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_document_response>::write_msgpack(
    const ores::analytics::messaging::get_todays_market_document_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_todays_market_document_response
wire_traits<ores::analytics::messaging::get_todays_market_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_todays_market_document_response>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::delete_todays_market_document_request>::write_json(
    const ores::analytics::messaging::delete_todays_market_document_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_todays_market_document_request
wire_traits<ores::analytics::messaging::delete_todays_market_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_todays_market_document_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::delete_todays_market_document_request>::write_msgpack(
    const ores::analytics::messaging::delete_todays_market_document_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_todays_market_document_request
wire_traits<ores::analytics::messaging::delete_todays_market_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_todays_market_document_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::delete_todays_market_document_response>::write_json(
    const ores::analytics::messaging::delete_todays_market_document_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_todays_market_document_response
wire_traits<ores::analytics::messaging::delete_todays_market_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_todays_market_document_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::delete_todays_market_document_response>::write_msgpack(
    const ores::analytics::messaging::delete_todays_market_document_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_todays_market_document_response
wire_traits<ores::analytics::messaging::delete_todays_market_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_todays_market_document_response>(
        data);
}

}
