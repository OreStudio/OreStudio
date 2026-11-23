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
#include "ores.trading.api/domain/script_calibration_codec.hpp"
#include "ores.trading.api/domain/script_calibration_strike_codec.hpp"
#include "ores.trading.api/domain/script_coarsening_codec.hpp"
#include "ores.trading.api/domain/script_codec.hpp"
#include "ores.trading.api/domain/script_result_codec.hpp"
#include "ores.trading.api/domain/script_variant_codec.hpp"
#include "ores.trading.api/domain/trade_booking_codec.hpp"
#include "ores.trading.api/domain/trade_codec.hpp"
#include "ores.trading.api/domain/trade_identifier_codec.hpp"
#include "ores.trading.api/messaging/script_library_operation_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::script_pin>::write_json(
    const ores::trading::messaging::script_pin& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_pin
wire_traits<ores::trading::messaging::script_pin>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_pin>(text);
}

std::string wire_traits<ores::trading::messaging::script_pin>::write_msgpack(
    const ores::trading::messaging::script_pin& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_pin
wire_traits<ores::trading::messaging::script_pin>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_pin>(data);
}

std::string wire_traits<ores::trading::messaging::export_script_library_request>::write_json(
    const ores::trading::messaging::export_script_library_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::export_script_library_request
wire_traits<ores::trading::messaging::export_script_library_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::export_script_library_request>(text);
}

std::string wire_traits<ores::trading::messaging::export_script_library_request>::write_msgpack(
    const ores::trading::messaging::export_script_library_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::export_script_library_request
wire_traits<ores::trading::messaging::export_script_library_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::export_script_library_request>(data);
}

std::string wire_traits<ores::trading::messaging::export_script_library_response>::write_json(
    const ores::trading::messaging::export_script_library_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::export_script_library_response
wire_traits<ores::trading::messaging::export_script_library_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::export_script_library_response>(text);
}

std::string wire_traits<ores::trading::messaging::export_script_library_response>::write_msgpack(
    const ores::trading::messaging::export_script_library_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::export_script_library_response
wire_traits<ores::trading::messaging::export_script_library_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::export_script_library_response>(data);
}

std::string wire_traits<ores::trading::messaging::published_result>::write_json(
    const ores::trading::messaging::published_result& v) {
    return object::write_json(v);
}

ores::trading::messaging::published_result
wire_traits<ores::trading::messaging::published_result>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::published_result>(text);
}

std::string wire_traits<ores::trading::messaging::published_result>::write_msgpack(
    const ores::trading::messaging::published_result& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::published_result
wire_traits<ores::trading::messaging::published_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::published_result>(data);
}

std::string wire_traits<ores::trading::messaging::published_calibration>::write_json(
    const ores::trading::messaging::published_calibration& v) {
    return object::write_json(v);
}

ores::trading::messaging::published_calibration
wire_traits<ores::trading::messaging::published_calibration>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::published_calibration>(text);
}

std::string wire_traits<ores::trading::messaging::published_calibration>::write_msgpack(
    const ores::trading::messaging::published_calibration& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::published_calibration
wire_traits<ores::trading::messaging::published_calibration>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::published_calibration>(data);
}

std::string wire_traits<ores::trading::messaging::published_variant>::write_json(
    const ores::trading::messaging::published_variant& v) {
    return object::write_json(v);
}

ores::trading::messaging::published_variant
wire_traits<ores::trading::messaging::published_variant>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::published_variant>(text);
}

std::string wire_traits<ores::trading::messaging::published_variant>::write_msgpack(
    const ores::trading::messaging::published_variant& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::published_variant
wire_traits<ores::trading::messaging::published_variant>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::published_variant>(data);
}

std::string wire_traits<ores::trading::messaging::script_diagnostic>::write_json(
    const ores::trading::messaging::script_diagnostic& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_diagnostic
wire_traits<ores::trading::messaging::script_diagnostic>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_diagnostic>(text);
}

std::string wire_traits<ores::trading::messaging::script_diagnostic>::write_msgpack(
    const ores::trading::messaging::script_diagnostic& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_diagnostic
wire_traits<ores::trading::messaging::script_diagnostic>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_diagnostic>(data);
}

std::string wire_traits<ores::trading::messaging::script_variable_value>::write_json(
    const ores::trading::messaging::script_variable_value& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_variable_value
wire_traits<ores::trading::messaging::script_variable_value>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_variable_value>(text);
}

std::string wire_traits<ores::trading::messaging::script_variable_value>::write_msgpack(
    const ores::trading::messaging::script_variable_value& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_variable_value
wire_traits<ores::trading::messaging::script_variable_value>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_variable_value>(data);
}

std::string wire_traits<ores::trading::messaging::validate_script_request>::write_json(
    const ores::trading::messaging::validate_script_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::validate_script_request
wire_traits<ores::trading::messaging::validate_script_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::validate_script_request>(text);
}

std::string wire_traits<ores::trading::messaging::validate_script_request>::write_msgpack(
    const ores::trading::messaging::validate_script_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::validate_script_request
wire_traits<ores::trading::messaging::validate_script_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::validate_script_request>(data);
}

std::string wire_traits<ores::trading::messaging::validate_script_response>::write_json(
    const ores::trading::messaging::validate_script_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::validate_script_response
wire_traits<ores::trading::messaging::validate_script_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::validate_script_response>(text);
}

std::string wire_traits<ores::trading::messaging::validate_script_response>::write_msgpack(
    const ores::trading::messaging::validate_script_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::validate_script_response
wire_traits<ores::trading::messaging::validate_script_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::validate_script_response>(data);
}

std::string wire_traits<ores::trading::messaging::book_scripted_request>::write_json(
    const ores::trading::messaging::book_scripted_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::book_scripted_request
wire_traits<ores::trading::messaging::book_scripted_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::book_scripted_request>(text);
}

std::string wire_traits<ores::trading::messaging::book_scripted_request>::write_msgpack(
    const ores::trading::messaging::book_scripted_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::book_scripted_request
wire_traits<ores::trading::messaging::book_scripted_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::book_scripted_request>(data);
}

std::string wire_traits<ores::trading::messaging::book_scripted_response>::write_json(
    const ores::trading::messaging::book_scripted_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::book_scripted_response
wire_traits<ores::trading::messaging::book_scripted_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::book_scripted_response>(text);
}

std::string wire_traits<ores::trading::messaging::book_scripted_response>::write_msgpack(
    const ores::trading::messaging::book_scripted_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::book_scripted_response
wire_traits<ores::trading::messaging::book_scripted_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::book_scripted_response>(data);
}

std::string wire_traits<ores::trading::messaging::publish_script_request>::write_json(
    const ores::trading::messaging::publish_script_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::publish_script_request
wire_traits<ores::trading::messaging::publish_script_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::publish_script_request>(text);
}

std::string wire_traits<ores::trading::messaging::publish_script_request>::write_msgpack(
    const ores::trading::messaging::publish_script_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::publish_script_request
wire_traits<ores::trading::messaging::publish_script_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::publish_script_request>(data);
}

std::string wire_traits<ores::trading::messaging::publish_script_response>::write_json(
    const ores::trading::messaging::publish_script_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::publish_script_response
wire_traits<ores::trading::messaging::publish_script_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::publish_script_response>(text);
}

std::string wire_traits<ores::trading::messaging::publish_script_response>::write_msgpack(
    const ores::trading::messaging::publish_script_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::publish_script_response
wire_traits<ores::trading::messaging::publish_script_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::publish_script_response>(data);
}

}
