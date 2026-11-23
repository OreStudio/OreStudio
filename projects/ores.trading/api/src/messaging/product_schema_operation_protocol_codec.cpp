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
#include "ores.trading.api/domain/script_input_code_codec.hpp"
#include "ores.trading.api/domain/script_input_codec.hpp"
#include "ores.trading.api/messaging/product_schema_operation_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::product_field>::write_json(
    const ores::trading::messaging::product_field& v) {
    return object::write_json(v);
}

ores::trading::messaging::product_field
wire_traits<ores::trading::messaging::product_field>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::product_field>(text);
}

std::string wire_traits<ores::trading::messaging::product_field>::write_msgpack(
    const ores::trading::messaging::product_field& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::product_field
wire_traits<ores::trading::messaging::product_field>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::product_field>(data);
}

std::string wire_traits<ores::trading::messaging::product_form>::write_json(
    const ores::trading::messaging::product_form& v) {
    return object::write_json(v);
}

ores::trading::messaging::product_form
wire_traits<ores::trading::messaging::product_form>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::product_form>(text);
}

std::string wire_traits<ores::trading::messaging::product_form>::write_msgpack(
    const ores::trading::messaging::product_form& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::product_form
wire_traits<ores::trading::messaging::product_form>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::product_form>(data);
}

std::string wire_traits<ores::trading::messaging::product_summary>::write_json(
    const ores::trading::messaging::product_summary& v) {
    return object::write_json(v);
}

ores::trading::messaging::product_summary
wire_traits<ores::trading::messaging::product_summary>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::product_summary>(text);
}

std::string wire_traits<ores::trading::messaging::product_summary>::write_msgpack(
    const ores::trading::messaging::product_summary& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::product_summary
wire_traits<ores::trading::messaging::product_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::product_summary>(data);
}

std::string wire_traits<ores::trading::messaging::describe_product_request>::write_json(
    const ores::trading::messaging::describe_product_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::describe_product_request
wire_traits<ores::trading::messaging::describe_product_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::describe_product_request>(text);
}

std::string wire_traits<ores::trading::messaging::describe_product_request>::write_msgpack(
    const ores::trading::messaging::describe_product_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::describe_product_request
wire_traits<ores::trading::messaging::describe_product_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::describe_product_request>(data);
}

std::string wire_traits<ores::trading::messaging::describe_product_response>::write_json(
    const ores::trading::messaging::describe_product_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::describe_product_response
wire_traits<ores::trading::messaging::describe_product_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::describe_product_response>(text);
}

std::string wire_traits<ores::trading::messaging::describe_product_response>::write_msgpack(
    const ores::trading::messaging::describe_product_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::describe_product_response
wire_traits<ores::trading::messaging::describe_product_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::describe_product_response>(data);
}

std::string wire_traits<ores::trading::messaging::list_products_request>::write_json(
    const ores::trading::messaging::list_products_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_products_request
wire_traits<ores::trading::messaging::list_products_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::list_products_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_products_request>::write_msgpack(
    const ores::trading::messaging::list_products_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_products_request
wire_traits<ores::trading::messaging::list_products_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_products_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_products_response>::write_json(
    const ores::trading::messaging::list_products_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_products_response
wire_traits<ores::trading::messaging::list_products_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::list_products_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_products_response>::write_msgpack(
    const ores::trading::messaging::list_products_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_products_response
wire_traits<ores::trading::messaging::list_products_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_products_response>(data);
}

}
