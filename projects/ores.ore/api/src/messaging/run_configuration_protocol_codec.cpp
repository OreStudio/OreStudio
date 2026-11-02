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
#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::ore::messaging::run_input_file>::write_json(
    const ores::ore::messaging::run_input_file& v) {
    return object::write_json(v);
}

ores::ore::messaging::run_input_file
wire_traits<ores::ore::messaging::run_input_file>::read_json(std::string_view text) {
    return object::read_json<ores::ore::messaging::run_input_file>(text);
}

std::string wire_traits<ores::ore::messaging::run_input_file>::write_msgpack(
    const ores::ore::messaging::run_input_file& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::run_input_file
wire_traits<ores::ore::messaging::run_input_file>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::run_input_file>(data);
}

std::string wire_traits<ores::ore::messaging::saved_document>::write_json(
    const ores::ore::messaging::saved_document& v) {
    return object::write_json(v);
}

ores::ore::messaging::saved_document
wire_traits<ores::ore::messaging::saved_document>::read_json(std::string_view text) {
    return object::read_json<ores::ore::messaging::saved_document>(text);
}

std::string wire_traits<ores::ore::messaging::saved_document>::write_msgpack(
    const ores::ore::messaging::saved_document& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::saved_document
wire_traits<ores::ore::messaging::saved_document>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::saved_document>(data);
}

std::string wire_traits<ores::ore::messaging::import_run_configuration_request>::write_json(
    const ores::ore::messaging::import_run_configuration_request& v) {
    return object::write_json(v);
}

ores::ore::messaging::import_run_configuration_request
wire_traits<ores::ore::messaging::import_run_configuration_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::import_run_configuration_request>(text);
}

std::string wire_traits<ores::ore::messaging::import_run_configuration_request>::write_msgpack(
    const ores::ore::messaging::import_run_configuration_request& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::import_run_configuration_request
wire_traits<ores::ore::messaging::import_run_configuration_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::import_run_configuration_request>(data);
}

std::string wire_traits<ores::ore::messaging::import_run_configuration_response>::write_json(
    const ores::ore::messaging::import_run_configuration_response& v) {
    return object::write_json(v);
}

ores::ore::messaging::import_run_configuration_response
wire_traits<ores::ore::messaging::import_run_configuration_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::import_run_configuration_response>(text);
}

std::string wire_traits<ores::ore::messaging::import_run_configuration_response>::write_msgpack(
    const ores::ore::messaging::import_run_configuration_response& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::import_run_configuration_response
wire_traits<ores::ore::messaging::import_run_configuration_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::import_run_configuration_response>(data);
}

std::string wire_traits<ores::ore::messaging::run_configuration_import_execute_request>::write_json(
    const ores::ore::messaging::run_configuration_import_execute_request& v) {
    return object::write_json(v);
}

ores::ore::messaging::run_configuration_import_execute_request
wire_traits<ores::ore::messaging::run_configuration_import_execute_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::run_configuration_import_execute_request>(text);
}

std::string
wire_traits<ores::ore::messaging::run_configuration_import_execute_request>::write_msgpack(
    const ores::ore::messaging::run_configuration_import_execute_request& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::run_configuration_import_execute_request
wire_traits<ores::ore::messaging::run_configuration_import_execute_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::run_configuration_import_execute_request>(
        data);
}

std::string wire_traits<ores::ore::messaging::run_configuration_import_execute_result>::write_json(
    const ores::ore::messaging::run_configuration_import_execute_result& v) {
    return object::write_json(v);
}

ores::ore::messaging::run_configuration_import_execute_result
wire_traits<ores::ore::messaging::run_configuration_import_execute_result>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::run_configuration_import_execute_result>(text);
}

std::string
wire_traits<ores::ore::messaging::run_configuration_import_execute_result>::write_msgpack(
    const ores::ore::messaging::run_configuration_import_execute_result& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::run_configuration_import_execute_result
wire_traits<ores::ore::messaging::run_configuration_import_execute_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::run_configuration_import_execute_result>(
        data);
}

std::string
wire_traits<ores::ore::messaging::run_configuration_import_rollback_request>::write_json(
    const ores::ore::messaging::run_configuration_import_rollback_request& v) {
    return object::write_json(v);
}

ores::ore::messaging::run_configuration_import_rollback_request
wire_traits<ores::ore::messaging::run_configuration_import_rollback_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::run_configuration_import_rollback_request>(text);
}

std::string
wire_traits<ores::ore::messaging::run_configuration_import_rollback_request>::write_msgpack(
    const ores::ore::messaging::run_configuration_import_rollback_request& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::run_configuration_import_rollback_request
wire_traits<ores::ore::messaging::run_configuration_import_rollback_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::run_configuration_import_rollback_request>(
        data);
}

std::string wire_traits<ores::ore::messaging::export_run_configuration_request>::write_json(
    const ores::ore::messaging::export_run_configuration_request& v) {
    return object::write_json(v);
}

ores::ore::messaging::export_run_configuration_request
wire_traits<ores::ore::messaging::export_run_configuration_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::export_run_configuration_request>(text);
}

std::string wire_traits<ores::ore::messaging::export_run_configuration_request>::write_msgpack(
    const ores::ore::messaging::export_run_configuration_request& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::export_run_configuration_request
wire_traits<ores::ore::messaging::export_run_configuration_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::export_run_configuration_request>(data);
}

std::string wire_traits<ores::ore::messaging::export_run_configuration_response>::write_json(
    const ores::ore::messaging::export_run_configuration_response& v) {
    return object::write_json(v);
}

ores::ore::messaging::export_run_configuration_response
wire_traits<ores::ore::messaging::export_run_configuration_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::export_run_configuration_response>(text);
}

std::string wire_traits<ores::ore::messaging::export_run_configuration_response>::write_msgpack(
    const ores::ore::messaging::export_run_configuration_response& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::export_run_configuration_response
wire_traits<ores::ore::messaging::export_run_configuration_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::export_run_configuration_response>(data);
}

}
