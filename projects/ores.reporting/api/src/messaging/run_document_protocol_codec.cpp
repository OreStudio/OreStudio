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
#include "ores.reporting.api/domain/configuration_analytic_codec.hpp"
#include "ores.reporting.api/domain/configuration_market_binding_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_building_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_execution_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_fixings_codec.hpp"
#include "ores.reporting.api/domain/report_configuration_codec.hpp"
#include "ores.reporting.api/domain/risk_report_config_codec.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::run_parameter>::write_json(
    const ores::reporting::messaging::run_parameter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::run_parameter
wire_traits<ores::reporting::messaging::run_parameter>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::run_parameter>(text);
}

std::string wire_traits<ores::reporting::messaging::run_parameter>::write_msgpack(
    const ores::reporting::messaging::run_parameter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::run_parameter
wire_traits<ores::reporting::messaging::run_parameter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::run_parameter>(data);
}

std::string wire_traits<ores::reporting::messaging::run_analytic>::write_json(
    const ores::reporting::messaging::run_analytic& v) {
    return object::write_json(v);
}

ores::reporting::messaging::run_analytic
wire_traits<ores::reporting::messaging::run_analytic>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::run_analytic>(text);
}

std::string wire_traits<ores::reporting::messaging::run_analytic>::write_msgpack(
    const ores::reporting::messaging::run_analytic& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::run_analytic
wire_traits<ores::reporting::messaging::run_analytic>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::run_analytic>(data);
}

std::string wire_traits<ores::reporting::messaging::run_file>::write_json(
    const ores::reporting::messaging::run_file& v) {
    return object::write_json(v);
}

ores::reporting::messaging::run_file
wire_traits<ores::reporting::messaging::run_file>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::run_file>(text);
}

std::string wire_traits<ores::reporting::messaging::run_file>::write_msgpack(
    const ores::reporting::messaging::run_file& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::run_file
wire_traits<ores::reporting::messaging::run_file>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::run_file>(data);
}

std::string wire_traits<ores::reporting::messaging::run_engine_settings>::write_json(
    const ores::reporting::messaging::run_engine_settings& v) {
    return object::write_json(v);
}

ores::reporting::messaging::run_engine_settings
wire_traits<ores::reporting::messaging::run_engine_settings>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::run_engine_settings>(text);
}

std::string wire_traits<ores::reporting::messaging::run_engine_settings>::write_msgpack(
    const ores::reporting::messaging::run_engine_settings& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::run_engine_settings
wire_traits<ores::reporting::messaging::run_engine_settings>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::run_engine_settings>(data);
}

std::string wire_traits<ores::reporting::messaging::run_document>::write_json(
    const ores::reporting::messaging::run_document& v) {
    return object::write_json(v);
}

ores::reporting::messaging::run_document
wire_traits<ores::reporting::messaging::run_document>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::run_document>(text);
}

std::string wire_traits<ores::reporting::messaging::run_document>::write_msgpack(
    const ores::reporting::messaging::run_document& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::run_document
wire_traits<ores::reporting::messaging::run_document>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::run_document>(data);
}

std::string wire_traits<ores::reporting::messaging::save_run_document_request>::write_json(
    const ores::reporting::messaging::save_run_document_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::save_run_document_request
wire_traits<ores::reporting::messaging::save_run_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::save_run_document_request>(text);
}

std::string wire_traits<ores::reporting::messaging::save_run_document_request>::write_msgpack(
    const ores::reporting::messaging::save_run_document_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::save_run_document_request
wire_traits<ores::reporting::messaging::save_run_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::save_run_document_request>(data);
}

std::string wire_traits<ores::reporting::messaging::save_run_document_response>::write_json(
    const ores::reporting::messaging::save_run_document_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::save_run_document_response
wire_traits<ores::reporting::messaging::save_run_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::save_run_document_response>(text);
}

std::string wire_traits<ores::reporting::messaging::save_run_document_response>::write_msgpack(
    const ores::reporting::messaging::save_run_document_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::save_run_document_response
wire_traits<ores::reporting::messaging::save_run_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::save_run_document_response>(data);
}

std::string wire_traits<ores::reporting::messaging::bind_configuration_request>::write_json(
    const ores::reporting::messaging::bind_configuration_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::bind_configuration_request
wire_traits<ores::reporting::messaging::bind_configuration_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::bind_configuration_request>(text);
}

std::string wire_traits<ores::reporting::messaging::bind_configuration_request>::write_msgpack(
    const ores::reporting::messaging::bind_configuration_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::bind_configuration_request
wire_traits<ores::reporting::messaging::bind_configuration_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::bind_configuration_request>(data);
}

std::string wire_traits<ores::reporting::messaging::bind_configuration_response>::write_json(
    const ores::reporting::messaging::bind_configuration_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::bind_configuration_response
wire_traits<ores::reporting::messaging::bind_configuration_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::bind_configuration_response>(text);
}

std::string wire_traits<ores::reporting::messaging::bind_configuration_response>::write_msgpack(
    const ores::reporting::messaging::bind_configuration_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::bind_configuration_response
wire_traits<ores::reporting::messaging::bind_configuration_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::bind_configuration_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_run_document_request>::write_json(
    const ores::reporting::messaging::get_run_document_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_run_document_request
wire_traits<ores::reporting::messaging::get_run_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_run_document_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_run_document_request>::write_msgpack(
    const ores::reporting::messaging::get_run_document_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_run_document_request
wire_traits<ores::reporting::messaging::get_run_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_run_document_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_run_document_response>::write_json(
    const ores::reporting::messaging::get_run_document_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_run_document_response
wire_traits<ores::reporting::messaging::get_run_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_run_document_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_run_document_response>::write_msgpack(
    const ores::reporting::messaging::get_run_document_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_run_document_response
wire_traits<ores::reporting::messaging::get_run_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_run_document_response>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_run_document_request>::write_json(
    const ores::reporting::messaging::delete_run_document_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_run_document_request
wire_traits<ores::reporting::messaging::delete_run_document_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_run_document_request>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_run_document_request>::write_msgpack(
    const ores::reporting::messaging::delete_run_document_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_run_document_request
wire_traits<ores::reporting::messaging::delete_run_document_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_run_document_request>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_run_document_response>::write_json(
    const ores::reporting::messaging::delete_run_document_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_run_document_response
wire_traits<ores::reporting::messaging::delete_run_document_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_run_document_response>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_run_document_response>::write_msgpack(
    const ores::reporting::messaging::delete_run_document_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_run_document_response
wire_traits<ores::reporting::messaging::delete_run_document_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_run_document_response>(data);
}

}
