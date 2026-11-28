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
#include "ores.scheduler.api/domain/retention_run_codec.hpp"
#include "ores.scheduler.api/messaging/retention_run_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::scheduler::messaging::retention_run_key>::write_json(
    const ores::scheduler::messaging::retention_run_key& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retention_run_key
wire_traits<ores::scheduler::messaging::retention_run_key>::read_json(std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retention_run_key>(text);
}

std::string wire_traits<ores::scheduler::messaging::retention_run_key>::write_msgpack(
    const ores::scheduler::messaging::retention_run_key& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retention_run_key
wire_traits<ores::scheduler::messaging::retention_run_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retention_run_key>(data);
}

std::string wire_traits<ores::scheduler::messaging::retention_run_lookup>::write_json(
    const ores::scheduler::messaging::retention_run_lookup& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retention_run_lookup
wire_traits<ores::scheduler::messaging::retention_run_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retention_run_lookup>(text);
}

std::string wire_traits<ores::scheduler::messaging::retention_run_lookup>::write_msgpack(
    const ores::scheduler::messaging::retention_run_lookup& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retention_run_lookup
wire_traits<ores::scheduler::messaging::retention_run_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retention_run_lookup>(data);
}

std::string wire_traits<ores::scheduler::messaging::retention_runs_filter>::write_json(
    const ores::scheduler::messaging::retention_runs_filter& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retention_runs_filter
wire_traits<ores::scheduler::messaging::retention_runs_filter>::read_json(std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retention_runs_filter>(text);
}

std::string wire_traits<ores::scheduler::messaging::retention_runs_filter>::write_msgpack(
    const ores::scheduler::messaging::retention_runs_filter& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retention_runs_filter
wire_traits<ores::scheduler::messaging::retention_runs_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retention_runs_filter>(data);
}

std::string wire_traits<ores::scheduler::messaging::retention_run_event>::write_json(
    const ores::scheduler::messaging::retention_run_event& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retention_run_event
wire_traits<ores::scheduler::messaging::retention_run_event>::read_json(std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retention_run_event>(text);
}

std::string wire_traits<ores::scheduler::messaging::retention_run_event>::write_msgpack(
    const ores::scheduler::messaging::retention_run_event& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retention_run_event
wire_traits<ores::scheduler::messaging::retention_run_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retention_run_event>(data);
}

std::string wire_traits<ores::scheduler::messaging::list_retention_runs_request>::write_json(
    const ores::scheduler::messaging::list_retention_runs_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::list_retention_runs_request
wire_traits<ores::scheduler::messaging::list_retention_runs_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::list_retention_runs_request>(text);
}

std::string wire_traits<ores::scheduler::messaging::list_retention_runs_request>::write_msgpack(
    const ores::scheduler::messaging::list_retention_runs_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::list_retention_runs_request
wire_traits<ores::scheduler::messaging::list_retention_runs_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::list_retention_runs_request>(data);
}

std::string wire_traits<ores::scheduler::messaging::list_retention_runs_response>::write_json(
    const ores::scheduler::messaging::list_retention_runs_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::list_retention_runs_response
wire_traits<ores::scheduler::messaging::list_retention_runs_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::list_retention_runs_response>(text);
}

std::string wire_traits<ores::scheduler::messaging::list_retention_runs_response>::write_msgpack(
    const ores::scheduler::messaging::list_retention_runs_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::list_retention_runs_response
wire_traits<ores::scheduler::messaging::list_retention_runs_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::list_retention_runs_response>(data);
}

std::string wire_traits<ores::scheduler::messaging::get_retention_run_request>::write_json(
    const ores::scheduler::messaging::get_retention_run_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_retention_run_request
wire_traits<ores::scheduler::messaging::get_retention_run_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_retention_run_request>(text);
}

std::string wire_traits<ores::scheduler::messaging::get_retention_run_request>::write_msgpack(
    const ores::scheduler::messaging::get_retention_run_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_retention_run_request
wire_traits<ores::scheduler::messaging::get_retention_run_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_retention_run_request>(data);
}

std::string wire_traits<ores::scheduler::messaging::get_retention_run_response>::write_json(
    const ores::scheduler::messaging::get_retention_run_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_retention_run_response
wire_traits<ores::scheduler::messaging::get_retention_run_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_retention_run_response>(text);
}

std::string wire_traits<ores::scheduler::messaging::get_retention_run_response>::write_msgpack(
    const ores::scheduler::messaging::get_retention_run_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_retention_run_response
wire_traits<ores::scheduler::messaging::get_retention_run_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_retention_run_response>(data);
}

std::string wire_traits<ores::scheduler::messaging::get_many_retention_runs_request>::write_json(
    const ores::scheduler::messaging::get_many_retention_runs_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_many_retention_runs_request
wire_traits<ores::scheduler::messaging::get_many_retention_runs_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_many_retention_runs_request>(text);
}

std::string wire_traits<ores::scheduler::messaging::get_many_retention_runs_request>::write_msgpack(
    const ores::scheduler::messaging::get_many_retention_runs_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_many_retention_runs_request
wire_traits<ores::scheduler::messaging::get_many_retention_runs_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_many_retention_runs_request>(data);
}

std::string wire_traits<ores::scheduler::messaging::get_many_retention_runs_response>::write_json(
    const ores::scheduler::messaging::get_many_retention_runs_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_many_retention_runs_response
wire_traits<ores::scheduler::messaging::get_many_retention_runs_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_many_retention_runs_response>(text);
}

std::string
wire_traits<ores::scheduler::messaging::get_many_retention_runs_response>::write_msgpack(
    const ores::scheduler::messaging::get_many_retention_runs_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_many_retention_runs_response
wire_traits<ores::scheduler::messaging::get_many_retention_runs_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_many_retention_runs_response>(data);
}

}
