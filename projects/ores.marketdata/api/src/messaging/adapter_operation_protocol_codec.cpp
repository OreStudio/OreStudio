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
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::vendor_id>::write_json(
    const ores::marketdata::messaging::vendor_id& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::vendor_id
wire_traits<ores::marketdata::messaging::vendor_id>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::vendor_id>(text);
}

std::string wire_traits<ores::marketdata::messaging::vendor_id>::write_msgpack(
    const ores::marketdata::messaging::vendor_id& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::vendor_id
wire_traits<ores::marketdata::messaging::vendor_id>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::vendor_id>(data);
}

std::string wire_traits<ores::marketdata::messaging::rejected_id>::write_json(
    const ores::marketdata::messaging::rejected_id& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::rejected_id
wire_traits<ores::marketdata::messaging::rejected_id>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::rejected_id>(text);
}

std::string wire_traits<ores::marketdata::messaging::rejected_id>::write_msgpack(
    const ores::marketdata::messaging::rejected_id& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::rejected_id
wire_traits<ores::marketdata::messaging::rejected_id>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::rejected_id>(data);
}

std::string wire_traits<ores::marketdata::messaging::adapter_announce_request>::write_json(
    const ores::marketdata::messaging::adapter_announce_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::adapter_announce_request
wire_traits<ores::marketdata::messaging::adapter_announce_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::adapter_announce_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::adapter_announce_request>::write_msgpack(
    const ores::marketdata::messaging::adapter_announce_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::adapter_announce_request
wire_traits<ores::marketdata::messaging::adapter_announce_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::adapter_announce_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::adapter_announce_response>::write_json(
    const ores::marketdata::messaging::adapter_announce_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::adapter_announce_response
wire_traits<ores::marketdata::messaging::adapter_announce_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::adapter_announce_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::adapter_announce_response>::write_msgpack(
    const ores::marketdata::messaging::adapter_announce_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::adapter_announce_response
wire_traits<ores::marketdata::messaging::adapter_announce_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::adapter_announce_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::adapter_demand_request>::write_json(
    const ores::marketdata::messaging::adapter_demand_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::adapter_demand_request
wire_traits<ores::marketdata::messaging::adapter_demand_request>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::adapter_demand_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::adapter_demand_request>::write_msgpack(
    const ores::marketdata::messaging::adapter_demand_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::adapter_demand_request
wire_traits<ores::marketdata::messaging::adapter_demand_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::adapter_demand_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::adapter_demand_response>::write_json(
    const ores::marketdata::messaging::adapter_demand_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::adapter_demand_response
wire_traits<ores::marketdata::messaging::adapter_demand_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::adapter_demand_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::adapter_demand_response>::write_msgpack(
    const ores::marketdata::messaging::adapter_demand_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::adapter_demand_response
wire_traits<ores::marketdata::messaging::adapter_demand_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::adapter_demand_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::adapter_tick>::write_json(
    const ores::marketdata::messaging::adapter_tick& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::adapter_tick
wire_traits<ores::marketdata::messaging::adapter_tick>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::adapter_tick>(text);
}

std::string wire_traits<ores::marketdata::messaging::adapter_tick>::write_msgpack(
    const ores::marketdata::messaging::adapter_tick& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::adapter_tick
wire_traits<ores::marketdata::messaging::adapter_tick>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::adapter_tick>(data);
}

std::string wire_traits<ores::marketdata::messaging::adapter_heartbeat>::write_json(
    const ores::marketdata::messaging::adapter_heartbeat& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::adapter_heartbeat
wire_traits<ores::marketdata::messaging::adapter_heartbeat>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::adapter_heartbeat>(text);
}

std::string wire_traits<ores::marketdata::messaging::adapter_heartbeat>::write_msgpack(
    const ores::marketdata::messaging::adapter_heartbeat& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::adapter_heartbeat
wire_traits<ores::marketdata::messaging::adapter_heartbeat>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::adapter_heartbeat>(data);
}

std::string wire_traits<ores::marketdata::messaging::adapter_status>::write_json(
    const ores::marketdata::messaging::adapter_status& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::adapter_status
wire_traits<ores::marketdata::messaging::adapter_status>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::adapter_status>(text);
}

std::string wire_traits<ores::marketdata::messaging::adapter_status>::write_msgpack(
    const ores::marketdata::messaging::adapter_status& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::adapter_status
wire_traits<ores::marketdata::messaging::adapter_status>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::adapter_status>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_adapter_status_request>::write_json(
    const ores::marketdata::messaging::list_adapter_status_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_adapter_status_request
wire_traits<ores::marketdata::messaging::list_adapter_status_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_adapter_status_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_adapter_status_request>::write_msgpack(
    const ores::marketdata::messaging::list_adapter_status_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_adapter_status_request
wire_traits<ores::marketdata::messaging::list_adapter_status_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_adapter_status_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_adapter_status_response>::write_json(
    const ores::marketdata::messaging::list_adapter_status_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_adapter_status_response
wire_traits<ores::marketdata::messaging::list_adapter_status_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_adapter_status_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_adapter_status_response>::write_msgpack(
    const ores::marketdata::messaging::list_adapter_status_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_adapter_status_response
wire_traits<ores::marketdata::messaging::list_adapter_status_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_adapter_status_response>(data);
}

}
