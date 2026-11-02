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
#include "ores.reporting.api/domain/report_error_event_codec.hpp"
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::report_error_event_key>::write_json(
    const ores::reporting::messaging::report_error_event_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_error_event_key
wire_traits<ores::reporting::messaging::report_error_event_key>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_error_event_key>(text);
}

std::string wire_traits<ores::reporting::messaging::report_error_event_key>::write_msgpack(
    const ores::reporting::messaging::report_error_event_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_error_event_key
wire_traits<ores::reporting::messaging::report_error_event_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_error_event_key>(data);
}

std::string wire_traits<ores::reporting::messaging::report_error_event_lookup>::write_json(
    const ores::reporting::messaging::report_error_event_lookup& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_error_event_lookup
wire_traits<ores::reporting::messaging::report_error_event_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_error_event_lookup>(text);
}

std::string wire_traits<ores::reporting::messaging::report_error_event_lookup>::write_msgpack(
    const ores::reporting::messaging::report_error_event_lookup& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_error_event_lookup
wire_traits<ores::reporting::messaging::report_error_event_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_error_event_lookup>(data);
}

std::string wire_traits<ores::reporting::messaging::report_error_events_filter>::write_json(
    const ores::reporting::messaging::report_error_events_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_error_events_filter
wire_traits<ores::reporting::messaging::report_error_events_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_error_events_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::report_error_events_filter>::write_msgpack(
    const ores::reporting::messaging::report_error_events_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_error_events_filter
wire_traits<ores::reporting::messaging::report_error_events_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_error_events_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::report_error_event_event>::write_json(
    const ores::reporting::messaging::report_error_event_event& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_error_event_event
wire_traits<ores::reporting::messaging::report_error_event_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_error_event_event>(text);
}

std::string wire_traits<ores::reporting::messaging::report_error_event_event>::write_msgpack(
    const ores::reporting::messaging::report_error_event_event& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_error_event_event
wire_traits<ores::reporting::messaging::report_error_event_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_error_event_event>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_error_events_request>::write_json(
    const ores::reporting::messaging::list_report_error_events_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_error_events_request
wire_traits<ores::reporting::messaging::list_report_error_events_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_error_events_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_report_error_events_request>::write_msgpack(
    const ores::reporting::messaging::list_report_error_events_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_error_events_request
wire_traits<ores::reporting::messaging::list_report_error_events_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_error_events_request>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_error_events_response>::write_json(
    const ores::reporting::messaging::list_report_error_events_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_error_events_response
wire_traits<ores::reporting::messaging::list_report_error_events_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_error_events_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_report_error_events_response>::write_msgpack(
    const ores::reporting::messaging::list_report_error_events_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_error_events_response
wire_traits<ores::reporting::messaging::list_report_error_events_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_error_events_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::get_report_error_event_request>::write_json(
    const ores::reporting::messaging::get_report_error_event_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_error_event_request
wire_traits<ores::reporting::messaging::get_report_error_event_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_error_event_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_error_event_request>::write_msgpack(
    const ores::reporting::messaging::get_report_error_event_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_error_event_request
wire_traits<ores::reporting::messaging::get_report_error_event_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_error_event_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_error_event_response>::write_json(
    const ores::reporting::messaging::get_report_error_event_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_error_event_response
wire_traits<ores::reporting::messaging::get_report_error_event_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_error_event_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_error_event_response>::write_msgpack(
    const ores::reporting::messaging::get_report_error_event_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_error_event_response
wire_traits<ores::reporting::messaging::get_report_error_event_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_error_event_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_error_events_request>::write_json(
    const ores::reporting::messaging::get_many_report_error_events_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_error_events_request
wire_traits<ores::reporting::messaging::get_many_report_error_events_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_error_events_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_error_events_request>::write_msgpack(
    const ores::reporting::messaging::get_many_report_error_events_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_error_events_request
wire_traits<ores::reporting::messaging::get_many_report_error_events_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_error_events_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_error_events_response>::write_json(
    const ores::reporting::messaging::get_many_report_error_events_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_error_events_response
wire_traits<ores::reporting::messaging::get_many_report_error_events_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_error_events_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_error_events_response>::write_msgpack(
    const ores::reporting::messaging::get_many_report_error_events_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_error_events_response
wire_traits<ores::reporting::messaging::get_many_report_error_events_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_error_events_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::list_by_report_instance_id_report_error_events_request>::
    write_json(
        const ores::reporting::messaging::list_by_report_instance_id_report_error_events_request&
            v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_by_report_instance_id_report_error_events_request
wire_traits<ores::reporting::messaging::list_by_report_instance_id_report_error_events_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_by_report_instance_id_report_error_events_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_by_report_instance_id_report_error_events_request>::
    write_msgpack(
        const ores::reporting::messaging::list_by_report_instance_id_report_error_events_request&
            v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_by_report_instance_id_report_error_events_request
wire_traits<ores::reporting::messaging::list_by_report_instance_id_report_error_events_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_by_report_instance_id_report_error_events_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_by_report_instance_id_report_error_events_response>::
    write_json(
        const ores::reporting::messaging::list_by_report_instance_id_report_error_events_response&
            v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_by_report_instance_id_report_error_events_response
wire_traits<ores::reporting::messaging::list_by_report_instance_id_report_error_events_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_by_report_instance_id_report_error_events_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_by_report_instance_id_report_error_events_response>::
    write_msgpack(
        const ores::reporting::messaging::list_by_report_instance_id_report_error_events_response&
            v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_by_report_instance_id_report_error_events_response
wire_traits<ores::reporting::messaging::list_by_report_instance_id_report_error_events_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_by_report_instance_id_report_error_events_response>(data);
}

std::string wire_traits<ores::reporting::messaging::report_error_count>::write_json(
    const ores::reporting::messaging::report_error_count& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_error_count
wire_traits<ores::reporting::messaging::report_error_count>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_error_count>(text);
}

std::string wire_traits<ores::reporting::messaging::report_error_count>::write_msgpack(
    const ores::reporting::messaging::report_error_count& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_error_count
wire_traits<ores::reporting::messaging::report_error_count>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_error_count>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_error_counts_request>::write_json(
    const ores::reporting::messaging::get_report_error_counts_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_error_counts_request
wire_traits<ores::reporting::messaging::get_report_error_counts_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_error_counts_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_error_counts_request>::write_msgpack(
    const ores::reporting::messaging::get_report_error_counts_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_error_counts_request
wire_traits<ores::reporting::messaging::get_report_error_counts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_error_counts_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_error_counts_response>::write_json(
    const ores::reporting::messaging::get_report_error_counts_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_error_counts_response
wire_traits<ores::reporting::messaging::get_report_error_counts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_error_counts_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_report_error_counts_response>::write_msgpack(
    const ores::reporting::messaging::get_report_error_counts_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_error_counts_response
wire_traits<ores::reporting::messaging::get_report_error_counts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_error_counts_response>(data);
}

}
