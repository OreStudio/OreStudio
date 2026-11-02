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
#include "ores.reporting.api/domain/report_metric_codec.hpp"
#include "ores.reporting.api/messaging/report_metric_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::report_metric_key>::write_json(
    const ores::reporting::messaging::report_metric_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_metric_key
wire_traits<ores::reporting::messaging::report_metric_key>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_metric_key>(text);
}

std::string wire_traits<ores::reporting::messaging::report_metric_key>::write_msgpack(
    const ores::reporting::messaging::report_metric_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_metric_key
wire_traits<ores::reporting::messaging::report_metric_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_metric_key>(data);
}

std::string wire_traits<ores::reporting::messaging::report_metric_write>::write_json(
    const ores::reporting::messaging::report_metric_write& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_metric_write
wire_traits<ores::reporting::messaging::report_metric_write>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_metric_write>(text);
}

std::string wire_traits<ores::reporting::messaging::report_metric_write>::write_msgpack(
    const ores::reporting::messaging::report_metric_write& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_metric_write
wire_traits<ores::reporting::messaging::report_metric_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_metric_write>(data);
}

std::string wire_traits<ores::reporting::messaging::report_metric_change>::write_json(
    const ores::reporting::messaging::report_metric_change& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_metric_change
wire_traits<ores::reporting::messaging::report_metric_change>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_metric_change>(text);
}

std::string wire_traits<ores::reporting::messaging::report_metric_change>::write_msgpack(
    const ores::reporting::messaging::report_metric_change& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_metric_change
wire_traits<ores::reporting::messaging::report_metric_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_metric_change>(data);
}

std::string wire_traits<ores::reporting::messaging::report_metric_removal>::write_json(
    const ores::reporting::messaging::report_metric_removal& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_metric_removal
wire_traits<ores::reporting::messaging::report_metric_removal>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_metric_removal>(text);
}

std::string wire_traits<ores::reporting::messaging::report_metric_removal>::write_msgpack(
    const ores::reporting::messaging::report_metric_removal& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_metric_removal
wire_traits<ores::reporting::messaging::report_metric_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_metric_removal>(data);
}

std::string wire_traits<ores::reporting::messaging::report_metric_lookup>::write_json(
    const ores::reporting::messaging::report_metric_lookup& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_metric_lookup
wire_traits<ores::reporting::messaging::report_metric_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_metric_lookup>(text);
}

std::string wire_traits<ores::reporting::messaging::report_metric_lookup>::write_msgpack(
    const ores::reporting::messaging::report_metric_lookup& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_metric_lookup
wire_traits<ores::reporting::messaging::report_metric_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_metric_lookup>(data);
}

std::string wire_traits<ores::reporting::messaging::report_metrics_filter>::write_json(
    const ores::reporting::messaging::report_metrics_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_metrics_filter
wire_traits<ores::reporting::messaging::report_metrics_filter>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_metrics_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::report_metrics_filter>::write_msgpack(
    const ores::reporting::messaging::report_metrics_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_metrics_filter
wire_traits<ores::reporting::messaging::report_metrics_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_metrics_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::report_metric_event>::write_json(
    const ores::reporting::messaging::report_metric_event& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_metric_event
wire_traits<ores::reporting::messaging::report_metric_event>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_metric_event>(text);
}

std::string wire_traits<ores::reporting::messaging::report_metric_event>::write_msgpack(
    const ores::reporting::messaging::report_metric_event& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_metric_event
wire_traits<ores::reporting::messaging::report_metric_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_metric_event>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_metrics_request>::write_json(
    const ores::reporting::messaging::list_report_metrics_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_metrics_request
wire_traits<ores::reporting::messaging::list_report_metrics_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_metrics_request>(text);
}

std::string wire_traits<ores::reporting::messaging::list_report_metrics_request>::write_msgpack(
    const ores::reporting::messaging::list_report_metrics_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_metrics_request
wire_traits<ores::reporting::messaging::list_report_metrics_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_metrics_request>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_metrics_response>::write_json(
    const ores::reporting::messaging::list_report_metrics_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_metrics_response
wire_traits<ores::reporting::messaging::list_report_metrics_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_metrics_response>(text);
}

std::string wire_traits<ores::reporting::messaging::list_report_metrics_response>::write_msgpack(
    const ores::reporting::messaging::list_report_metrics_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_metrics_response
wire_traits<ores::reporting::messaging::list_report_metrics_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_metrics_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_metric_request>::write_json(
    const ores::reporting::messaging::get_report_metric_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_metric_request
wire_traits<ores::reporting::messaging::get_report_metric_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_metric_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_metric_request>::write_msgpack(
    const ores::reporting::messaging::get_report_metric_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_metric_request
wire_traits<ores::reporting::messaging::get_report_metric_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_metric_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_metric_response>::write_json(
    const ores::reporting::messaging::get_report_metric_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_metric_response
wire_traits<ores::reporting::messaging::get_report_metric_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_metric_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_metric_response>::write_msgpack(
    const ores::reporting::messaging::get_report_metric_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_metric_response
wire_traits<ores::reporting::messaging::get_report_metric_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_metric_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_many_report_metrics_request>::write_json(
    const ores::reporting::messaging::get_many_report_metrics_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_metrics_request
wire_traits<ores::reporting::messaging::get_many_report_metrics_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_metrics_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_many_report_metrics_request>::write_msgpack(
    const ores::reporting::messaging::get_many_report_metrics_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_metrics_request
wire_traits<ores::reporting::messaging::get_many_report_metrics_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_metrics_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_many_report_metrics_response>::write_json(
    const ores::reporting::messaging::get_many_report_metrics_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_metrics_response
wire_traits<ores::reporting::messaging::get_many_report_metrics_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_metrics_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_metrics_response>::write_msgpack(
    const ores::reporting::messaging::get_many_report_metrics_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_metrics_response
wire_traits<ores::reporting::messaging::get_many_report_metrics_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_metrics_response>(data);
}

std::string wire_traits<ores::reporting::messaging::put_report_metric_request>::write_json(
    const ores::reporting::messaging::put_report_metric_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_report_metric_request
wire_traits<ores::reporting::messaging::put_report_metric_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_report_metric_request>(text);
}

std::string wire_traits<ores::reporting::messaging::put_report_metric_request>::write_msgpack(
    const ores::reporting::messaging::put_report_metric_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_report_metric_request
wire_traits<ores::reporting::messaging::put_report_metric_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_report_metric_request>(data);
}

std::string wire_traits<ores::reporting::messaging::put_report_metric_response>::write_json(
    const ores::reporting::messaging::put_report_metric_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_report_metric_response
wire_traits<ores::reporting::messaging::put_report_metric_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_report_metric_response>(text);
}

std::string wire_traits<ores::reporting::messaging::put_report_metric_response>::write_msgpack(
    const ores::reporting::messaging::put_report_metric_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_report_metric_response
wire_traits<ores::reporting::messaging::put_report_metric_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_report_metric_response>(data);
}

std::string wire_traits<ores::reporting::messaging::put_many_report_metrics_request>::write_json(
    const ores::reporting::messaging::put_many_report_metrics_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_report_metrics_request
wire_traits<ores::reporting::messaging::put_many_report_metrics_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_report_metrics_request>(text);
}

std::string wire_traits<ores::reporting::messaging::put_many_report_metrics_request>::write_msgpack(
    const ores::reporting::messaging::put_many_report_metrics_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_report_metrics_request
wire_traits<ores::reporting::messaging::put_many_report_metrics_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_report_metrics_request>(data);
}

std::string wire_traits<ores::reporting::messaging::put_many_report_metrics_response>::write_json(
    const ores::reporting::messaging::put_many_report_metrics_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_report_metrics_response
wire_traits<ores::reporting::messaging::put_many_report_metrics_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_report_metrics_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_metrics_response>::write_msgpack(
    const ores::reporting::messaging::put_many_report_metrics_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_report_metrics_response
wire_traits<ores::reporting::messaging::put_many_report_metrics_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_report_metrics_response>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_report_metric_request>::write_json(
    const ores::reporting::messaging::delete_report_metric_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_report_metric_request
wire_traits<ores::reporting::messaging::delete_report_metric_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_report_metric_request>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_report_metric_request>::write_msgpack(
    const ores::reporting::messaging::delete_report_metric_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_report_metric_request
wire_traits<ores::reporting::messaging::delete_report_metric_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_report_metric_request>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_report_metric_response>::write_json(
    const ores::reporting::messaging::delete_report_metric_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_report_metric_response
wire_traits<ores::reporting::messaging::delete_report_metric_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_report_metric_response>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_report_metric_response>::write_msgpack(
    const ores::reporting::messaging::delete_report_metric_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_report_metric_response
wire_traits<ores::reporting::messaging::delete_report_metric_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_report_metric_response>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_many_report_metrics_request>::write_json(
    const ores::reporting::messaging::delete_many_report_metrics_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_report_metrics_request
wire_traits<ores::reporting::messaging::delete_many_report_metrics_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_report_metrics_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_metrics_request>::write_msgpack(
    const ores::reporting::messaging::delete_many_report_metrics_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_report_metrics_request
wire_traits<ores::reporting::messaging::delete_many_report_metrics_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_report_metrics_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_metrics_response>::write_json(
    const ores::reporting::messaging::delete_many_report_metrics_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_report_metrics_response
wire_traits<ores::reporting::messaging::delete_many_report_metrics_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_report_metrics_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_metrics_response>::write_msgpack(
    const ores::reporting::messaging::delete_many_report_metrics_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_report_metrics_response
wire_traits<ores::reporting::messaging::delete_many_report_metrics_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_report_metrics_response>(
        data);
}

}
