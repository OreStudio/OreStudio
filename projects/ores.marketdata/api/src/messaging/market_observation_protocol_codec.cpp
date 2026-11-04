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
#include "ores.marketdata.api/domain/market_observation_codec.hpp"
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::market_observation_key>::write_json(
    const ores::marketdata::messaging::market_observation_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_observation_key
wire_traits<ores::marketdata::messaging::market_observation_key>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_observation_key>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_key>::write_msgpack(
    const ores::marketdata::messaging::market_observation_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_observation_key
wire_traits<ores::marketdata::messaging::market_observation_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_observation_key>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_write>::write_json(
    const ores::marketdata::messaging::market_observation_write& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_observation_write
wire_traits<ores::marketdata::messaging::market_observation_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_observation_write>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_write>::write_msgpack(
    const ores::marketdata::messaging::market_observation_write& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_observation_write
wire_traits<ores::marketdata::messaging::market_observation_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_observation_write>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_change>::write_json(
    const ores::marketdata::messaging::market_observation_change& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_observation_change
wire_traits<ores::marketdata::messaging::market_observation_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_observation_change>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_change>::write_msgpack(
    const ores::marketdata::messaging::market_observation_change& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_observation_change
wire_traits<ores::marketdata::messaging::market_observation_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_observation_change>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_removal>::write_json(
    const ores::marketdata::messaging::market_observation_removal& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_observation_removal
wire_traits<ores::marketdata::messaging::market_observation_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_observation_removal>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_removal>::write_msgpack(
    const ores::marketdata::messaging::market_observation_removal& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_observation_removal
wire_traits<ores::marketdata::messaging::market_observation_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_observation_removal>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_lookup>::write_json(
    const ores::marketdata::messaging::market_observation_lookup& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_observation_lookup
wire_traits<ores::marketdata::messaging::market_observation_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_observation_lookup>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_lookup>::write_msgpack(
    const ores::marketdata::messaging::market_observation_lookup& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_observation_lookup
wire_traits<ores::marketdata::messaging::market_observation_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_observation_lookup>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_observations_filter>::write_json(
    const ores::marketdata::messaging::market_observations_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_observations_filter
wire_traits<ores::marketdata::messaging::market_observations_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_observations_filter>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_observations_filter>::write_msgpack(
    const ores::marketdata::messaging::market_observations_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_observations_filter
wire_traits<ores::marketdata::messaging::market_observations_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_observations_filter>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_event>::write_json(
    const ores::marketdata::messaging::market_observation_event& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_observation_event
wire_traits<ores::marketdata::messaging::market_observation_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_observation_event>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_observation_event>::write_msgpack(
    const ores::marketdata::messaging::market_observation_event& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_observation_event
wire_traits<ores::marketdata::messaging::market_observation_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_observation_event>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_market_observations_request>::write_json(
    const ores::marketdata::messaging::list_market_observations_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_market_observations_request
wire_traits<ores::marketdata::messaging::list_market_observations_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_market_observations_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_market_observations_request>::write_msgpack(
    const ores::marketdata::messaging::list_market_observations_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_market_observations_request
wire_traits<ores::marketdata::messaging::list_market_observations_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_market_observations_request>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::list_market_observations_response>::write_json(
    const ores::marketdata::messaging::list_market_observations_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_market_observations_response
wire_traits<ores::marketdata::messaging::list_market_observations_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_market_observations_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_market_observations_response>::write_msgpack(
    const ores::marketdata::messaging::list_market_observations_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_market_observations_response
wire_traits<ores::marketdata::messaging::list_market_observations_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_market_observations_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::get_market_observation_request>::write_json(
    const ores::marketdata::messaging::get_market_observation_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_market_observation_request
wire_traits<ores::marketdata::messaging::get_market_observation_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_market_observation_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_market_observation_request>::write_msgpack(
    const ores::marketdata::messaging::get_market_observation_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_market_observation_request
wire_traits<ores::marketdata::messaging::get_market_observation_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_market_observation_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_market_observation_response>::write_json(
    const ores::marketdata::messaging::get_market_observation_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_market_observation_response
wire_traits<ores::marketdata::messaging::get_market_observation_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_market_observation_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_market_observation_response>::write_msgpack(
    const ores::marketdata::messaging::get_market_observation_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_market_observation_response
wire_traits<ores::marketdata::messaging::get_market_observation_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_market_observation_response>(data);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_market_observations_request>::write_json(
    const ores::marketdata::messaging::get_many_market_observations_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_market_observations_request
wire_traits<ores::marketdata::messaging::get_many_market_observations_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_market_observations_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_market_observations_request>::write_msgpack(
    const ores::marketdata::messaging::get_many_market_observations_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_market_observations_request
wire_traits<ores::marketdata::messaging::get_many_market_observations_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_market_observations_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_market_observations_response>::write_json(
    const ores::marketdata::messaging::get_many_market_observations_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_market_observations_response
wire_traits<ores::marketdata::messaging::get_many_market_observations_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_market_observations_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_market_observations_response>::write_msgpack(
    const ores::marketdata::messaging::get_many_market_observations_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_market_observations_response
wire_traits<ores::marketdata::messaging::get_many_market_observations_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_market_observations_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::put_market_observation_request>::write_json(
    const ores::marketdata::messaging::put_market_observation_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_market_observation_request
wire_traits<ores::marketdata::messaging::put_market_observation_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_market_observation_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::put_market_observation_request>::write_msgpack(
    const ores::marketdata::messaging::put_market_observation_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_market_observation_request
wire_traits<ores::marketdata::messaging::put_market_observation_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_market_observation_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_market_observation_response>::write_json(
    const ores::marketdata::messaging::put_market_observation_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_market_observation_response
wire_traits<ores::marketdata::messaging::put_market_observation_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_market_observation_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::put_market_observation_response>::write_msgpack(
    const ores::marketdata::messaging::put_market_observation_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_market_observation_response
wire_traits<ores::marketdata::messaging::put_market_observation_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_market_observation_response>(data);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_market_observations_request>::write_json(
    const ores::marketdata::messaging::put_many_market_observations_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_many_market_observations_request
wire_traits<ores::marketdata::messaging::put_many_market_observations_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_many_market_observations_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_market_observations_request>::write_msgpack(
    const ores::marketdata::messaging::put_many_market_observations_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_many_market_observations_request
wire_traits<ores::marketdata::messaging::put_many_market_observations_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_many_market_observations_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_market_observations_response>::write_json(
    const ores::marketdata::messaging::put_many_market_observations_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_many_market_observations_response
wire_traits<ores::marketdata::messaging::put_many_market_observations_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_many_market_observations_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_market_observations_response>::write_msgpack(
    const ores::marketdata::messaging::put_many_market_observations_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_many_market_observations_response
wire_traits<ores::marketdata::messaging::put_many_market_observations_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_many_market_observations_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::delete_market_observation_request>::write_json(
    const ores::marketdata::messaging::delete_market_observation_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_market_observation_request
wire_traits<ores::marketdata::messaging::delete_market_observation_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_market_observation_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_market_observation_request>::write_msgpack(
    const ores::marketdata::messaging::delete_market_observation_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_market_observation_request
wire_traits<ores::marketdata::messaging::delete_market_observation_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_market_observation_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_market_observation_response>::write_json(
    const ores::marketdata::messaging::delete_market_observation_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_market_observation_response
wire_traits<ores::marketdata::messaging::delete_market_observation_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_market_observation_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_market_observation_response>::write_msgpack(
    const ores::marketdata::messaging::delete_market_observation_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_market_observation_response
wire_traits<ores::marketdata::messaging::delete_market_observation_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_market_observation_response>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_market_observations_request>::write_json(
    const ores::marketdata::messaging::delete_many_market_observations_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_many_market_observations_request
wire_traits<ores::marketdata::messaging::delete_many_market_observations_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_many_market_observations_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_market_observations_request>::write_msgpack(
    const ores::marketdata::messaging::delete_many_market_observations_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_many_market_observations_request
wire_traits<ores::marketdata::messaging::delete_many_market_observations_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::delete_many_market_observations_request>(data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_market_observations_response>::write_json(
    const ores::marketdata::messaging::delete_many_market_observations_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_many_market_observations_response
wire_traits<ores::marketdata::messaging::delete_many_market_observations_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_many_market_observations_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_market_observations_response>::write_msgpack(
    const ores::marketdata::messaging::delete_many_market_observations_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_many_market_observations_response
wire_traits<ores::marketdata::messaging::delete_many_market_observations_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::delete_many_market_observations_response>(data);
}

std::string
wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_request>::write_json(
    const ores::marketdata::messaging::list_by_series_id_market_observations_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_by_series_id_market_observations_request
wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::marketdata::messaging::list_by_series_id_market_observations_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_request>::
    write_msgpack(
        const ores::marketdata::messaging::list_by_series_id_market_observations_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_by_series_id_market_observations_request
wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::list_by_series_id_market_observations_request>(data);
}

std::string
wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_response>::
    write_json(
        const ores::marketdata::messaging::list_by_series_id_market_observations_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_by_series_id_market_observations_response
wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::marketdata::messaging::list_by_series_id_market_observations_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_response>::
    write_msgpack(
        const ores::marketdata::messaging::list_by_series_id_market_observations_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_by_series_id_market_observations_response
wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::list_by_series_id_market_observations_response>(data);
}

}
