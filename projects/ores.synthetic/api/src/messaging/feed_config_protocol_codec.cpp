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
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::synthetic::messaging::start_feed_request>::write_json(
    const ores::synthetic::messaging::start_feed_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::start_feed_request
wire_traits<ores::synthetic::messaging::start_feed_request>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::start_feed_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::start_feed_request>::write_msgpack(
    const ores::synthetic::messaging::start_feed_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::start_feed_request
wire_traits<ores::synthetic::messaging::start_feed_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::start_feed_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::start_feed_response>::write_json(
    const ores::synthetic::messaging::start_feed_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::start_feed_response
wire_traits<ores::synthetic::messaging::start_feed_response>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::start_feed_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::start_feed_response>::write_msgpack(
    const ores::synthetic::messaging::start_feed_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::start_feed_response
wire_traits<ores::synthetic::messaging::start_feed_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::start_feed_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::stop_feed_request>::write_json(
    const ores::synthetic::messaging::stop_feed_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stop_feed_request
wire_traits<ores::synthetic::messaging::stop_feed_request>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stop_feed_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::stop_feed_request>::write_msgpack(
    const ores::synthetic::messaging::stop_feed_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stop_feed_request
wire_traits<ores::synthetic::messaging::stop_feed_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stop_feed_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::stop_feed_response>::write_json(
    const ores::synthetic::messaging::stop_feed_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stop_feed_response
wire_traits<ores::synthetic::messaging::stop_feed_response>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stop_feed_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::stop_feed_response>::write_msgpack(
    const ores::synthetic::messaging::stop_feed_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stop_feed_response
wire_traits<ores::synthetic::messaging::stop_feed_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stop_feed_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::provider_dispersion>::write_json(
    const ores::synthetic::messaging::provider_dispersion& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::provider_dispersion
wire_traits<ores::synthetic::messaging::provider_dispersion>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::provider_dispersion>(text);
}

std::string wire_traits<ores::synthetic::messaging::provider_dispersion>::write_msgpack(
    const ores::synthetic::messaging::provider_dispersion& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::provider_dispersion
wire_traits<ores::synthetic::messaging::provider_dispersion>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::provider_dispersion>(data);
}

std::string wire_traits<ores::synthetic::messaging::variance_start>::write_json(
    const ores::synthetic::messaging::variance_start& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::variance_start
wire_traits<ores::synthetic::messaging::variance_start>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::variance_start>(text);
}

std::string wire_traits<ores::synthetic::messaging::variance_start>::write_msgpack(
    const ores::synthetic::messaging::variance_start& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::variance_start
wire_traits<ores::synthetic::messaging::variance_start>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::variance_start>(data);
}

std::string wire_traits<ores::synthetic::messaging::feed_start>::write_json(
    const ores::synthetic::messaging::feed_start& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::feed_start
wire_traits<ores::synthetic::messaging::feed_start>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::feed_start>(text);
}

std::string wire_traits<ores::synthetic::messaging::feed_start>::write_msgpack(
    const ores::synthetic::messaging::feed_start& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::feed_start
wire_traits<ores::synthetic::messaging::feed_start>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::feed_start>(data);
}

std::string wire_traits<ores::synthetic::messaging::running_feed>::write_json(
    const ores::synthetic::messaging::running_feed& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::running_feed
wire_traits<ores::synthetic::messaging::running_feed>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::running_feed>(text);
}

std::string wire_traits<ores::synthetic::messaging::running_feed>::write_msgpack(
    const ores::synthetic::messaging::running_feed& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::running_feed
wire_traits<ores::synthetic::messaging::running_feed>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::running_feed>(data);
}

std::string wire_traits<ores::synthetic::messaging::list_feeds_request>::write_json(
    const ores::synthetic::messaging::list_feeds_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_feeds_request
wire_traits<ores::synthetic::messaging::list_feeds_request>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_feeds_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::list_feeds_request>::write_msgpack(
    const ores::synthetic::messaging::list_feeds_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_feeds_request
wire_traits<ores::synthetic::messaging::list_feeds_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_feeds_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::list_feeds_response>::write_json(
    const ores::synthetic::messaging::list_feeds_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_feeds_response
wire_traits<ores::synthetic::messaging::list_feeds_response>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_feeds_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::list_feeds_response>::write_msgpack(
    const ores::synthetic::messaging::list_feeds_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_feeds_response
wire_traits<ores::synthetic::messaging::list_feeds_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_feeds_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_summary>::write_json(
    const ores::synthetic::messaging::calibration_policy_summary& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_summary
wire_traits<ores::synthetic::messaging::calibration_policy_summary>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_summary>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_summary>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_summary& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_summary
wire_traits<ores::synthetic::messaging::calibration_policy_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_summary>(data);
}

std::string wire_traits<ores::synthetic::messaging::candidate_outcome>::write_json(
    const ores::synthetic::messaging::candidate_outcome& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::candidate_outcome
wire_traits<ores::synthetic::messaging::candidate_outcome>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::candidate_outcome>(text);
}

std::string wire_traits<ores::synthetic::messaging::candidate_outcome>::write_msgpack(
    const ores::synthetic::messaging::candidate_outcome& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::candidate_outcome
wire_traits<ores::synthetic::messaging::candidate_outcome>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::candidate_outcome>(data);
}

std::string wire_traits<ores::synthetic::messaging::variance_calibration_summary>::write_json(
    const ores::synthetic::messaging::variance_calibration_summary& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::variance_calibration_summary
wire_traits<ores::synthetic::messaging::variance_calibration_summary>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::variance_calibration_summary>(text);
}

std::string wire_traits<ores::synthetic::messaging::variance_calibration_summary>::write_msgpack(
    const ores::synthetic::messaging::variance_calibration_summary& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::variance_calibration_summary
wire_traits<ores::synthetic::messaging::variance_calibration_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::variance_calibration_summary>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibrated_feed>::write_json(
    const ores::synthetic::messaging::calibrated_feed& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibrated_feed
wire_traits<ores::synthetic::messaging::calibrated_feed>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibrated_feed>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibrated_feed>::write_msgpack(
    const ores::synthetic::messaging::calibrated_feed& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibrated_feed
wire_traits<ores::synthetic::messaging::calibrated_feed>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibrated_feed>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibrate_request>::write_json(
    const ores::synthetic::messaging::calibrate_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibrate_request
wire_traits<ores::synthetic::messaging::calibrate_request>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibrate_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibrate_request>::write_msgpack(
    const ores::synthetic::messaging::calibrate_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibrate_request
wire_traits<ores::synthetic::messaging::calibrate_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibrate_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibrate_response>::write_json(
    const ores::synthetic::messaging::calibrate_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibrate_response
wire_traits<ores::synthetic::messaging::calibrate_response>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibrate_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibrate_response>::write_msgpack(
    const ores::synthetic::messaging::calibrate_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibrate_response
wire_traits<ores::synthetic::messaging::calibrate_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibrate_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::review_variance_calibration_request>::write_json(
    const ores::synthetic::messaging::review_variance_calibration_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::review_variance_calibration_request
wire_traits<ores::synthetic::messaging::review_variance_calibration_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::review_variance_calibration_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::review_variance_calibration_request>::write_msgpack(
    const ores::synthetic::messaging::review_variance_calibration_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::review_variance_calibration_request
wire_traits<ores::synthetic::messaging::review_variance_calibration_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::review_variance_calibration_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::review_variance_calibration_response>::write_json(
    const ores::synthetic::messaging::review_variance_calibration_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::review_variance_calibration_response
wire_traits<ores::synthetic::messaging::review_variance_calibration_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::review_variance_calibration_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::review_variance_calibration_response>::write_msgpack(
    const ores::synthetic::messaging::review_variance_calibration_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::review_variance_calibration_response
wire_traits<ores::synthetic::messaging::review_variance_calibration_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::review_variance_calibration_response>(
        data);
}

}
