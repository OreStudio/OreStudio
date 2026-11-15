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
#include "ores.compute.api/messaging/run_files_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::compute::messaging::run_file_archive>::write_json(
    const ores::compute::messaging::run_file_archive& v) {
    return object::write_json(v);
}

ores::compute::messaging::run_file_archive
wire_traits<ores::compute::messaging::run_file_archive>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::run_file_archive>(text);
}

std::string wire_traits<ores::compute::messaging::run_file_archive>::write_msgpack(
    const ores::compute::messaging::run_file_archive& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::run_file_archive
wire_traits<ores::compute::messaging::run_file_archive>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::run_file_archive>(data);
}

std::string wire_traits<ores::compute::messaging::list_run_files_request>::write_json(
    const ores::compute::messaging::list_run_files_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_run_files_request
wire_traits<ores::compute::messaging::list_run_files_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::list_run_files_request>(text);
}

std::string wire_traits<ores::compute::messaging::list_run_files_request>::write_msgpack(
    const ores::compute::messaging::list_run_files_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_run_files_request
wire_traits<ores::compute::messaging::list_run_files_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::list_run_files_request>(data);
}

std::string wire_traits<ores::compute::messaging::list_run_files_response>::write_json(
    const ores::compute::messaging::list_run_files_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_run_files_response
wire_traits<ores::compute::messaging::list_run_files_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::list_run_files_response>(text);
}

std::string wire_traits<ores::compute::messaging::list_run_files_response>::write_msgpack(
    const ores::compute::messaging::list_run_files_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_run_files_response
wire_traits<ores::compute::messaging::list_run_files_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::list_run_files_response>(data);
}

}
