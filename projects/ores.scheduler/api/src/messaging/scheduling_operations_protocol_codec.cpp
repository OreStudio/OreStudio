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
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::scheduler::messaging::get_job_instances_request>::write_json(
    const ores::scheduler::messaging::get_job_instances_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_job_instances_request
wire_traits<ores::scheduler::messaging::get_job_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_job_instances_request>(text);
}

std::string wire_traits<ores::scheduler::messaging::get_job_instances_request>::write_msgpack(
    const ores::scheduler::messaging::get_job_instances_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_job_instances_request
wire_traits<ores::scheduler::messaging::get_job_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_job_instances_request>(data);
}

std::string wire_traits<ores::scheduler::messaging::job_instance_summary>::write_json(
    const ores::scheduler::messaging::job_instance_summary& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::job_instance_summary
wire_traits<ores::scheduler::messaging::job_instance_summary>::read_json(std::string_view text) {
    return object::read_json<ores::scheduler::messaging::job_instance_summary>(text);
}

std::string wire_traits<ores::scheduler::messaging::job_instance_summary>::write_msgpack(
    const ores::scheduler::messaging::job_instance_summary& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::job_instance_summary
wire_traits<ores::scheduler::messaging::job_instance_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::job_instance_summary>(data);
}

std::string wire_traits<ores::scheduler::messaging::get_job_instances_response>::write_json(
    const ores::scheduler::messaging::get_job_instances_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_job_instances_response
wire_traits<ores::scheduler::messaging::get_job_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_job_instances_response>(text);
}

std::string wire_traits<ores::scheduler::messaging::get_job_instances_response>::write_msgpack(
    const ores::scheduler::messaging::get_job_instances_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_job_instances_response
wire_traits<ores::scheduler::messaging::get_job_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_job_instances_response>(data);
}

std::string wire_traits<ores::scheduler::messaging::get_scheduler_status_request>::write_json(
    const ores::scheduler::messaging::get_scheduler_status_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_scheduler_status_request
wire_traits<ores::scheduler::messaging::get_scheduler_status_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_scheduler_status_request>(text);
}

std::string wire_traits<ores::scheduler::messaging::get_scheduler_status_request>::write_msgpack(
    const ores::scheduler::messaging::get_scheduler_status_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_scheduler_status_request
wire_traits<ores::scheduler::messaging::get_scheduler_status_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_scheduler_status_request>(data);
}

std::string wire_traits<ores::scheduler::messaging::job_schedule_status>::write_json(
    const ores::scheduler::messaging::job_schedule_status& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::job_schedule_status
wire_traits<ores::scheduler::messaging::job_schedule_status>::read_json(std::string_view text) {
    return object::read_json<ores::scheduler::messaging::job_schedule_status>(text);
}

std::string wire_traits<ores::scheduler::messaging::job_schedule_status>::write_msgpack(
    const ores::scheduler::messaging::job_schedule_status& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::job_schedule_status
wire_traits<ores::scheduler::messaging::job_schedule_status>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::job_schedule_status>(data);
}

std::string wire_traits<ores::scheduler::messaging::get_scheduler_status_response>::write_json(
    const ores::scheduler::messaging::get_scheduler_status_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_scheduler_status_response
wire_traits<ores::scheduler::messaging::get_scheduler_status_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_scheduler_status_response>(text);
}

std::string wire_traits<ores::scheduler::messaging::get_scheduler_status_response>::write_msgpack(
    const ores::scheduler::messaging::get_scheduler_status_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_scheduler_status_response
wire_traits<ores::scheduler::messaging::get_scheduler_status_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_scheduler_status_response>(data);
}

}
