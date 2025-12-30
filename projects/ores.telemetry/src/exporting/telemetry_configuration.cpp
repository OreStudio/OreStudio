/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.telemetry/exporting/telemetry_configuration.hpp"

namespace ores::telemetry::exporting {

namespace {

const std::string telemetry_enabled_arg("telemetry-enabled");
const std::string telemetry_service_name_arg("telemetry-service-name");
const std::string telemetry_service_version_arg("telemetry-service-version");
const std::string telemetry_output_file_arg("telemetry-output-file");
const std::string telemetry_output_dir_arg("telemetry-output-directory");
const std::string telemetry_streaming_enabled_arg("telemetry-streaming-enabled");
const std::string telemetry_batch_size_arg("telemetry-batch-size");
const std::string telemetry_flush_interval_arg("telemetry-flush-interval");

}

boost::program_options::options_description
telemetry_configuration::make_options_description(
    const std::string& default_service_name,
    const std::string& default_service_version) {
    using boost::program_options::value;
    using boost::program_options::options_description;

    options_description r("Telemetry");
    r.add_options()
        ("telemetry-enabled",
            "Enable telemetry export. When enabled, all log records are "
            "exported to a JSON Lines file for log aggregation.")
        ("telemetry-service-name",
            value<std::string>()->default_value(default_service_name),
            "Name of the service producing telemetry. Used as service.name "
            "in resource attributes.")
        ("telemetry-service-version",
            value<std::string>()->default_value(default_service_version),
            "Version of the service producing telemetry. Used as "
            "service.version in resource attributes.")
        ("telemetry-output-file",
            value<std::string>()->default_value("telemetry.jsonl"),
            "Name of the telemetry output file (JSON Lines format).")
        ("telemetry-output-directory",
            value<std::string>()->default_value("log"),
            "Directory in which to place the telemetry output file.")
        ("telemetry-streaming-enabled",
            "Enable streaming of log records to the server. When enabled, "
            "log records are batched and sent to the connected server.")
        ("telemetry-batch-size",
            value<std::uint32_t>()->default_value(50),
            "Number of records to batch before sending to the server.")
        ("telemetry-flush-interval",
            value<std::uint64_t>()->default_value(5),
            "Maximum seconds to wait before flushing a partial batch.");

    return r;
}

std::optional<telemetry_options> telemetry_configuration::
read_options(const boost::program_options::variables_map& vm) {

    const bool enabled(vm.count(telemetry_enabled_arg) != 0);
    if (!enabled)
        return {};

    telemetry_options r;
    r.service_name = vm[telemetry_service_name_arg].as<std::string>();
    r.service_version = vm[telemetry_service_version_arg].as<std::string>();
    r.output_file = vm[telemetry_output_file_arg].as<std::string>();
    r.output_directory = vm[telemetry_output_dir_arg].as<std::string>();
    r.streaming_enabled = vm.count(telemetry_streaming_enabled_arg) != 0;
    r.batch_size = vm[telemetry_batch_size_arg].as<std::uint32_t>();
    r.flush_interval = std::chrono::seconds(
        vm[telemetry_flush_interval_arg].as<std::uint64_t>());

    return r;
}

}
