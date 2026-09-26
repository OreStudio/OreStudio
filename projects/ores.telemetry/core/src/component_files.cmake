# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(files
    "domain/resource.cpp"
    "domain/span.cpp"
    "domain/span_context.cpp"
    "domain/span_id.cpp"
    "domain/telemetry_context.cpp"
    "domain/trace_id.cpp"
    "exporting/file_log_exporter.cpp"
    "exporting/hybrid_log_exporter.cpp"
    "exporting/telemetry_configuration.cpp"
    "exporting/telemetry_options.cpp"
    "exporting/upload_position_tracker.cpp"
    "generators/span_id_generator.cpp"
    "generators/trace_id_generator.cpp"
    "log/database_sink_backend.cpp"
    "log/lifecycle_manager.cpp"
    "log/telemetry_sink_backend.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/attribute_value.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/log_record.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/nats_samples_query.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/nats_server_sample.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/nats_stream_sample.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/ores.telemetry.domain.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/resource.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/semantic_conventions.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/service_sample.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/span.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/span_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/span_id.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/span_kind.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/span_link.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/span_status.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/telemetry_batch.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/telemetry_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/telemetry_log_entry.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/telemetry_query.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/telemetry_source.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/telemetry_stats.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/domain/trace_id.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/exporting/file_log_exporter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/exporting/hybrid_log_exporter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/exporting/log_exporter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/exporting/ores.telemetry.exporting.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/exporting/telemetry_configuration.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/exporting/telemetry_options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/exporting/upload_position_tracker.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/generators/ores.telemetry.generators.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/generators/span_id_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/generators/trace_id_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/log/database_sink_backend.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/log/database_sink_utils.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/log/lifecycle_manager.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/log/log.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/log/macros.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/log/ores.telemetry.log.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/log/skip_telemetry_guard.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/log/telemetry_sink_backend.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/messaging/nats_samples_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/messaging/service_samples_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/messaging/telemetry_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.telemetry.core/ores.telemetry.hpp"
)
