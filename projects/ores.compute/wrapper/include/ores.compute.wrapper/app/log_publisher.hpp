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
#ifndef ORES_COMPUTE_WRAPPER_APP_LOG_PUBLISHER_HPP
#define ORES_COMPUTE_WRAPPER_APP_LOG_PUBLISHER_HPP

#include <filesystem>
#include <string>
#include "ores.nats/service/client.hpp"

namespace ores::compute::wrapper::app {

/**
 * @brief Parses ORE's log.txt and publishes log entries to telemetry.
 *
 * Reads @c job_dir/log.txt, parses each line via ore_log_parser, converts
 * the entries to publish_log_entry_item records, and publishes them in
 * batches of up to 200 to "telemetry.v1.logs.publish". Tagged with
 * @c result_id for downstream filtering.
 */
void publish_ore_logs(ores::nats::service::client& nats,
    const std::string& result_id,
    const std::filesystem::path& job_dir);

/**
 * @brief Publishes wrapper engine.log lines to telemetry.
 *
 * Reads @c job_dir/engine.log line-by-line and publishes each non-empty
 * line as an "info" entry (engine log has no structured severity). Tagged
 * with @c result_id.
 */
void publish_engine_logs(ores::nats::service::client& nats,
    const std::string& result_id,
    const std::filesystem::path& job_dir);

} // namespace ores::compute::wrapper::app

#endif
