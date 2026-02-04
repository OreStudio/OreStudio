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
#include "ores.telemetry/log/lifecycle_manager.hpp"

#include <boost/make_shared.hpp>
#include <boost/log/core.hpp>

namespace ores::telemetry::log {

lifecycle_manager::lifecycle_manager(std::optional<logging_options> ocfg)
    : ores::logging::lifecycle_manager(std::move(ocfg)) {
    // Base class handles console and file sinks.
    // Telemetry and database sinks are added via their respective methods if needed.
}

lifecycle_manager::~lifecycle_manager() {
    // Stop and flush the database sink first (it's async like telemetry sink).
    // Order matters: remove from core first to stop new records, then flush
    // pending records, then stop the background thread.
    if (database_sink_) {
        boost::log::core::get()->remove_sink(database_sink_);
        database_sink_->flush();
        database_sink_->stop();
    }

    // Stop and flush the telemetry sink (it's async)
    if (telemetry_sink_) {
        boost::log::core::get()->remove_sink(telemetry_sink_);
        telemetry_sink_->flush();
        telemetry_sink_->stop();
    }
    // Base class destructor handles console and file sinks.
}

void lifecycle_manager::add_telemetry_sink(
    std::shared_ptr<domain::resource> resource,
    telemetry_sink_backend::log_record_handler handler) {

    auto backend = boost::make_shared<telemetry_sink_backend>(
        std::move(resource), std::move(handler));

    telemetry_sink_ = boost::make_shared<telemetry_sink_type>(backend);

    // The telemetry sink receives all log records (no filtering by severity)
    // Filtering can be done in the handler if needed
    boost::log::core::get()->add_sink(telemetry_sink_);
}

void lifecycle_manager::add_database_sink(
    std::shared_ptr<domain::resource> resource,
    database_log_handler handler,
    const std::string& source_type,
    const std::string& source_name) {

    auto backend = boost::make_shared<database_sink_backend>(
        std::move(resource), std::move(handler), source_type, source_name);

    database_sink_ = boost::make_shared<database_sink_type>(backend);

    // The database sink receives all log records (no filtering by severity)
    // Filtering can be done in the handler if needed
    boost::log::core::get()->add_sink(database_sink_);
}

}
