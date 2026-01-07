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
    // Telemetry sink is added via add_telemetry_sink() if needed.
}

lifecycle_manager::~lifecycle_manager() {
    // Stop and flush the telemetry sink first (it's async)
    if (telemetry_sink_) {
        telemetry_sink_->stop();
        telemetry_sink_->flush();
        boost::log::core::get()->remove_sink(telemetry_sink_);
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

}
