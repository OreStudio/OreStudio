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
#ifndef ORES_ORE_SERVICE_CONFIG_OPTIONS_HPP
#define ORES_ORE_SERVICE_CONFIG_OPTIONS_HPP

#include <iosfwd>
#include <string>
#include <optional>
#include "ores.logging/logging_options.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.nats/config/nats_options.hpp"

namespace ores::ore::service::config {

/**
 * @brief All configuration options required by ores.ore.service.
 */
struct options final {
    std::optional<ores::logging::logging_options> logging;
    ores::nats::config::nats_options nats;
    ores::database::database_options database;

    /**
     * @brief Base URL of the storage HTTP API, e.g. "http://localhost:8080".
     *
     * Used by storage_transfer to download ORE import tarballs.
     */
    std::string http_base_url;

    /**
     * @brief Root directory for per-import working directories.
     *
     * Each import unpacks its tarball into work_dir/{request_id}/.
     * Default: ../var/ore-service/work (relative to the binary directory,
     * resolves to publish/var/ore-service/work/ in the build tree).
     */
    std::string work_dir = "../var/ore-service/work";
};

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
