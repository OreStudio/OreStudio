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
 * Template: cpp_service_config_options.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ORE_SERVICE_CONFIG_OPTIONS_HPP
#define ORES_ORE_SERVICE_CONFIG_OPTIONS_HPP

#include "ores.database/domain/database_options.hpp"
#include "ores.logging/logging_options.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <iosfwd>
#include <optional>
#include <string>
#include <tuple>

namespace ores::ore::service::config {

/**
 * @brief All of the configuration options required by the service.
 */
struct options final {
    /**
     * @brief Configuration options related to logging, if any.
     */
    std::optional<ores::logging::logging_options> logging;
    /**
     * @brief Configuration related to NATS transport.
     */
    ores::nats::config::nats_options nats;
    /**
     * @brief Configuration related to database operations.
     */
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
    std::string work_dir{"../var/ore-service/work"};
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<options>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"logging", &options::logging},
        ::ores::utility::codec::field{"nats", &options::nats},
        ::ores::utility::codec::field{"database", &options::database},
        ::ores::utility::codec::field{"http_base_url", &options::http_base_url},
        ::ores::utility::codec::field{"work_dir", &options::work_dir},
    };
}

static_assert(::ores::utility::codec::names_every_member<options>(),
              "options has a member its wire shape does not name");

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
