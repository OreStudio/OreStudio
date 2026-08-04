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
#ifndef ORES_NATS_CONFIG_NATS_CONFIGURATION_HPP
#define ORES_NATS_CONFIG_NATS_CONFIGURATION_HPP

#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/export.hpp"
#include <boost/program_options.hpp>

namespace ores::nats::config {

/**
 * @brief Centralized manager for NATS configuration parsing.
 *
 * Provides utilities for creating command-line option descriptions and
 * reading configuration from parsed options. Follows the same pattern as
 * logging_configuration and database_configuration.
 *
 * Standard options:
 *   --nats-url          NATS server URL (default: nats://localhost:4222)
 *   --nats-wire-format  Message wire format: json or msgpack (default: msgpack)
 */
class ORES_NATS_EXPORT nats_configuration final {
public:
    nats_configuration() = delete;

    /**
     * @brief Creates the boost::program_options description for NATS CLI
     * arguments.
     *
     * @return options_description for NATS configuration.
     */
    static boost::program_options::options_description make_options_description();

    /**
     * @brief Reads NATS options from parsed variables map.
     *
     * @param vm Parsed command-line options.
     * @return nats_options populated from the variables map.
     */
    static nats_options read_options(const boost::program_options::variables_map& vm);

    /**
     * @brief Registers "NATS" as a shared config domain with
     * ores::utility::program_options::shared_domain_registry, covering
     * unprefixed ORES_NATS_URL, ORES_NATS_SUBJECT_PREFIX, and
     * ORES_NATS_WIRE_FORMAT -- so those reach every application's
     * environment_mapper_factory fallback tier. Deliberately excludes
     * ORES_NATS_TLS_CA/CERT/KEY: cert/key are genuinely per-service file
     * paths that cannot be shared, and CA is kept grouped with them rather
     * than resolved independently -- registering CA alone would make
     * nats_options.tls_ca_cert non-empty via the shared fallback while
     * cert/key stay unresolved for the 19 services with no per-app mirror,
     * and client.cpp's mTLS gate throws at connect time if CA is set but
     * cert/key are not. The whole TLS trio is deferred to the follow-on
     * task that removes the CLI-arg threading these currently still rely
     * on, once per-service cert/key env wiring is designed alongside it.
     * Also deliberately excludes every other ORES_NATS_* variable that is
     * not a nats_configuration option (e.g. ORES_NATS_PORT, the server's
     * own listen port -- matching it here would make parse_environment
     * reject it as an unrecognised option for client applications).
     * Idempotent; call once from each application's own config parser
     * setup, before parsing options.
     */
    static void register_shared_domain();
};

}

#endif
