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
#ifndef ORES_TESTING_NATS_OPTIONS_HELPER_HPP
#define ORES_TESTING_NATS_OPTIONS_HELPER_HPP

#include "ores.nats/config/nats_options.hpp"
#include "ores.platform/environment/environment.hpp"

namespace ores::testing {

/**
 * @brief Builds NATS connection options from the process environment.
 *
 * Reads ORES_NATS_URL, ORES_NATS_SUBJECT_PREFIX, and the ORES_NATS_TLS_*
 * trio. CMake bakes every .env variable into the ctest process
 * environment, so these are populated identically for both `compass
 * build` local runs and CI.
 */
inline ores::nats::config::nats_options make_nats_options() {
    using env = ores::platform::environment::environment;

    ores::nats::config::nats_options opts;
    opts.url = env::get_value_or_default("ORES_NATS_URL", "");
    if (opts.url.empty())
        opts.url = "nats://localhost:4222";
    opts.subject_prefix = env::get_value_or_default("ORES_NATS_SUBJECT_PREFIX", "");
    opts.tls_ca_cert = env::get_value_or_default("ORES_NATS_TLS_CA", "");
    opts.tls_client_cert = env::get_value_or_default("ORES_NATS_TLS_CERT", "");
    opts.tls_client_key = env::get_value_or_default("ORES_NATS_TLS_KEY", "");
    return opts;
}

}

#endif
