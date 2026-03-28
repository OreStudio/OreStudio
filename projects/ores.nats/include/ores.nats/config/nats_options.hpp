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
#ifndef ORES_NATS_CONFIG_NATS_OPTIONS_HPP
#define ORES_NATS_CONFIG_NATS_OPTIONS_HPP

#include <string>

namespace ores::nats::config {

/**
 * @brief Configuration for a NATS connection.
 */
struct nats_options final {
    /**
     * @brief NATS server URL (e.g. "nats://localhost:4222" or
     *        "tls+tcp://localhost:4222" for TLS).
     */
    std::string url = "nats://localhost:4222";

    /**
     * @brief Subject prefix applied to every outbound subject.
     *
     * Format: "ores.{tier}.{instance}", e.g. "ores.dev.local1".
     * When set, all pub/sub/request subjects become
     * "{subject_prefix}.{relative_subject}". Leave empty to use
     * subjects as-is (e.g. in tests or scripts that manage prefixes
     * externally).
     */
    std::string subject_prefix;

    /**
     * @brief Mutual TLS settings.
     *
     * All three paths must be set together, or all left empty (plain TCP).
     * When set, the NATS connection uses TLS and presents the client
     * certificate to the broker for mutual authentication.
     *
     *   tls_ca_cert     — path to the CA certificate (ca.crt)
     *   tls_client_cert — path to this service's certificate (<service>.crt)
     *   tls_client_key  — path to this service's private key (<service>.key)
     */
    std::string tls_ca_cert;
    std::string tls_client_cert;
    std::string tls_client_key;
};

}

#endif
