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
#ifndef ORES_UTILITY_PROGRAM_OPTIONS_SHARED_DOMAIN_REGISTRY_HPP
#define ORES_UTILITY_PROGRAM_OPTIONS_SHARED_DOMAIN_REGISTRY_HPP

#include "ores.utility/export.hpp"
#include <map>
#include <set>
#include <string>

namespace ores::utility::program_options {

/**
 * @brief Registry of shared config domains that environment_mapper_factory
 * falls back to for every application.
 *
 * A "shared domain" is a config module (e.g. NATS) whose options are the
 * same for every service and are read from unprefixed ORES_<DOMAIN>_*
 * environment variables, as opposed to the per-app ORES_<APP_NAME>_*
 * variables environment_mapper_factory maps by default.
 *
 * A domain registers the exact suffixes it owns (e.g. "URL",
 * "SUBJECT_PREFIX" for ORES_NATS_URL, ORES_NATS_SUBJECT_PREFIX), not a
 * blanket prefix -- a domain's raw environment namespace can contain
 * variables that are not command-line options at all (e.g. NATS server
 * settings like ORES_NATS_PORT alongside client options like
 * ORES_NATS_URL), and matching those too would make
 * boost::program_options::parse_environment reject them as unrecognised
 * options.
 *
 * Domains are opted in explicitly by the owning config module (e.g.
 * nats_configuration::register_shared_domain()), called once by each
 * application's own parser setup before options are parsed. This registry
 * intentionally knows nothing about any specific domain -- it is a plain
 * map of registered prefixes to their allowed suffixes.
 */
class ORES_UTILITY_EXPORT shared_domain_registry {
public:
    shared_domain_registry() = delete;

    /**
     * @brief Registers a shared config domain by its environment variable
     * prefix (e.g. "NATS" for ORES_NATS_*) and the exact suffixes it owns
     * (e.g. {"URL", "WIRE_FORMAT"} for ORES_NATS_URL, ORES_NATS_WIRE_FORMAT).
     * Idempotent; re-registering the same prefix merges in any new
     * suffixes.
     */
    static void register_domain(const std::string& prefix,
                                const std::set<std::string>& allowed_suffixes);

    /**
     * @brief Returns every registered domain prefix mapped to its allowed
     * suffixes.
     */
    static const std::map<std::string, std::set<std::string>>& domains();
};

}

#endif
