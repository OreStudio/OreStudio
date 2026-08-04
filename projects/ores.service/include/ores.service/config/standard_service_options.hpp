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
#ifndef ORES_SERVICE_CONFIG_STANDARD_SERVICE_OPTIONS_HPP
#define ORES_SERVICE_CONFIG_STANDARD_SERVICE_OPTIONS_HPP

#include "ores.database/domain/database_options.hpp"
#include "ores.logging/logging_options.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.service/export.hpp"
#include <boost/program_options.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::service::config {

/**
 * @brief The common sub-options every NATS domain service reads, once
 * parsed.
 */
struct standard_options final {
    std::optional<ores::logging::logging_options> logging;
    ores::nats::config::nats_options nats;
    ores::database::database_options database;
};

/**
 * @brief Builds and parses the options every NATS domain service shares
 * (common/logging/database/nats), collapsing the near-identical
 * composition-and-parsing boilerplate that used to be hand-duplicated
 * across each service's own config/parser.cpp.
 *
 * Also the single call site for
 * ores::nats::config::nats_configuration::register_shared_domain().
 */
class ORES_SERVICE_EXPORT standard_service_options final {
public:
    standard_service_options() = delete;

    /**
     * @brief Composes common_configuration + logging_configuration +
     * database_configuration + nats_configuration into one
     * options_description, merging in @p extra_options if given, and
     * registers NATS as a shared config domain.
     *
     * @param log_file Default log file name passed to
     * logging_configuration::make_options_description (e.g.
     * "ores.iam.service.log").
     * @param extra_options App-specific options to merge in, if any.
     */
    static boost::program_options::options_description make_options_description(
        const std::string& log_file,
        const boost::program_options::options_description& extra_options = {});

    /**
     * @brief Parses @p arguments against @p od via
     * boost::program_options::command_line_parser, then layers
     * environment variables on top via environment_mapper_factory's
     * per-app-then-shared-domain fallback tiers.
     *
     * @param od Options description, typically from
     * make_options_description().
     * @param arguments Command-line arguments (excluding argv[0]).
     * @param app_name Per-app environment prefix, in capitals (e.g.
     * "IAM_SERVICE"), matching environment_mapper_factory's convention.
     */
    static boost::program_options::variables_map
    parse(const boost::program_options::options_description& od,
         const std::vector<std::string>& arguments,
         const std::string& app_name);

    /**
     * @brief Reads the standard logging/nats/database sub-options from a
     * parsed variables map. Call after checking for help/version.
     */
    static standard_options read_options(const boost::program_options::variables_map& vm);

    /**
     * @brief True if the parsed variables map requests --help.
     */
    static bool wants_help(const boost::program_options::variables_map& vm);

    /**
     * @brief True if the parsed variables map requests --version.
     */
    static bool wants_version(const boost::program_options::variables_map& vm);
};

}

#endif
