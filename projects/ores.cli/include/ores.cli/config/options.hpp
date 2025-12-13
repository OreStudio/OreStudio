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
#ifndef ORES_CLI_CONFIG_HPP
#define ORES_CLI_CONFIG_HPP

#include <iosfwd>
#include <optional>
#include "ores.utility/log/logging_options.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.cli/config/import_options.hpp"
#include "ores.cli/config/export_options.hpp"
#include "ores.cli/config/delete_options.hpp"
#include "ores.cli/config/list_options.hpp"
#include "ores.cli/config/add_options.hpp"

namespace ores::cli::config {

/**
 * @brief All of the configuration options required by the command line
 * application.
 */
struct options final {
    /**
     * @brief Configuration options related to logging, if any.
     */
    std::optional<ores::utility::log::logging_options> logging;
    /**
     * @brief Configuration related to importing of data, if any.
     */
    std::optional<import_options> importing;
    /**
     * @brief Configuration related to exporting of data, if any.
     */
    std::optional<export_options> exporting;
    /**
     * @brief Configuration related to deleting entities, if any.
     */
    std::optional<delete_options> deleting;
    /**
     * @brief Configuration related to listing entities, if any.
     */
    std::optional<list_options> listing;
    /**
     * @brief Configuration related to adding entities, if any.
     */
    std::optional<add_options> adding;
    /**
     * @brief Database connection configuration.
     */
    ores::database::database_options database;
};

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
