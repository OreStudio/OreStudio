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
#ifndef ORES_CLI_CONFIGURATION_HPP
#define ORES_CLI_CONFIGURATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <optional>
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.cli/importing_configuration.hpp"
#include "ores.cli/dumping_configuration.hpp"

namespace ores::cli {

/**
 * @brief All of the configuration required by the command line application.
 */
struct configuration final {
    /**
     * @brief Configuration related to logging, if any.
     */
    std::optional<ores::utility::log::logging_configuration> logging;
    /**
     * @brief Configuration related to importing, if any.
     */
    std::optional<importing_configuration> importing;
    /**
     * @brief Configuration related to dumping, if any.
     */
    std::optional<dumping_configuration> dumping;
};

std::ostream& operator<<(std::ostream& s, const configuration& v);

}

#endif
