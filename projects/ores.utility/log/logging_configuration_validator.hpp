/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_UTILITY_LOG_LOGGING_CONFIGURATION_VALIDATOR_HPP
#define ORES_UTILITY_LOG_LOGGING_CONFIGURATION_VALIDATOR_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include "ores.utility/log/logging_configuration.hpp"

namespace ores::utility::log {

/**
 * @brief Checks the validity of the supplied logging configuration.
 */
class logging_configuration_validator final {
public:
    logging_configuration_validator() = delete;
    logging_configuration_validator(const logging_configuration_validator&) = delete;
    logging_configuration_validator(logging_configuration_validator&&) = delete;
    ~logging_configuration_validator() = delete;
    logging_configuration_validator&
    operator=(const logging_configuration_validator&) = delete;

    static void validate(const logging_configuration& cfg);
};

}

#endif
