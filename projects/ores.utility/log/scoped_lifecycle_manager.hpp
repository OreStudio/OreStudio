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
#ifndef ORES_UTILITY_LOG_SCOPED_LIFE_CYCLE_MANAGER_HPP
#define ORES_UTILITY_LOG_SCOPED_LIFE_CYCLE_MANAGER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <optional>
#include "ores.utility/log/logging_configuration.hpp"

namespace ores::utility::log {

/**
 * @brief Provides a RAII wrapper around the logging lifecycle
 * manager.
 */
class scoped_lifecycle_manager {
public:
    scoped_lifecycle_manager(const scoped_lifecycle_manager&) = delete;
    scoped_lifecycle_manager(scoped_lifecycle_manager&&) = default;
    scoped_lifecycle_manager&
    operator=(const scoped_lifecycle_manager&) = delete;

    scoped_lifecycle_manager();
    scoped_lifecycle_manager(const std::optional<logging_configuration>& ocfg);
    ~scoped_lifecycle_manager();

    /**
     * @brief Forces a initialisation / re-initialisation of logging.
     */
    void initialise(const std::optional<logging_configuration>& ocfg);

    /**
     * @brief Returns true if the logging system has been initialised
     * at least once.
     */
    bool is_initialised() const;

private:
    bool is_initialised_;
};

}

#endif
