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
#ifndef ORES_CONSOLE_APPLICATION_HPP
#define ORES_CONSOLE_APPLICATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include "ores.console/configuration.hpp"
#include "ores.ore/xml/xml_importer.hpp"

namespace ores::console {

/**
 * @brief Entry point for the dogen command line application.
 */
class application final {
public:
    application() = default;
    application(const application&) = delete;
    application(application&&) = delete;
    ~application() = default;
    application& operator=(const application&) = delete;

    /**
     * Executes the application.
     *
     * @param cfg Application configuration.
     */
    void run(const configuration& cfg) const;

private:
    ore::xml::xml_importer xml_importer_;
};

}

#endif
