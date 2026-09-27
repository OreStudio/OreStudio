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
#ifndef ORES_SHELL_APP_HTTP_BASE_URL_HPP
#define ORES_SHELL_APP_HTTP_BASE_URL_HPP

#include "ores.platform/environment/environment.hpp"
#include <string>

namespace ores::shell::app {

/**
 * @brief Base URL of the ORE HTTP server, which is the door storage bytes pass
 * through.
 *
 * The port is read from the environment the server was started from, so the
 * shell and the server cannot disagree about where storage lives. Stated once
 * because every byte-moving command needs it.
 */
inline std::string default_http_base_url() {
    return "http://localhost:" +
           ores::platform::environment::environment::get_value_or_default("ORES_HTTP_PORT", "20600");
}

}

#endif
