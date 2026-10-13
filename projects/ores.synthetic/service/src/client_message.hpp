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
#ifndef ORES_SYNTHETIC_SERVICE_CLIENT_MESSAGE_HPP
#define ORES_SYNTHETIC_SERVICE_CLIENT_MESSAGE_HPP

#include <exception>
#include <stdexcept>
#include <string>

namespace ores::synthetic::service {

/**
 * @brief What a client may read about a failure.
 *
 * An invalid argument is the caller's own mistake, and its message is written for the
 * caller: "The half-life must be longer than zero." Any other failure may carry the text of
 * a database or a library, so the client gets a fixed sentence and the log keeps the cause.
 */
inline std::string client_message(const std::exception& e) {
    if (dynamic_cast<const std::invalid_argument*>(&e) != nullptr)
        return e.what();
    return "The request failed. The service log says why.";
}

}

#endif
