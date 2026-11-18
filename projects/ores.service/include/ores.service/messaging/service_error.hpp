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
#ifndef ORES_SERVICE_MESSAGING_SERVICE_ERROR_HPP
#define ORES_SERVICE_MESSAGING_SERVICE_ERROR_HPP

#include "ores.service/export.hpp"
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

namespace ores::service::messaging {

/**
 * @brief Thrown when the server rejected the request at the protocol level.
 *
 * The rejection carries an @c X-Error reply header, for example an expired or
 * invalid token, rather than a failure to parse or transport the request. It is
 * distinct from a generic @c std::runtime_error so a caller can tell "the
 * server told us why, and repeating the same request will not help" apart from
 * a transient hiccup worth retrying.
 */
class ORES_SERVICE_EXPORT service_error : public std::runtime_error {
public:
    service_error(std::string error_code, std::string_view subject);
    ~service_error() override;

    [[nodiscard]] const std::string& error_code() const noexcept {
        return error_code_;
    }

private:
    std::string error_code_;
};

}

#endif
