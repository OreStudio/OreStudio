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
#ifndef ORES_IAM_SERVICE_SESSION_CONVERTER_HPP
#define ORES_IAM_SERVICE_SESSION_CONVERTER_HPP

#include <memory>
#include "ores.comms/service/session_data.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::iam::service {

/**
 * @brief Converts between comms::service::session_data and iam::domain::session.
 *
 * This converter exists to support the dependency inversion between ores.comms
 * and ores.iam. The protocol-level session data lives in ores.comms, while the
 * domain model and persistence layer remain in ores.iam.
 *
 * Flow:
 * 1. On login, ores.iam creates a domain::session
 * 2. Converts to session_data and stores in auth_session_service
 * 3. On logout, retrieves session_data and converts back to domain::session
 *    for persistence updates
 */
class session_converter final {
public:
    /**
     * @brief Convert from iam domain session to comms session_data.
     *
     * Used when storing a newly created session in auth_session_service.
     */
    static std::shared_ptr<comms::service::session_data>
    to_session_data(const domain::session& s);

    /**
     * @brief Convert from comms session_data to iam domain session.
     *
     * Used when retrieving session data for persistence operations.
     */
    static domain::session
    from_session_data(const comms::service::session_data& d);

    /**
     * @brief Update a domain session from session_data.
     *
     * Used when updating byte counters or end_time from the comms layer.
     */
    static void update_from_session_data(domain::session& s,
        const comms::service::session_data& d);
};

}

#endif
