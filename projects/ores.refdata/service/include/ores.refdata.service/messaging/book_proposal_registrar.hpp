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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_BOOK_PROPOSAL_REGISTRAR_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_BOOK_PROPOSAL_REGISTRAR_HPP

#include "ores.database/domain/context.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.refdata.service/export.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief Subscribes the operations that propose changes to books.
 *
 * They live here and not with the entity registrars because they raise
 * approval requests, and the inbox libraries cannot be linked below the
 * refdata core.
 */
class ORES_REFDATA_SERVICE_EXPORT book_proposal_registrar {
public:
    static std::vector<ores::nats::service::subscription> register_handlers(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier = std::nullopt);
};

}

#endif
